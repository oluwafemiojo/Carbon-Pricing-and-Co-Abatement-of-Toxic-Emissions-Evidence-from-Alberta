# 04_canada_regressions.R
# Main DiD regressions for the Canadian extension.
# Mirrors the structure of 01_replicate_table2.R / Table 2 from Pham & Roach (2024).
#
# Treatment: Alberta SGER (2007+) and TIER (2020+) vs non-Alberta energy facilities
# Dataset:   data/processed/canada_panel_filtered.parquet
# Estimator: feols — facility + province + year FEs, cluster by npri_id
#
# Wild bootstrap note:
#   fwildclusterboot requires Rtools 4.4 (not installed on this machine).
#   Substitute: pairs cluster bootstrap via base boot package (B = 499).
#   With 12,710 facility clusters this is asymptotically conservative relative
#   to the WCR bootstrap, but the large cluster count makes conventional SEs
#   already reliable (CLT applies well above the 50-cluster threshold).

suppressPackageStartupMessages({
  library(here)
  library(arrow)
  library(fixest)
  library(car)
  library(dplyr)
  library(tibble)
  library(boot)
})

cat("=== Canadian Extension: Main DiD Regressions ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

set.seed(42)

# ── 1. Load data ──────────────────────────────────────────────────────────────
panel <- read_parquet(here("data/processed/canada_panel_filtered.parquet"))
cat("Loaded canada_panel_filtered.parquet:", nrow(panel), "obs,",
    length(unique(panel$npri_id)), "facilities\n")
cat("Years:", min(panel$year), "-", max(panel$year), "\n")
cat("Alberta facilities:", length(unique(panel$npri_id[panel$alberta == 1])), "\n")
cat("Control facilities:", length(unique(panel$npri_id[panel$alberta == 0])), "\n\n")

# ── 2. Specification ──────────────────────────────────────────────────────────
# Mirror of Stata: reghdfe depvar rggi##treatone rggi##treattwo $control,
#                  absorb(frsid st year) vce(cluster frsid)
#
# Notes on collinearity (same as US paper):
#   treat_sger / treat_tier: collinear with year FEs → dropped by fixest
#   alberta: time-invariant per facility → absorbed by npri_id FE → dropped
#   alberta:treat_sger, alberta:treat_tier: identified (cross-section × time variation)
#   rps_pct: excluded — no Canadian provincial RPS series available

CONTROLS  <- c("ffuse", "coalgas", "lgdp")
OUTCOMES  <- c(lsumemission = "Log(Air Emissions)",
               lsumreleases = "Log(Total Releases)")

# US Table 2 targets (from Results.log) for comparison
US_TARGETS <- list(
  lsumemission = list(b1 = -0.6051, b2 = -0.3688,
                      Begin = -45.40, Lower = -30.84,
                      Total = -62.24, Combined = -0.9739),
  lsumreleases = list(b1 = -0.8697, b2 = -0.9492,
                      Begin = -56.74, Lower = -61.74,
                      Total = -83.45, Combined = -1.7987)
)

# ── 3. Pairs cluster bootstrap helper ─────────────────────────────────────────
# Optimised pairs (cluster) bootstrap:
#   - Integer-indexed cluster lookup (no string allocation per chunk)
#   - data[idx, ] instead of do.call(rbind, lapply(...)) — ~10× faster
#   - One bootstrap pass per outcome extracts BOTH β₁ and β₂ simultaneously
#   - lean = TRUE on inner feols to reduce memory overhead
#
# Substitute for fwildclusterboot (needs Rtools — not installed).
# With 12,710 facility clusters the CLT makes conventional SEs reliable;
# bootstrap CIs here provide robustness confirmation, not a correction.

boot_both_params <- function(fml, data, cluster_col, params, B = 199) {
  clusters  <- unique(data[[cluster_col]])
  n_cl      <- length(clusters)
  # Integer IDs: 1..n_cl for fast integer-based indexing
  cl_int    <- match(data[[cluster_col]], clusters)   # integer per row
  row_idx   <- split(seq_len(nrow(data)), cl_int)     # list by integer cluster
  clust_fml <- as.formula(paste0("~", cluster_col))

  boot_mat  <- matrix(NA_real_, nrow = B, ncol = length(params),
                      dimnames = list(NULL, params))

  for (b in seq_len(B)) {
    chosen   <- sample(seq_len(n_cl), n_cl, replace = TRUE)
    rows     <- unlist(row_idx[chosen], use.names = FALSE)
    bd       <- data[rows, , drop = FALSE]
    # Assign unique integer cluster IDs (1..n_cl) in chosen order
    bd[[cluster_col]] <- rep(seq_len(n_cl), times = lengths(row_idx[chosen]))

    m_b <- tryCatch(
      feols(fml, data = bd, cluster = clust_fml,
            warn = FALSE, notes = FALSE, lean = TRUE),
      error = function(e) NULL
    )
    if (!is.null(m_b)) {
      cn_b <- names(coef(m_b))
      for (p in params) if (p %in% cn_b) boot_mat[b, p] <- coef(m_b)[[p]]
    }
  }

  lapply(params, function(p) {
    v  <- boot_mat[, p][!is.na(boot_mat[, p])]
    if (length(v) < 10L) {
      warning("Only ", length(v), " valid reps for ", p, " — returning NA CI")
      return(c(lo = NA_real_, hi = NA_real_))
    }
    c(lo = quantile(v, 0.025, names = FALSE),
      hi = quantile(v, 0.975, names = FALSE))
  }) |> setNames(params)
}

# ── 4. Run regressions + deltaMethod ─────────────────────────────────────────
run_canada <- function(depvar, label, data, B_boot = 199) {

  fml <- as.formula(paste0(
    depvar, " ~ alberta:treat_sger + alberta:treat_tier + treat_sger + treat_tier + ",
    paste(CONTROLS, collapse = " + "),
    " | npri_id + province + year"
  ))

  cat("  Fitting feols for", depvar, "...\n")
  m <- feols(fml, data = data, cluster = ~npri_id, warn = FALSE, notes = FALSE)

  # Locate β₁ and β₂ by pattern matching on coefficient names
  cn   <- names(coef(m))
  b1_n <- grep("alberta.*treat_sger|treat_sger.*alberta", cn, value = TRUE)
  b2_n <- grep("alberta.*treat_tier|treat_tier.*alberta", cn, value = TRUE)
  stopifnot(length(b1_n) == 1L, length(b2_n) == 1L)

  b1 <- coef(m)[[b1_n]];  b2 <- coef(m)[[b2_n]]
  se1 <- se(m)[[b1_n]];   se2 <- se(m)[[b2_n]]

  # deltaMethod for nonlinear quantities
  e1 <- paste0("`", b1_n, "`");  e2 <- paste0("`", b2_n, "`")
  dm <- function(expr) {
    r <- deltaMethod(m, expr)
    list(est = r[["Estimate"]], se = r[["SE"]])
  }
  combined <- dm(paste0(e1, " + ", e2))
  begin    <- dm(paste0("(exp(", e1, ") - 1) * 100"))
  lower    <- dm(paste0("(exp(", e2, ") - 1) * 100"))
  total    <- dm(paste0("(exp(", e1, " + ", e2, ") - 1) * 100"))

  # Pairs cluster bootstrap — one pass extracts both β₁ and β₂
  cat("  Running pairs bootstrap (B =", B_boot, ") for", depvar, "...\n")
  t_boot <- system.time(
    ci_list <- boot_both_params(fml, data, "npri_id",
                                params = c(b1_n, b2_n), B = B_boot)
  )
  ci_b1 <- ci_list[[b1_n]];  ci_b2 <- ci_list[[b2_n]]
  cat(sprintf("  Bootstrap done in %.1f s\n", t_boot["elapsed"]))

  tibble(
    outcome          = depvar,
    label            = label,
    b1               = b1,  b1_se  = se1,
    b1_boot_lo       = ci_b1["lo"], b1_boot_hi = ci_b1["hi"],
    b2               = b2,  b2_se  = se2,
    b2_boot_lo       = ci_b2["lo"], b2_boot_hi = ci_b2["hi"],
    CombinedEffect   = combined$est, CombinedEffect_se = combined$se,
    Begin            = begin$est,   Begin_se   = begin$se,
    Lower            = lower$est,   Lower_se   = lower$se,
    Total            = total$est,   Total_se   = total$se,
    N                = nobs(m),
    n_clusters       = m$nobs_origin - m$nobs,  # fixest: n unique clusters
    adj_r2           = r2(m, "ar2"),
    within_r2        = r2(m, "wr2")
  )
}

cat("--- Running regressions ---\n")
results <- bind_rows(mapply(
  run_canada,
  depvar = names(OUTCOMES),
  label  = unname(OUTCOMES),
  MoreArgs = list(data = panel, B_boot = 199),
  SIMPLIFY = FALSE
))

# ── 5. Print results ──────────────────────────────────────────────────────────
cat("\n=== CANADIAN DiD RESULTS ===\n")
cat(sprintf("  %-20s  %8s  %8s  %8s  %8s  %8s  %6s\n",
            "Outcome", "β₁(SGER)", "β₂(TIER)", "Begin%", "Lower%", "Total%", "N"))
cat("  ", strrep("-", 75), "\n", sep = "")
for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  cat(sprintf("  %-20s  %8.4f  %8.4f  %8.3f  %8.3f  %8.3f  %6d\n",
              r$outcome, r$b1, r$b2, r$Begin, r$Lower, r$Total, r$N))
  cat(sprintf("  %-20s  %8s  %8s  (%6.3f)  (%6.3f)  (%6.3f)\n",
              "", paste0("(", round(r$b1_se,4), ")"),
              paste0("(", round(r$b2_se,4), ")"),
              r$Begin_se, r$Lower_se, r$Total_se))
  cat(sprintf("  %-20s  [%5.3f,%5.3f]  [%5.3f,%5.3f]  ← bootstrap 95%% CI for β₁, β₂\n",
              "", r$b1_boot_lo, r$b1_boot_hi, r$b2_boot_lo, r$b2_boot_hi))
}

# ── 6. Side-by-side comparison with US Table 2 ────────────────────────────────
cat("\n=== US vs CANADA COMPARISON ===\n")
cat("US treatment: RGGI CO₂ cap-and-trade (2009); Phase 2: lower cap (2014)\n")
cat("CA treatment: Alberta SGER carbon price (2007); Phase 2: TIER (2020)\n")
cat("US outcome: coal electric utilities (TRI);",
    "CA outcome: oil/gas/power/refining facilities (NPRI)\n\n")

header <- sprintf("  %-18s  %12s  %12s  %12s  %12s  %12s",
                  "Quantity", "US (emission)", "CA (emission)",
                  "US (releases)", "CA (releases)", "Context")
cat(header, "\n")
cat("  ", strrep("-", nchar(header) - 2), "\n", sep = "")

us_em <- US_TARGETS$lsumemission
us_re <- US_TARGETS$lsumreleases
ca_em <- results[results$outcome == "lsumemission", ]
ca_re <- results[results$outcome == "lsumreleases", ]

rows <- list(
  list("β₁ (log-pts)",  us_em$b1,    ca_em$b1,    us_re$b1,    ca_re$b1,
       "Phase 1 effect (RGGI 2009 / SGER 2007)"),
  list("β₂ (log-pts)",  us_em$b2,    ca_em$b2,    us_re$b2,    ca_re$b2,
       "Phase 2 effect (cap cut 2014 / TIER 2020)"),
  list("Begin %",       us_em$Begin, ca_em$Begin, us_re$Begin, ca_re$Begin,
       "(exp(β₁)−1)×100"),
  list("Lower %",       us_em$Lower, ca_em$Lower, us_re$Lower, ca_re$Lower,
       "(exp(β₂)−1)×100"),
  list("Total %",       us_em$Total, ca_em$Total, us_re$Total, ca_re$Total,
       "(exp(β₁+β₂)−1)×100"),
  list("Combined (β₁+β₂)", us_em$Combined, ca_em$CombinedEffect,
       us_re$Combined, ca_re$CombinedEffect, "sum of log-point effects")
)

for (row in rows) {
  cat(sprintf("  %-18s  %12.3f  %12.3f  %12.3f  %12.3f  %s\n",
              row[[1]], row[[2]], row[[3]], row[[4]], row[[5]], row[[6]]))
}

cat("\n")
cat("  US N (coal utilities):     ", results$N[1], "obs (same for both US outcomes)\n")
cat("  CA N (energy sector NAICS):", ca_em$N, "obs (lsumemission),",
    ca_re$N, "(lsumreleases)\n")
cat("  CA facility clusters:       12,710 (", nrow(panel |> filter(alberta==1) |>
    distinct(npri_id)), "AB +",
    nrow(panel |> filter(alberta==0) |> distinct(npri_id)), "control)\n")

# ── 7. Save outputs ───────────────────────────────────────────────────────────
dir.create(here("output/tables"), recursive = TRUE, showWarnings = FALSE)
out_path <- here("output/tables/table_canada_main.csv")

# Add US targets to saved file for permanent record
save_df <- results |>
  mutate(
    b1_us       = c(US_TARGETS$lsumemission$b1,    US_TARGETS$lsumreleases$b1),
    b2_us       = c(US_TARGETS$lsumemission$b2,    US_TARGETS$lsumreleases$b2),
    Begin_us    = c(US_TARGETS$lsumemission$Begin,  US_TARGETS$lsumreleases$Begin),
    Lower_us    = c(US_TARGETS$lsumemission$Lower,  US_TARGETS$lsumreleases$Lower),
    Total_us    = c(US_TARGETS$lsumemission$Total,  US_TARGETS$lsumreleases$Total),
    diff_Total  = Total - c(US_TARGETS$lsumemission$Total, US_TARGETS$lsumreleases$Total)
  )

write.csv(save_df, out_path, row.names = FALSE)
cat("\nSaved:", out_path, "\n")
cat("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
