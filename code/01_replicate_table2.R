# 01_replicate_table2.R
# Replicates Table 2 from Pham & Roach (2024)
# "Spillover Benefits of Carbon Dioxide Cap and Trade: Evidence from the TRI"
# Economic Inquiry, Vol. 62(1), 2024. DOI: 10.1111/ecin.13162
#
# Stata original: EI Replication files.do, lines 77-83
# Dataset:        EI Data Coal.dta (coal electric utilities, 2000-2019)
# Estimator:      OLS with facility + state + year FEs (fixest::feols)
# Clustering:     facility (frsid)

suppressPackageStartupMessages({
  library(here)
  library(haven)
  library(fixest)
  library(car)
  library(dplyr)
  library(tibble)
})

cat("=== Table 2 Replication: Pham & Roach (2024) ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ── 1. Load data ──────────────────────────────────────────────────────────────
coal <- read_dta(here("data/raw/pham_roach_pkg/191201-V2/EI Data Coal.dta"))
cat("Loaded EI Data Coal.dta:", nrow(coal), "obs,", ncol(coal), "vars\n")

# ── 2. Sample restrictions ────────────────────────────────────────────────────
# Table 2 uses the full EI Data Coal.dta with no additional restrictions.
# (Table 6 / Appendix D restrict to noneic==1 and noneic==1 & st!="CA" —
#  those are separate scripts.)
cat("Sample: full EI Data Coal.dta (no additional restrictions)\n")
cat("  Total obs          :", nrow(coal), "\n")
cat("  Unique facilities  :", length(unique(coal$frsid)), "\n")
cat("  RGGI facilities    :", length(unique(coal$frsid[coal$rggi == 1])), "\n")
cat("  Non-RGGI facilities:", length(unique(coal$frsid[coal$rggi == 0])), "\n")
cat("  States             :", length(unique(coal$st)), "\n")
cat("  Year range         :", min(coal$year), "-", max(coal$year), "\n\n")

# ── 3. Specification globals (mirror Stata do-file lines 12-15) ───────────────
# Stata: gl absorbvar = "frsid st year"
#        gl clustervar = "frsid"
#        gl control    = "ffuse rps_pct coalgas lgdp"
#
# Notes on omitted terms:
#   treatone / treattwo are collinear with year FEs (every facility in a given
#   year has the same value) — Stata warns "omitted because of collinearity".
#   fixest drops them silently. We exclude them from the formula for clarity.
#
#   rggi is time-invariant at the facility level. In Stata's reghdfe it is NOT
#   dropped (it gets a small, insignificant coefficient because multi-way FE
#   demeaning leaves residual between-state variation). fixest absorbs it into
#   the facility FE. The DiD interaction coefficients are unaffected either way.

controls <- c("ffuse", "rps_pct", "coalgas", "lgdp")

# Table 2 outcome order (columns 1-6 in the paper)
outcomes <- c(
  "lsumemission",
  "lsumemission_metal1",
  "lsumemission_metal0",
  "lsumreleases",
  "lsumreleases_metal1",
  "lsumreleases_metal0"
)

# ── 4. Paper targets from Results.log (for comparison) ───────────────────────
# Extracted from Results.log lines 1031-1344 (Table 2 nlcom output).
# Column order matches the foreach loop in the do-file.
paper <- tribble(
  ~outcome,               ~Begin_paper, ~Lower_paper, ~Total_paper, ~CombinedEffect_paper,
  "lsumemission",          -45.400,      -30.841,      -62.239,      -0.9739,
  "lsumemission_metal1",   -35.139,      -61.027,      -74.722,      -1.3752,
  "lsumemission_metal0",   -44.739,      -30.638,      -61.670,      -0.9589,
  "lsumreleases",          -56.742,      -61.738,      -83.449,      -1.7987,
  "lsumreleases_metal1",   -36.145,      -65.038,      -77.675,      -1.4995,
  "lsumreleases_metal0",   -53.658,      -60.040,      -81.482,      -1.6864
)

# ── 5. Run regressions + deltaMethod ─────────────────────────────────────────
run_spec <- function(depvar, data) {

  fml <- as.formula(paste0(
    depvar, " ~ rggi:treatone + rggi:treattwo + ",
    paste(controls, collapse = " + "),
    " | frsid + st + year"
  ))

  m <- feols(fml, data = data, cluster = ~frsid, warn = FALSE, notes = FALSE)

  # Coefficient names fixest assigns to the interaction terms.
  # For numeric x numeric, feols uses "a:b" matching formula order → "rggi:treatone".
  cn <- names(coef(m))
  b1_name <- grep("rggi.*treatone|treatone.*rggi", cn, value = TRUE)
  b2_name <- grep("rggi.*treattwo|treattwo.*rggi", cn, value = TRUE)

  if (length(b1_name) != 1 || length(b2_name) != 1) {
    stop("Could not uniquely identify β1/β2 coefficient names. Found: ",
         paste(cn, collapse = ", "))
  }

  b1 <- coef(m)[[b1_name]]
  b2 <- coef(m)[[b2_name]]

  # deltaMethod expressions use backtick-quoted names for coefficients
  # containing special characters (`:` in this case).
  e1 <- paste0("`", b1_name, "`")
  e2 <- paste0("`", b2_name, "`")

  dm <- function(expr) {
    res <- deltaMethod(m, expr)
    list(est = res[["Estimate"]], se = res[["SE"]])
  }

  combined <- dm(paste0(e1, " + ", e2))
  begin    <- dm(paste0("(exp(", e1, ") - 1) * 100"))
  lower    <- dm(paste0("(exp(", e2, ") - 1) * 100"))
  total    <- dm(paste0("(exp(", e1, " + ", e2, ") - 1) * 100"))

  tibble(
    outcome            = depvar,
    b1                 = b1,
    b1_se              = se(m)[[b1_name]],
    b2                 = b2,
    b2_se              = se(m)[[b2_name]],
    CombinedEffect     = combined$est,
    CombinedEffect_se  = combined$se,
    Begin              = begin$est,
    Begin_se           = begin$se,
    Lower              = lower$est,
    Lower_se           = lower$se,
    Total              = total$est,
    Total_se           = total$se,
    N                  = nobs(m),
    adj_r2             = r2(m, "ar2"),
    within_r2          = r2(m, "wr2")
  )
}

cat("Running 6 regressions...\n")
results <- bind_rows(lapply(outcomes, run_spec, data = coal))
cat("Done.\n\n")

# ── 6. Print replication results ──────────────────────────────────────────────
cat("=== REPLICATED COEFFICIENTS ===\n")
cat(sprintf("  %-26s  %8s  %8s  %8s  %8s  %8s  %6s\n",
            "Outcome", "β1", "β2", "Begin%", "Lower%", "Total%", "N"))
cat("  ", strrep("-", 80), "\n", sep = "")
for (i in seq_len(nrow(results))) {
  r <- results[i, ]
  cat(sprintf("  %-26s  %8.4f  %8.4f  %8.3f  %8.3f  %8.3f  %6d\n",
              r$outcome, r$b1, r$b2, r$Begin, r$Lower, r$Total, r$N))
}

# ── 7. Comparison vs paper ────────────────────────────────────────────────────
cat("\n=== COMPARISON VS PAPER (Results.log) ===\n")

comparison <- results |>
  select(outcome, Begin, Lower, Total, CombinedEffect) |>
  left_join(paper, by = "outcome") |>
  mutate(
    diff_Begin   = Begin   - Begin_paper,
    diff_Lower   = Lower   - Lower_paper,
    diff_Total   = Total   - Total_paper
  )

cat(sprintf("  %-26s  %10s  %10s  %10s\n",
            "Outcome", "Δ Begin%", "Δ Lower%", "Δ Total%"))
cat("  ", strrep("-", 62), "\n", sep = "")
for (i in seq_len(nrow(comparison))) {
  r <- comparison[i, ]
  cat(sprintf("  %-26s  %+10.4f  %+10.4f  %+10.4f\n",
              r$outcome, r$diff_Begin, r$diff_Lower, r$diff_Total))
}
cat("\n  (Values near 0 indicate successful replication.)\n")

# ── 8. Save results ───────────────────────────────────────────────────────────
dir.create(here("output/tables"), recursive = TRUE, showWarnings = FALSE)
out_path <- here("output/tables/table2_replication.csv")

results |>
  left_join(paper, by = "outcome") |>
  write.csv(out_path, row.names = FALSE)

cat("\nSaved:", out_path, "\n")
cat("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
