# 05_canada_event_study.R
# Canadian extension: event study figure for lsumemission
# Mirrors 02_event_study.R but uses the Alberta SGER/TIER treatment structure.
#
# Reference year: 2006 (last pre-SGER year; no albertaXy2006 dummy)
# Treatment events: SGER 2007, TIER 2020 — two vertical reference lines
# Outcome: lsumemission (log air emissions)
#
# Pre-trend data note:
#   Alberta has only 84-117 facility-year observations in 2000-2002 versus
#   2,600+ from 2003 onward. Estimates for 2000-2002 carry very wide CIs
#   and should be interpreted with caution. The meaningful pre-trend window
#   for assessing parallel trends is 2003-2005.

suppressPackageStartupMessages({
  library(here)
  library(arrow)
  library(fixest)
  library(car)
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(stringr)
})

cat("=== Canadian Event Study: lsumemission ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ── 1. Load data ──────────────────────────────────────────────────────────────
panel <- read_parquet(here("data/processed/canada_panel_filtered.parquet"))
cat("Loaded:", nrow(panel), "obs,", length(unique(panel$npri_id)), "facilities\n\n")

# ── 2. Event study specification ──────────────────────────────────────────────
CONTROLS  <- c("ffuse", "coalgas", "lgdp")
REF_YEAR  <- 2006L
ALL_YEARS <- 2000L:2022L
EVENT_YEARS <- setdiff(ALL_YEARS, REF_YEAR)   # 22 dummies
EVENT_VARS  <- paste0("albertaXy", EVENT_YEARS)

# Verify all dummies are in the panel
missing <- setdiff(EVENT_VARS, names(panel))
if (length(missing) > 0) stop("Missing event dummies: ", paste(missing, collapse=", "))
cat("Event dummies: ", length(EVENT_VARS),
    " (", min(EVENT_YEARS), "-", max(EVENT_YEARS), ", 2006 omitted)\n\n")

# Alberta obs per year — needed to flag thin pre-period data
ab_obs_by_year <- panel |>
  filter(alberta == 1) |>
  group_by(year) |>
  summarise(n = n(), .groups = "drop")
cat("Alberta obs by year (pre-treatment period):\n")
print(as.data.frame(ab_obs_by_year |> filter(year <= 2006)))
THIN_YEARS <- ab_obs_by_year$year[ab_obs_by_year$n < 500]
cat("\nYears with < 500 AB obs (thin data):", paste(THIN_YEARS, collapse=", "), "\n\n")

# ── 3. Estimate event study regression ───────────────────────────────────────
fml <- as.formula(paste0(
  "lsumemission ~ ", paste(EVENT_VARS, collapse = " + "),
  " + ", paste(CONTROLS, collapse = " + "),
  " | npri_id + province + year"
))

cat("Fitting event study feols...\n")
m <- feols(fml, data = panel, cluster = ~npri_id, warn = FALSE, notes = FALSE)
cat("N =", nobs(m), " | Adj R² =", round(r2(m, "ar2"), 4),
    " | Within R² =", round(r2(m, "wr2"), 4), "\n\n")

# ── 4. Extract coefficients and 95% CIs ──────────────────────────────────────
b    <- coef(m)[EVENT_VARS]
ci   <- confint(m)[EVENT_VARS, ]

event_data <- tibble(
  year     = EVENT_YEARS,
  estimate = as.numeric(b),
  ci_lo    = as.numeric(ci[, 1]),
  ci_hi    = as.numeric(ci[, 2]),
  is_thin  = year %in% THIN_YEARS
) |>
  bind_rows(
    tibble(year = REF_YEAR, estimate = NA_real_,   # reference year: NA gap
           ci_lo = NA_real_, ci_hi = NA_real_, is_thin = FALSE)
  ) |>
  arrange(year)

cat("Event study estimates:\n")
print(as.data.frame(event_data |>
  select(year, estimate, ci_lo, ci_hi, is_thin) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))))

# ── 5. Pre-trend tests ────────────────────────────────────────────────────────
cat("\n=== PRE-TREND TESTS ===\n")

# Full pre-period: 2000-2005 (all 6 pre-treatment years)
pre_vars_all  <- paste0("albertaXy", 2000:2005)
# Substantive pre-period: 2003-2005 (years with adequate sample)
pre_vars_core <- paste0("albertaXy", 2003:2005)

run_pretest <- function(vars, label) {
  # fixest::wald() is the native Wald test for fixest models.
  # It returns: stat (chi-sq or F), p, df1, df2
  # Pattern: match variable names exactly via regex anchored with ^...$
  pat <- paste0("^(", paste(vars, collapse = "|"), ")$")
  tryCatch({
    wt  <- fixest::wald(m, keep = pat)
    # wald() returns a list; extract stat, p, df
    stat_val <- wt[["stat"]]
    p_val    <- wt[["p"]]
    df_val   <- wt[["df1"]]
    cat(sprintf("  %-40s stat(%d) = %6.3f, p = %.4f  %s\n",
                label, df_val, stat_val, p_val,
                ifelse(p_val < 0.05, "*** REJECT H0 (pre-trends differ)",
                       ifelse(p_val < 0.10, "* Marginal (p<0.10)",
                              "Fail to reject (parallel trends consistent)"))))
    list(stat = stat_val, df = df_val, p = p_val)
  }, error = function(e) {
    cat("  ", label, ": test failed —", conditionMessage(e), "\n")
    list(stat = NA_real_, df = NA_integer_, p = NA_real_)
  })
}

pt_all  <- run_pretest(pre_vars_all,  "Joint F-test: 2000-2005 (all pre-years)")
pt_core <- run_pretest(pre_vars_core, "Joint F-test: 2003-2005 (adequate N)")

# Individual pre-trend estimates
cat("\n  Individual pre-treatment coefficients (should be near zero):\n")
for (v in pre_vars_all) {
  yr   <- as.integer(str_extract(v, "[0-9]+"))
  est  <- round(coef(m)[[v]], 4)
  se_v <- round(se(m)[[v]], 4)
  n_ab <- ab_obs_by_year$n[ab_obs_by_year$year == yr]
  thin_flag <- if (yr %in% THIN_YEARS) " [thin data]" else ""
  cat(sprintf("    %d: β = %7.4f (SE %6.4f)  N_AB = %4d%s\n",
              yr, est, se_v, n_ab, thin_flag))
}

# Overall pre-trend verdict — guard against NA from failed test
p_core      <- pt_core$p
PRETREND_OK <- isTRUE(!is.na(p_core) && p_core >= 0.05)
PRETREND_UNK <- is.na(p_core)

verdict <- if (PRETREND_UNK) {
  "UNKNOWN — pre-trend test could not be computed"
} else if (PRETREND_OK) {
  "Pre-trends CONSISTENT with parallel trends (p >= 0.05 for 2003-2005 joint test)"
} else {
  "WARNING: Pre-trends DO NOT support parallel trends assumption (p < 0.05)"
}
cat(sprintf("\nVERDICT (based on 2003-2005 window with adequate sample):\n  %s\n",
            verdict))

# ── 6. Build the plot ─────────────────────────────────────────────────────────
pretrend_label <- if (PRETREND_UNK) {
  "Pre-trend test: unavailable"
} else {
  sprintf(
    "Pre-trend Wald test (2003-2005)\nstat(%d)=%.2f, p=%.3f\n%s",
    pt_core$df, pt_core$stat, pt_core$p,
    if (PRETREND_OK) "Parallel trends: consistent" else "WARNING: trends differ"
  )
}

# Shade the thin-data zone (2000-2002)
thin_rect <- if (length(THIN_YEARS) > 0) {
  data.frame(xmin = min(THIN_YEARS) - 0.5, xmax = max(THIN_YEARS) + 0.5,
             ymin = -Inf, ymax = Inf)
} else NULL

p_plot <- ggplot(event_data, aes(x = year, y = estimate)) +

  # Thin-data shading (2000-2002)
  { if (!is.null(thin_rect))
      geom_rect(data = thin_rect,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                inherit.aes = FALSE,
                fill = "grey92", alpha = 0.8) } +

  # Pre-treatment shading (2000-2005)
  annotate("rect", xmin = 1999.5, xmax = 2006.5,
           ymin = -Inf, ymax = Inf,
           fill = "steelblue", alpha = 0.04) +

  # 95% CI ribbon (NA at 2006 creates natural gap)
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              fill = "grey70", alpha = 0.40, na.rm = TRUE) +

  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.5) +

  # SGER start (2007)
  geom_vline(xintercept = 2007, linetype = "dashed",
             colour = "#d73027", linewidth = 0.7) +
  annotate("text", x = 2007.15, y = Inf,
           label = "SGER\n(2007)", hjust = 0, vjust = 1.3,
           size = 2.9, colour = "#d73027") +

  # TIER start (2020)
  geom_vline(xintercept = 2020, linetype = "dashed",
             colour = "#4575b4", linewidth = 0.7) +
  annotate("text", x = 2020.15, y = Inf,
           label = "TIER\n(2020)", hjust = 0, vjust = 1.3,
           size = 2.9, colour = "#4575b4") +

  # Reference year label
  annotate("text", x = REF_YEAR, y = -Inf,
           label = "ref\n(2006)", hjust = 0.5, vjust = -0.3,
           size = 2.5, colour = "grey50") +

  # Coefficient line and points — thin years in lighter colour
  geom_line(data = event_data |> filter(!is_thin),
            colour = "grey20", linewidth = 0.7, na.rm = TRUE) +
  geom_line(data = event_data |> filter(is_thin),
            colour = "grey60", linewidth = 0.5, linetype = "dotted", na.rm = TRUE) +
  geom_point(data = event_data |> filter(!is_thin),
             colour = "grey20", size = 1.8, na.rm = TRUE) +
  geom_point(data = event_data |> filter(is_thin),
             colour = "grey60", size = 1.4, shape = 1, na.rm = TRUE) +   # open circles

  # Pre-trend test annotation
  annotate("label", x = 2000, y = -Inf,
           label = pretrend_label,
           hjust = 0, vjust = -0.1,
           size = 2.5, colour = if (PRETREND_OK) "grey30" else "firebrick",
           label.size = 0.3, fill = "white", alpha = 0.85) +

  # Thin-data annotation
  { if (!is.null(thin_rect))
      annotate("text",
               x = mean(c(thin_rect$xmin, thin_rect$xmax)),
               y = Inf, label = "thin\ndata",
               vjust = 1.3, size = 2.3, colour = "grey55") } +

  # Scales and theme
  scale_x_continuous(
    name   = "Year",
    breaks = seq(2000, 2022, 2),
    limits = c(1999.5, 2022.5)
  ) +
  scale_y_continuous(name = "Coefficient estimate (log scale)") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(colour = "grey92"),
    panel.grid.minor = element_blank(),
    axis.title       = element_text(size = 10),
    plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0)
  ) +
  labs(
    title   = "Event Study: Alberta SGER/TIER Effect on Log(Air Emissions)",
    caption = paste0(
      "Notes: feols with facility, province, and year FEs; SEs clustered by facility (N=",
      formatC(nobs(m), format="d", big.mark=","), ").\n",
      "Reference year = 2006. Grey shading = pre-treatment period. ",
      "Dotted grey region (2000-2002) = < 500 Alberta obs; estimates unreliable.\n",
      "Ribbon = 95% CI."
    )
  )

# ── 7. Save ───────────────────────────────────────────────────────────────────
dir.create(here("output/figures"), recursive = TRUE, showWarnings = FALSE)
fig_path <- here("output/figures/fig_canada_event_study.png")
ggsave(fig_path, plot = p_plot, width = 10, height = 6, dpi = 300, units = "in")
cat("\nSaved:", fig_path, "\n")

# Save underlying data
write.csv(event_data, here("output/tables/fig_canada_event_study_data.csv"),
          row.names = FALSE)
cat("Saved: output/tables/fig_canada_event_study_data.csv\n")
cat("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
