# 02_event_study.R
# Reproduces Figure 2 from Pham & Roach (2024)
# "Spillover Benefits of Carbon Dioxide Cap and Trade: Evidence from the TRI"
# Economic Inquiry, Vol. 62(1), 2024. DOI: 10.1111/ecin.13162
#
# Stata original: EI Replication files.do, lines 35-74
# Dataset:        EI Data Coal.dta (full, no additional restrictions)
# Estimator:      OLS with facility + state + year FEs (fixest::feols)
# Clustering:     facility (frsid)
# Reference year: 2008 (no rggiXy2008 dummy in the Stata global)

suppressPackageStartupMessages({
  library(here)
  library(haven)
  library(fixest)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
})

cat("=== Figure 2 Replication: Event Study — Pham & Roach (2024) ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ── 1. Load data (identical to 01_replicate_table2.R) ────────────────────────
coal <- read_dta(here("data/raw/pham_roach_pkg/191201-V2/EI Data Coal.dta"))
cat("Loaded EI Data Coal.dta:", nrow(coal), "obs\n")
cat("Sample: full EI Data Coal.dta (no additional restrictions for Figure 2)\n\n")

# ── 2. Specification globals (mirror Stata do-file) ───────────────────────────
# Stata: gl rggiXy = "rggiXy2000 ... rggiXy2007 rggiXy2009 ... rggiXy2019"
# rggiXy2008 is intentionally absent — 2008 is the reference year
controls   <- c("ffuse", "rps_pct", "coalgas", "lgdp")
ref_year   <- 2008
event_years <- c(2000:2007, 2009:2019)   # 19 dummies, 2008 omitted
rggi_vars   <- paste0("rggiXy", event_years)

# Figure 2 outcomes (Stata foreach loop order)
outcomes <- c(
  lsumemission = "Log(Total Emission)",
  lsumreleases = "Log(Total Releases)"
)

# ── 3. Paper targets from Results.log ────────────────────────────────────────
# Extracted from Results.log lines 195–218 (lsumemission) and 325–343 (lsumreleases)
paper_targets <- bind_rows(
  tibble(
    outcome = "lsumemission",
    year    = event_years,
    b_paper = c(-1.173895,  0.188815,  0.082880, -0.017318, -0.049131,
                -0.001321, -0.064831, -0.059378,
                -0.382892, -0.386072, -0.697495, -1.315781, -0.961839,
                -0.898732, -0.874661, -1.108028, -1.343743, -1.196249, -1.234011),
    se_paper = c(0.5446574, 0.1864829, 0.1414692, 0.1055374, 0.1147253,
                 0.1025601, 0.0998978, 0.090029,
                 0.2487299, 0.1442691, 0.2556549, 0.4640622, 0.3467828,
                 0.3211263, 0.4220193, 0.4632162, 0.4324513, 0.4668583, 0.5245994)
  ),
  tibble(
    outcome = "lsumreleases",
    year    = event_years,
    b_paper = c(-0.801936,  0.040248, -0.104386, -0.133729, -0.172728,
                -0.150135, -0.167834, -0.187046,
                -0.727886, -0.544018, -0.915543, -1.688027, -1.509236,
                -1.708361, -1.833876, -1.911391, -2.167703, -2.149480, -2.352099),
    se_paper = c(0.5202718, 0.2570416, 0.2031315, 0.2024464, 0.1957491,
                 0.1763794, 0.1644353, 0.1891306,
                 0.3078379, 0.1940974, 0.2694386, 0.3853659, 0.3414236,
                 0.3979411, 0.4406402, 0.4389392, 0.4780713, 0.4743539, 0.4809511)
  )
)

# ── 4. Run event study regressions ────────────────────────────────────────────
# Stata: reghdfe depvar $rggiXy $control, absorb(frsid st year) vce(cluster frsid)
# Using pre-computed rggiXy* dummies from the dataset — exactly mirrors Stata.
# i(year, rggi, ref=2008) is equivalent but we use dummies for naming clarity.

run_event_study <- function(depvar, data) {
  fml <- as.formula(paste0(
    depvar, " ~ ", paste(rggi_vars, collapse = " + "),
    " + ", paste(controls, collapse = " + "),
    " | frsid + st + year"
  ))

  m <- feols(fml, data = data, cluster = ~frsid, warn = FALSE, notes = FALSE)

  # Extract coefficients and 95% CI for the 19 year dummies
  b   <- coef(m)[rggi_vars]
  se  <- se(m)[rggi_vars]
  ci  <- confint(m)[rggi_vars, ]

  replicated <- tibble(
    outcome  = depvar,
    year     = event_years,
    estimate = as.numeric(b),
    std_err  = as.numeric(se),
    ci_lo    = as.numeric(ci[, 1]),
    ci_hi    = as.numeric(ci[, 2])
  )

  # Insert reference year row with NA so ribbon/line visually gaps at 2008
  ref_row <- tibble(
    outcome  = depvar,
    year     = ref_year,
    estimate = NA_real_,
    std_err  = NA_real_,
    ci_lo    = NA_real_,
    ci_hi    = NA_real_
  )

  bind_rows(replicated, ref_row) |>
    arrange(year) |>
    mutate(rel_year = year - ref_year)
}

cat("Running event study regressions...\n")
event_data <- bind_rows(lapply(names(outcomes), run_event_study, data = coal))
cat("Done.\n\n")

# ── 5. Comparison vs paper ────────────────────────────────────────────────────
comparison <- event_data |>
  filter(!is.na(estimate)) |>
  left_join(paper_targets, by = c("outcome", "year")) |>
  mutate(diff_b  = estimate - b_paper,
         diff_se = std_err  - se_paper)

cat("=== COMPARISON VS PAPER (selected years) ===\n")
cat(sprintf("  %-18s  %6s  %9s  %9s  %9s\n",
            "Outcome", "Year", "β_paper", "β_replic", "Δβ"))
cat("  ", strrep("-", 57), "\n", sep = "")

key_years <- c(2000, 2007, 2009, 2012, 2019)
for (r in seq_len(nrow(comparison))) {
  row <- comparison[r, ]
  if (row$year %in% key_years) {
    cat(sprintf("  %-18s  %6d  %9.4f  %9.4f  %+9.5f\n",
                row$outcome, row$year, row$b_paper, row$estimate, row$diff_b))
  }
}

cat("\n")
max_diff <- comparison |>
  group_by(outcome) |>
  summarise(max_abs_diff = max(abs(diff_b), na.rm = TRUE), .groups = "drop")
for (r in seq_len(nrow(max_diff))) {
  cat(sprintf("  Max |Δβ| for %-20s : %.6f\n",
              max_diff$outcome[r], max_diff$max_abs_diff[r]))
}
cat("\n")

# ── 6. Plot ───────────────────────────────────────────────────────────────────
# Panel labels for facets
panel_labels <- c(
  lsumemission = "Log(Total Emission)",
  lsumreleases = "Log(Total Releases)"
)

plot_data <- event_data |>
  mutate(
    panel = factor(outcome,
                   levels = names(panel_labels),
                   labels = unname(panel_labels))
  )

p <- ggplot(plot_data, aes(x = rel_year, y = estimate)) +
  # 95% CI ribbon (gaps naturally where estimate is NA)
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              fill = "grey70", alpha = 0.4, na.rm = TRUE) +
  # Reference lines
  geom_hline(yintercept = 0,  linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  geom_vline(xintercept = 0,  linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  # Coefficient line and points
  geom_line(colour  = "grey20", linewidth = 0.7, na.rm = TRUE) +
  geom_point(colour = "grey20", size = 1.8,      na.rm = TRUE) +
  # Annotation for treatment start
  annotate("text", x = 0.2, y = Inf, label = "RGGI begins\n(2009)",
           hjust = 0, vjust = 1.3, size = 2.8, colour = "grey35") +
  # Scales
  scale_x_continuous(
    name   = "Year relative to 2008",
    breaks = seq(-8, 11, by = 2),
    limits = c(-8.5, 11.5)
  ) +
  scale_y_continuous(
    name   = "Coefficient estimate",
    breaks = seq(-3, 2, by = 1)
  ) +
  # Facets: side-by-side panels
  facet_wrap(~panel, ncol = 2) +
  # Theme
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(colour = "grey92"),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", colour = "grey80"),
    strip.text       = element_text(face = "bold", size = 10),
    axis.title       = element_text(size = 10),
    plot.caption     = element_text(size = 7.5, colour = "grey50", hjust = 0)
  ) +
  labs(
    caption = paste0(
      "Notes: Coefficients from feols with facility, state, and year FEs. ",
      "Standard errors clustered by facility. ",
      "Shaded band = 95% CI. Reference year = 2008 (no observation plotted)."
    )
  )

# ── 7. Save ───────────────────────────────────────────────────────────────────
dir.create(here("output/figures"), recursive = TRUE, showWarnings = FALSE)
out_path <- here("output/figures/fig02_event_study.png")
ggsave(out_path, plot = p, width = 10, height = 6, dpi = 300, units = "in")
cat("Saved:", out_path, "\n")

# Also save the underlying data for inspection
write.csv(event_data, here("output/tables/fig02_event_study_data.csv"), row.names = FALSE)
cat("Saved: output/tables/fig02_event_study_data.csv\n")
cat("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
