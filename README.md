# Carbon Pricing and Co-Abatement of Toxic Emissions: Evidence from Alberta's SGER/TIER Regulation

**Oluwafemi Michael Ojo** · University of British Columbia · May 2026

---

## Overview

This repository contains the replication code for Pham & Roach (2024) and a Canadian extension estimating the co-abatement effects of Alberta's industrial carbon pricing regulations on toxic air emissions.

**Key findings:**
- **US replication**: RGGI reduced toxic air emissions from coal utilities by **−62%** (matches paper within 0.5 pp across all outcomes)
- **Canadian extension**: Alberta SGER reduced energy-sector air emissions by **−17.5%** (Phase 1, 2007); TIER added **−26.5%** (Phase 2, 2020); combined effect **−39.4%**, roughly half the RGGI benchmark — consistent with intensity-based vs. cap-and-trade design

---

## Repository structure

```
.
├── code/
│   ├── 01_replicate_table2.R       # US DiD replication — Table 2 of Pham & Roach (2024)
│   ├── 02_event_study.R            # US event study — Figure 2 of Pham & Roach (2024)
│   ├── 03_canada_data_build.R      # Canadian panel construction (NPRI + Statistics Canada)
│   ├── 04_canada_regressions.R     # Canadian DiD regressions + pairs cluster bootstrap
│   └── 05_canada_event_study.R     # Canadian event study figure
│
├── data/
│   ├── raw/
│   │   └── pham_roach_pkg/         # Pham & Roach replication package (Stata data + do-file)
│   └── processed/
│       ├── canada_panel.parquet            # Full NPRI panel (all industries, 140k obs)
│       ├── canada_panel_filtered.parquet   # Energy-sector panel (64k obs, 12,710 facilities)
│       ├── canada_panel_dict.csv           # Variable dictionary
│       └── coalgas_canada.rds              # Coal/gas price ratio series (StatsCan)
│
├── output/
│   ├── figures/
│   │   ├── fig02_event_study.png           # US event study (Figure 2 replication)
│   │   └── fig_canada_event_study.png      # Canadian event study
│   └── tables/
│       ├── table2_replication.csv          # US Table 2 replication results
│       ├── table_canada_main.csv           # Canadian DiD results vs US benchmarks
│       ├── fig02_event_study_data.csv      # Underlying event study coefficients (US)
│       └── fig_canada_event_study_data.csv # Underlying event study coefficients (Canada)
│
└── paper/
    ├── working_paper.tex                   # LaTeX source
    └── Ojo_CarbonCoAbatement_WorkingPaper.pdf  # Compiled working paper
```

---

## Data sources

### US replication
- **Pham & Roach (2024) replication package** — `data/raw/pham_roach_pkg/`  
  Available at: https://doi.org/10.3886/E191201V2  
  Contains pre-built Stata `.dta` panels and the original `.do` file.  
  *Raw TRI CSVs (1.2 GB) are excluded from this repo; download from [EPA TRI](https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-present).*

### Canadian extension
- **NPRI Bulk Data** — download from [Open Canada](https://open.canada.ca/data/en/dataset/40e01423-7728-429c-ac9d-2954385ccdfb)  
  Save as:
  - `data/raw/npri/NPRI_releases_all_years.csv` (~390 MB)
  - `data/raw/npri/NPRI_facility_locations.csv` (~14 MB)
- **Statistics Canada** — pulled automatically via the `cansim` R package:
  - Table 36-10-0222-01 (provincial real GDP)
  - Table 25-10-0029-01 (energy supply and demand)
  - Table 18-10-0268-01 (IPPI hard coal)
  - Table 18-10-0004-01 (CPI natural gas)

---

## Reproducing the results

### Requirements

- R 4.4.1+
- R packages: `haven`, `fixest`, `car`, `tidyverse`, `arrow`, `cansim`, `ggplot2`, `boot`
- LaTeX (MiKTeX or TeX Live) for compiling the working paper

### Run order

```r
# 1. US replication
source("code/01_replicate_table2.R")   # ~5 seconds
source("code/02_event_study.R")        # ~5 seconds

# 2. Canadian data build (requires NPRI files in data/raw/npri/)
source("code/03_canada_data_build.R")  # ~90 seconds

# 3. Canadian analysis
source("code/04_canada_regressions.R") # ~70 seconds (includes bootstrap)
source("code/05_canada_event_study.R") # ~5 seconds
```

All scripts print a completion timestamp and save outputs automatically. No absolute paths — all paths use `here::here()`.

---

## Paper

The working paper `paper/Ojo_CarbonCoAbatement_WorkingPaper.pdf` documents:
- Institutional comparison of RGGI vs. SGER/TIER
- DiD specification and parallel trends validation (F(3) = 1.70, p = 0.166)
- Main results table (US vs. Canada side-by-side)
- Event study figure with two treatment event lines (SGER 2007, TIER 2020)
- Discussion of why log air emissions and log total releases are nearly identical in the Canadian sample (99.2% of NPRI releases from oil and gas are to air)

---

## Reference

Pham, H. and Roach, T. (2024). Spillover benefits of carbon dioxide cap and trade: Evidence from the Toxics Release Inventory. *Economic Inquiry*, 62(1). https://doi.org/10.1111/ecin.13162

---

## AI usage

This project was developed with the assistance of Claude Sonnet 4.6 (Anthropic) via Claude Code. See the AI Usage Statement in the working paper for details.
