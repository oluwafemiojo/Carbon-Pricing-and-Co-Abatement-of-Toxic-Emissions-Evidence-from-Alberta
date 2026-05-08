# 03_canada_data_build.R
# Builds the Canadian extension panel from NPRI and Statistics Canada data.
# Mirrors the variable structure of EI Data Coal.dta from Pham & Roach (2024).
#
# Data sources:
#   NPRI bulk releases file     — ECCC open data (ECCC data catalogue)
#   NPRI facility locations     — ECCC open data (ECCC data catalogue)
#   GDP (table 36-10-0222-01)   — Statistics Canada via cansim package
#   Energy (table 25-10-0029-01)— Statistics Canada via cansim package
#
# Treatment: Alberta SGER/TIER vs non-Alberta provinces (BC excluded)
# Reference year for event study: 2006 (last pre-SGER year)
# Output: data/processed/canada_panel.parquet          (all industries)
#         data/processed/canada_panel_filtered.parquet  (energy NAICS only)

suppressPackageStartupMessages({
  library(here)
  library(httr)
  library(readr)      # read_csv: handles Latin-1 locale before processing headers
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(arrow)
  library(cansim)
})

cat("=== Canadian Extension Data Build ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# ── 0. Constants ──────────────────────────────────────────────────────────────
YEAR_MIN    <- 2000L
YEAR_MAX    <- 2022L
REF_YEAR    <- 2006L   # omitted year: last pre-SGER year
EVENT_YEARS <- setdiff(YEAR_MIN:YEAR_MAX, REF_YEAR)

# Exclude BC (carbon tax since 2008 contaminates control group)
PROVINCES <- c("AB", "ON", "QC", "SK", "MB", "NB", "NS", "NL", "PE")

# Statistics Canada: province name → two-letter code
PROV_MAP <- c(
  "Alberta"                   = "AB",
  "Ontario"                   = "ON",
  "Quebec"                    = "QC",
  "Québec"                    = "QC",
  "Saskatchewan"              = "SK",
  "Manitoba"                  = "MB",
  "New Brunswick"             = "NB",
  "Nova Scotia"               = "NS",
  "Newfoundland and Labrador" = "NL",
  "Prince Edward Island"      = "PE"
)

# Fossil fuel types in energy table (for ffuse construction)
FOSSIL_FUELS <- c("Natural gas", "Total coal", "Total refined petroleum products")

# ── 1. Create directories ─────────────────────────────────────────────────────
dir.create(here("data/raw/npri"),  recursive = TRUE, showWarnings = FALSE)
dir.create(here("data/processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output/tables"),  recursive = TRUE, showWarnings = FALSE)

# ── 2. Download NPRI files ────────────────────────────────────────────────────
# NOTE: The ECCC data catalogue serves an HTML wrapper page at the /data/ path,
# not a direct file download. The script tries three URL patterns; if all fail
# it stops with manual-download instructions.

npri_urls <- list(
  releases = c(
    # Pattern 1: user-specified (api/file?path=)
    paste0("https://data-donnees.az.ec.gc.ca/api/file?path=/substances/",
           "plansreports/reporting-facilities-pollutant-release-and-",
           "transfer-data/bulk-data-files-for-all-years-releases-",
           "disposals-transfers-and-facility-locations/",
           "NPRI-INRP_Releases_Rejets_1993-present.csv"),
    # Pattern 2: /data/ path (returns 200 but may redirect)
    paste0("https://data-donnees.az.ec.gc.ca/data/substances/plansreports/",
           "reporting-facilities-pollutant-release-and-transfer-data/",
           "bulk-data-files-for-all-years-releases-disposals-transfers-and-",
           "facility-locations/NPRI-INRP_Releases_Rejets_1993-present.csv"),
    # Pattern 3: URL-encoded slashes
    paste0("https://data-donnees.az.ec.gc.ca/api/file?path=",
           "%2Fsubstances%2Fplansreports%2F",
           "reporting-facilities-pollutant-release-and-transfer-data%2F",
           "bulk-data-files-for-all-years-releases-disposals-transfers-",
           "and-facility-locations%2F",
           "NPRI-INRP_Releases_Rejets_1993-present.csv")
  ),
  facilities = c(
    paste0("https://data-donnees.az.ec.gc.ca/api/file?path=/substances/",
           "plansreports/reporting-facilities-pollutant-release-and-",
           "transfer-data/bulk-data-files-for-all-years-releases-",
           "disposals-transfers-and-facility-locations/",
           "NPRI-INRP_FacilityLocations_EmplacementInstallations.csv"),
    paste0("https://data-donnees.az.ec.gc.ca/data/substances/plansreports/",
           "reporting-facilities-pollutant-release-and-transfer-data/",
           "bulk-data-files-for-all-years-releases-disposals-transfers-and-",
           "facility-locations/",
           "NPRI-INRP_FacilityLocations_EmplacementInstallations.csv")
  )
)

dest_paths <- list(
  releases   = here("data/raw/npri/NPRI_releases_all_years.csv"),
  facilities = here("data/raw/npri/NPRI_facility_locations.csv")
)

# Returns TRUE if file looks like a CSV (not an HTML error page).
# Uses file size as primary heuristic (HTML 404 pages are tiny; real CSVs are large),
# with a character-content fallback wrapped in tryCatch for encoding safety.
is_csv_content <- function(path) {
  if (!file.exists(path) || file.size(path) < 10) return(FALSE)
  if (file.size(path) > 1e6) return(TRUE)          # >1 MB is almost certainly not an HTML error
  tryCatch({
    first_bytes <- rawToChar(readBin(path, "raw", n = 200L))
    !grepl("^\\s*<", first_bytes, useBytes = TRUE)  # useBytes avoids locale translation
  }, error = function(e) TRUE)                       # if we can't tell, assume it's fine
}

download_npri <- function(file_key) {
  dest <- dest_paths[[file_key]]
  if (file.exists(dest) && is_csv_content(dest)) {
    cat(file_key, "already downloaded (", round(file.size(dest) / 1e6, 1), "MB)\n")
    return(invisible(dest))
  }
  urls <- npri_urls[[file_key]]
  for (url in urls) {
    cat("Trying", file_key, "URL:", substr(url, 1, 80), "...\n")
    tryCatch({
      resp <- GET(url, write_disk(dest, overwrite = TRUE),
                  progress(), timeout(600))
      if (status_code(resp) %in% c(200L, 206L) && is_csv_content(dest)) {
        cat("  Success:", round(file.size(dest) / 1e6, 1), "MB\n")
        return(invisible(dest))
      }
      cat("  HTTP", status_code(resp), "— not a CSV, skipping\n")
      if (file.exists(dest)) file.remove(dest)
    }, error = function(e) {
      cat("  Error:", conditionMessage(e), "\n")
      if (file.exists(dest)) file.remove(dest)
    })
  }
  # All URLs failed — stop with manual instructions
  stop(
    "\n\nAll download attempts for '", file_key, "' failed.\n\n",
    "MANUAL DOWNLOAD INSTRUCTIONS:\n",
    "  1. Go to: https://open.canada.ca/data/en/dataset/40e01423-7728-429c-ac9d-2954385ccdfb\n",
    "  2. Download the bulk CSV file for '", file_key, "'\n",
    "  3. Save it to: ", dest, "\n",
    "  4. Re-run this script.\n"
  )
}

cat("--- Downloading NPRI files ---\n")
download_npri("releases")
download_npri("facilities")

# ── 3. Column discovery helper ────────────────────────────────────────────────
# Reads just the header row and finds the best-matching column name.
# Stops with a clear error showing all available columns if no match is found.

find_col <- function(headers, ..., required = TRUE) {
  patterns <- c(...)
  for (p in patterns) {
    m <- grep(p, headers, ignore.case = TRUE, perl = TRUE, value = TRUE)
    if (length(m) >= 1L) return(m[1L])
  }
  if (!required) return(NA_character_)
  stop(
    "Cannot find column matching any of: ", paste(patterns, collapse = " | "), "\n",
    "Available columns:\n  ", paste(headers, collapse = "\n  ")
  )
}

cat("\n--- Discovering NPRI column names (Latin-1 encoding) ---\n")

# The NPRI bulk CSVs use Latin-1 (Windows-1252) with bilingual headers.
# vroom's name-repair step fails before the locale is applied, so we read
# the header line with base R, parse it, then pass explicit col_names to vroom.
read_latin1_header <- function(path) {
  con <- file(path, encoding = "latin1")
  hdr <- readLines(con, n = 1L)
  close(con)
  # Split on commas that fall between quoted fields
  fields <- strsplit(hdr, '","')[[1]]
  gsub('^"|"$', "", fields)
}

rel_hdr <- read_latin1_header(dest_paths$releases)
fac_hdr <- read_latin1_header(dest_paths$facilities)

cat("Releases file:", length(rel_hdr), "columns\n")
cat(paste(seq_along(rel_hdr), rel_hdr, sep = ": ", collapse = "\n"), "\n\n")
cat("Facility file:", length(fac_hdr), "columns\n")
cat(paste(seq_along(fac_hdr), fac_hdr, sep = ": ", collapse = "\n"), "\n\n")

# Map releases file columns
# Structure (confirmed from peek):
#   Long format — one row per facility × year × substance × release_medium × subcategory
#   Column 1:  Reporting_Year   (year)
#   Column 2:  NPRI_ID          (facility ID)
#   Column 5:  NAICS            (industry code)
#   Column 8:  PROVINCE         (province code)
#   Column 10: Substance Name (English)  (for metal classification)
#   Column 12: Group (English)  = release MEDIUM (e.g. "Releases to Air")
#   Column 14: Category (English) = release subcategory (e.g. "Stack / Point")
#   Column 16: Quantity          (tonnes)
rc <- list(
  npri_id   = find_col(rel_hdr, "NPRI_ID",       "No_INRP",   "npri.*id"),
  year      = find_col(rel_hdr, "Reporting_Year", "Ann.e",     "year"),
  province  = find_col(rel_hdr, "^PROVINCE$",     "Province",  "Prov"),
  substance = find_col(rel_hdr, "Substance.*English", "Substance Name"),
  group_en  = find_col(rel_hdr, "Group.*English", "Groupe.*Anglais"),  # = release medium
  quantity  = find_col(rel_hdr, "^Quantity",      "Quantit")
)
cat("Releases column mapping:\n")
for (nm in names(rc)) cat(sprintf("  %-12s -> %s\n", nm, rc[[nm]]))

# Map facility locations file
# Column 2:  NPRI ID / ID INRP
# Column 8:  Province / Province
# Column 15: NAICS / Code SCIAN  (first NAICS col = 6-digit code)
fc <- list(
  npri_id  = find_col(fac_hdr, "NPRI.*ID",    "ID.*INRP"),
  province = find_col(fac_hdr, "^Province /", "Province", required = FALSE),
  naics    = find_col(fac_hdr, "^NAICS / ",   "^NAICS$",  "Code SCIAN$")
)
cat("\nFacility column mapping:\n")
for (nm in names(fc)) cat(sprintf("  %-10s -> %s\n", nm, fc[[nm]]))

# ── 4. Read and filter releases ───────────────────────────────────────────────
# Read the large file (376 MB) in one pass using vroom with pre-parsed col names
# and Latin-1 locale.  We skip row 1 (header already parsed above).
cat("\n--- Reading NPRI releases (376 MB; ~2-4 min) ---\n")

rel_cols_needed <- unlist(rc)    # only load the 6 columns we need
col_idx <- match(rel_cols_needed, rel_hdr)
if (any(is.na(col_idx))) {
  stop("Column(s) not found in releases file: ",
       paste(rel_cols_needed[is.na(col_idx)], collapse=", "))
}

# Build a col_types spec: read needed cols as character, skip the rest
col_type_vec <- rep("_", length(rel_hdr))           # "_" = skip
col_type_vec[col_idx] <- "c"                         # "c" = character
col_spec <- paste(col_type_vec, collapse = "")

releases_raw <- read_csv(
  dest_paths$releases,
  col_names  = rel_hdr,
  skip       = 1L,
  col_types  = col_spec,
  locale     = locale(encoding = "latin1"),
  progress   = TRUE,
  show_col_types = FALSE
)

cat("Raw rows:", nrow(releases_raw), "\n")

releases <- releases_raw |>
  rename(
    npri_id   = all_of(rc$npri_id),
    year      = all_of(rc$year),
    province  = all_of(rc$province),
    substance = all_of(rc$substance),
    group_en  = all_of(rc$group_en),
    quantity  = all_of(rc$quantity)
  ) |>
  mutate(
    year       = as.integer(str_trim(year)),
    province   = str_trim(str_to_upper(province)),
    quantity_t = as.numeric(str_replace_all(quantity, ",", ""))
  ) |>
  filter(year >= YEAR_MIN, year <= YEAR_MAX, province %in% PROVINCES) |>
  mutate(quantity_kg = replace_na(quantity_t, 0) * 1000)   # tonnes → kg

cat("After sample filter:", nrow(releases), "rows\n")
cat("Facilities in sample:", length(unique(releases$npri_id)), "\n")

cat("\nRelease medium distribution (top 10 values of group_en):\n")
print(sort(table(releases$group_en), decreasing = TRUE)[1:10])

# ── 5. Classify release medium and substance type ─────────────────────────────
# Release medium: `group_en` is "Releases to Air", "Releases to Water",
# "Releases to Land" (and possibly subcategories — filter to top-level mediums).
# Metal classification: no chemical group column in this file version;
# classify by matching metal element names in the substance name.

METAL_PATTERN <- paste(c(
  "lead", "mercury", "arsenic", "cadmium", "chromium", "nickel", "zinc",
  "copper", "manganese", "antimony", "selenium", "thallium", "beryllium",
  "cobalt", "barium", "vanadium", "silver", "\\btin\\b", "bismuth",
  "molybdenum", "titanium", "alumin", "\\biron\\b", "gallium", "indium",
  "tellurium", "strontium", "lithium"
), collapse = "|")

releases <- releases |>
  mutate(
    # Release medium flags (group_en values from peek: "Releases to Air", etc.)
    is_air   = str_detect(str_to_lower(group_en), "\\bair\\b"),
    # Metal flag: match metal element names in English substance name
    is_metal = str_detect(str_to_lower(replace_na(substance, "")), METAL_PATTERN)
  )

cat("\nRelease medium flags:\n")
cat("  is_air==TRUE rows :", sum(releases$is_air),  "\n")
cat("  is_air==FALSE rows:", sum(!releases$is_air), "\n")
cat("\nMetal classification:\n")
cat("  is_metal==TRUE :", sum(releases$is_metal), "\n")
cat("  is_metal==FALSE:", sum(!releases$is_metal), "\n")

# ── 6. Aggregate to facility-year level ───────────────────────────────────────
cat("\n--- Aggregating to facility-year ---\n")

panel <- releases |>
  group_by(npri_id, province, year) |>
  summarise(
    # Air releases (is_air): the NPRI "Releases to Air" medium
    sum_air_kg        = sum(quantity_kg[is_air],               na.rm = TRUE),
    # Total releases: all media in this file (air + water + land)
    sum_total_kg      = sum(quantity_kg,                        na.rm = TRUE),
    # Metal / non-metal splits — both for air and total
    sum_air_metal_kg  = sum(quantity_kg[is_air &  is_metal],   na.rm = TRUE),
    sum_air_nmetal_kg = sum(quantity_kg[is_air & !is_metal],   na.rm = TRUE),
    sum_tot_metal_kg  = sum(quantity_kg[is_metal],             na.rm = TRUE),
    sum_tot_nmetal_kg = sum(quantity_kg[!is_metal],            na.rm = TRUE),
    n_substances      = n_distinct(substance),
    .groups = "drop"
  ) |>
  # log1p(x) = log(x + 1): handles true zeros without -Inf
  mutate(
    lsumemission        = log1p(sum_air_kg),
    lsumreleases        = log1p(sum_total_kg),
    lsumemission_metal1 = log1p(sum_air_metal_kg),
    lsumemission_metal0 = log1p(sum_air_nmetal_kg),
    lsumreleases_metal1 = log1p(sum_tot_metal_kg),
    lsumreleases_metal0 = log1p(sum_tot_nmetal_kg)
  )

cat("Facility-year obs:", nrow(panel), "\n")
cat("Unique facilities:", length(unique(panel$npri_id)), "\n")

# ── 7. Add NAICS from facility locations ──────────────────────────────────────
cat("\n--- Merging facility metadata ---\n")

fac_hdr_needed <- na.omit(unlist(fc))
fac_col_idx    <- match(fac_hdr_needed, fac_hdr)
fac_type_vec   <- rep("_", length(fac_hdr))
fac_type_vec[fac_col_idx] <- "c"

facilities <- read_csv(
  dest_paths$facilities,
  col_names      = fac_hdr,
  skip           = 1L,
  col_types      = paste(fac_type_vec, collapse = ""),
  locale         = locale(encoding = "latin1"),
  progress       = FALSE,
  show_col_types = FALSE
) |>
  rename(
    npri_id  = all_of(fc$npri_id),
    province = all_of(fc$province),
    naics    = all_of(fc$naics)
  ) |>
  mutate(
    npri_id = str_trim(npri_id),
    naics   = str_trim(str_sub(naics, 1L, 6L))   # keep 6-digit NAICS code
  ) |>
  distinct(npri_id, .keep_all = TRUE)

panel <- panel |>
  mutate(npri_id = str_trim(npri_id)) |>
  left_join(facilities |> select(npri_id, naics), by = "npri_id")

cat("NAICS coverage:", sum(!is.na(panel$naics)), "/", nrow(panel), "rows\n")
cat("Top 8 NAICS codes:\n")
print(sort(table(panel$naics), decreasing = TRUE)[1:8])

# ── 7b. Industry filter ───────────────────────────────────────────────────────
# Restrict to energy-sector NAICS codes that closely parallel the US paper's
# coal electric utilities sample:
#   211110 / 211113 / 211114 — oil & gas extraction (large TIER-eligible emitters)
#   221112 — fossil-fuel electric power generation (direct analog to US sample)
#   324110 — petroleum refineries (large industrial TIER participants)
NAICS_KEEP <- c("211110", "211113", "211114", "221112", "324110")

cat("\n--- Applying industry filter ---\n")
cat("NAICS codes kept:", paste(NAICS_KEEP, collapse = ", "), "\n")

n_before <- nrow(panel)
fac_before <- length(unique(panel$npri_id))

panel <- panel |> filter(naics %in% NAICS_KEEP)

n_after  <- nrow(panel)
fac_after <- length(unique(panel$npri_id))
cat(sprintf("Rows:        %d → %d (dropped %d)\n", n_before, n_after, n_before - n_after))
cat(sprintf("Facilities:  %d → %d (dropped %d)\n", fac_before, fac_after, fac_before - fac_after))

cat("\nNAICS breakdown after filter:\n")
naics_tab <- panel |>
  count(naics, sort = TRUE) |>
  mutate(label = case_when(
    naics == "211110" ~ "Oil & gas extraction",
    naics == "211113" ~ "Conventional oil & gas extraction",
    naics == "211114" ~ "Oil sands extraction",
    naics == "221112" ~ "Fossil-fuel electric power generation",
    naics == "324110" ~ "Petroleum refineries",
    TRUE              ~ naics
  ))
print(as.data.frame(naics_tab))

# ── 8. Treatment variables ────────────────────────────────────────────────────
cat("\n--- Building treatment variables ---\n")

panel <- panel |>
  mutate(
    alberta    = as.integer(province == "AB"),
    treat_sger = as.integer(year >= 2007L),    # SGER Phase 1 (analog to treatone)
    treat_tier = as.integer(year >= 2020L)     # TIER introduction (analog to treattwo)
  )

# Event-study dummies: albertaXyYYYY for all years except reference (2006)
for (yr in EVENT_YEARS) {
  panel[[paste0("albertaXy", yr)]] <- as.integer(panel$alberta == 1L & panel$year == yr)
}

cat("Treatment balance:\n")
cat("  alberta==1 facilities:", length(unique(panel$npri_id[panel$alberta == 1])), "\n")
cat("  alberta==0 facilities:", length(unique(panel$npri_id[panel$alberta == 0])), "\n")
cat("  Obs with alberta==1  :", sum(panel$alberta == 1), "\n")
cat("  Obs with alberta==0  :", sum(panel$alberta == 0), "\n")

# ── 9. Controls from Statistics Canada ───────────────────────────────────────
cat("\n--- Fetching Statistics Canada controls ---\n")

## 9a. Log real provincial GDP — table 36-10-0222-01 ─────────────────────────
# Probed structure: Estimates == "Gross domestic product at market prices",
# Prices == "Chained (2017) dollars", GEO = province name (full English)
cat("  GDP (36-10-0222-01)...\n")
lgdp_ctrl <- tryCatch({
  gdp_raw <- get_cansim("36-10-0222-01")
  ctrl <- gdp_raw |>
    filter(
      GEO %in% names(PROV_MAP),
      Prices    == "Chained (2017) dollars",
      Estimates == "Gross domestic product at market prices"
    ) |>
    transmute(
      province = PROV_MAP[as.character(GEO)],
      year     = as.integer(REF_DATE),
      lgdp     = log(VALUE)
    ) |>
    filter(!is.na(province), !is.nan(lgdp), !is.infinite(lgdp),
           year >= YEAR_MIN, year <= YEAR_MAX)
  cat("  GDP rows:", nrow(ctrl), " | provinces:",
      length(unique(ctrl$province)), " | years:",
      min(ctrl$year), "-", max(ctrl$year), "\n")
  ctrl
}, error = function(e) {
  cat("  GDP fetch failed:", conditionMessage(e), "— lgdp set to NA\n")
  NULL
})

## 9b. Log fossil fuel share — table 25-10-0029-01 ────────────────────────────
# Probed structure:
#   Supply and demand characteristics == "Energy use, final demand"
#   Fuel type: "Natural gas", "Total coal", "Total refined petroleum products"
#              vs "Total primary and secondary energy"
#   GEO: province names; REF_DATE: year (integer string)
cat("  Energy (25-10-0029-01)...\n")
ffuse_ctrl <- tryCatch({
  en_raw <- get_cansim("25-10-0029-01")

  ctrl <- en_raw |>
    filter(
      GEO %in% names(PROV_MAP),
      `Supply and demand characteristics` == "Energy use, final demand",
      `Fuel type` %in% c("Total primary and secondary energy", FOSSIL_FUELS)
    ) |>
    transmute(
      province  = PROV_MAP[as.character(GEO)],
      year      = as.integer(REF_DATE),
      fuel      = as.character(`Fuel type`),
      value_tj  = VALUE
    ) |>
    filter(!is.na(province), year >= YEAR_MIN, year <= YEAR_MAX) |>
    group_by(province, year) |>
    summarise(
      fossil_tj = sum(value_tj[fuel %in% FOSSIL_FUELS], na.rm = TRUE),
      total_tj  = sum(value_tj[fuel == "Total primary and secondary energy"], na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(total_tj > 0, fossil_tj > 0) |>
    mutate(ffuse = log(fossil_tj / total_tj))

  cat("  Energy rows:", nrow(ctrl), " | provinces:",
      length(unique(ctrl$province)), " | years:",
      min(ctrl$year), "-", max(ctrl$year), "\n")

  # Spot check: Alberta 2010 should have high fossil share (~0.85+)
  ab_check <- ctrl |> filter(province == "AB", year == 2010)
  if (nrow(ab_check) > 0) {
    cat("  AB 2010 check: fossil share =",
        round(exp(ab_check$ffuse), 3), " | ffuse =",
        round(ab_check$ffuse, 4), "\n")
  }
  ctrl
}, error = function(e) {
  cat("  Energy fetch failed:", conditionMessage(e), "— ffuse set to NA\n")
  NULL
})

## 9c. Coal/gas price ratio (coalgas) — sourced from two cansim tables ────────
# Coal:    StatsCan 18-10-0268-01 IPPI, NAPCS "Hard coal" (national index,
#          base = 2019; covers 1981-present)
# Nat gas: StatsCan 18-10-0004-01 CPI, "Natural gas" (provincial for
#          AB/MB/ON/QC/SK; national average for NB/NS/NL/PE which lack
#          residential gas distribution networks)
# coalgas = log(coal_IPPI / natgas_CPI)
# Province FEs absorb level differences from different index base years.
cat("  coalgas (18-10-0268-01 + 18-10-0004-01)...\n")
coalgas_ctrl <- tryCatch({

  # --- Coal IPPI ---
  ippi <- get_cansim("18-10-0268-01")
  napcs_col <- names(ippi)[str_detect(names(ippi), "(?i)napcs|product")][1]
  coal_idx <- ippi |>
    filter(str_detect(as.character(.data[[napcs_col]]), "(?i)hard coal")) |>
    mutate(year = as.integer(substr(REF_DATE, 1, 4))) |>
    filter(year >= YEAR_MIN, year <= YEAR_MAX) |>
    group_by(year) |>
    summarise(coal_ippi = mean(VALUE, na.rm = TRUE), .groups = "drop")

  # --- Natural gas CPI (provincial) ---
  cpi_raw <- get_cansim("18-10-0004-01")
  ng_prov <- cpi_raw |>
    filter(as.character(GEO) %in% names(PROV_MAP),
           as.character(`Products and product groups`) == "Natural gas",
           str_detect(REF_DATE, "-01$")) |>
    mutate(province = PROV_MAP[as.character(GEO)],
           year     = as.integer(substr(REF_DATE, 1, 4))) |>
    filter(province %in% PROVINCES, year >= YEAR_MIN, year <= YEAR_MAX) |>
    group_by(province, year) |>
    summarise(natgas_cpi = mean(VALUE, na.rm = TRUE), .groups = "drop")

  # National fallback for provinces without gas CPI (NB, NS, NL, PE)
  ng_national <- cpi_raw |>
    filter(as.character(GEO) == "Canada",
           as.character(`Products and product groups`) == "Natural gas",
           str_detect(REF_DATE, "-01$")) |>
    mutate(year = as.integer(substr(REF_DATE, 1, 4))) |>
    filter(year >= YEAR_MIN, year <= YEAR_MAX) |>
    group_by(year) |>
    summarise(natgas_national = mean(VALUE, na.rm = TRUE), .groups = "drop")

  # --- Build ratio ---
  ctrl <- expand.grid(province = PROVINCES, year = YEAR_MIN:YEAR_MAX,
                      stringsAsFactors = FALSE) |>
    left_join(ng_prov,     by = c("province", "year")) |>
    left_join(ng_national, by = "year") |>
    left_join(coal_idx,    by = "year") |>
    mutate(
      natgas_use = coalesce(natgas_cpi, natgas_national),
      coalgas    = if_else(!is.na(coal_ippi) & !is.na(natgas_use) & natgas_use > 0,
                           log(coal_ippi / natgas_use), NA_real_)
    ) |>
    select(province, year, coalgas)

  cat("  coalgas rows:", nrow(ctrl),
      "| non-NA:", sum(!is.na(ctrl$coalgas)), "\n")
  ctrl
}, error = function(e) {
  cat("  coalgas fetch failed:", conditionMessage(e), "— set to NA\n")
  NULL
})

# ── 10. Merge controls onto panel ─────────────────────────────────────────────
cat("\n--- Merging controls ---\n")

if (!is.null(lgdp_ctrl)) {
  panel <- left_join(panel, lgdp_ctrl, by = c("province", "year"))
} else {
  panel$lgdp <- NA_real_
}

if (!is.null(ffuse_ctrl)) {
  panel <- left_join(panel, ffuse_ctrl |> select(province, year, ffuse),
                     by = c("province", "year"))
} else {
  panel$ffuse <- NA_real_
}

if (!is.null(coalgas_ctrl)) {
  panel <- left_join(panel, coalgas_ctrl, by = c("province", "year"))
} else {
  panel$coalgas <- NA_real_
}

cat("Control variable coverage:\n")
for (v in c("lgdp", "ffuse", "coalgas")) {
  n_ok <- sum(!is.na(panel[[v]]))
  cat(sprintf("  %-10s: %d/%d non-NA (%.1f%%)\n",
              v, n_ok, nrow(panel), 100 * n_ok / nrow(panel)))
}

# ── 11. Final panel assembly ──────────────────────────────────────────────────
cat("\n--- Final panel ---\n")

outcome_vars  <- c("lsumemission", "lsumreleases",
                   "lsumemission_metal1", "lsumemission_metal0",
                   "lsumreleases_metal1", "lsumreleases_metal0")
treat_vars    <- c("alberta", "treat_sger", "treat_tier")
event_vars    <- paste0("albertaXy", EVENT_YEARS)
control_vars  <- c("lgdp", "ffuse", "coalgas")
raw_agg_vars  <- c("sum_air_kg", "sum_total_kg", "sum_air_metal_kg",
                   "sum_air_nmetal_kg", "sum_tot_metal_kg", "sum_tot_nmetal_kg")

canada_panel <- panel |>
  select(
    # Identifiers
    npri_id, province, year, naics,
    # Outcomes
    all_of(outcome_vars),
    # Raw aggregates (kept for audit trail)
    all_of(raw_agg_vars),
    # Treatment
    all_of(treat_vars),
    all_of(event_vars),
    # Controls
    all_of(control_vars)
  ) |>
  arrange(npri_id, year)

cat("Final dimensions:", nrow(canada_panel), "obs x", ncol(canada_panel), "vars\n")

# ── 12. Print summary ─────────────────────────────────────────────────────────
cat("\n=== PANEL SUMMARY ===\n")
cat(sprintf("  %-30s %d\n", "Total observations:",  nrow(canada_panel)))
cat(sprintf("  %-30s %d\n", "Unique facilities:",   length(unique(canada_panel$npri_id))))
cat(sprintf("  %-30s %d – %d\n", "Years covered:", min(canada_panel$year), max(canada_panel$year)))

cat("\n  Province breakdown:\n")
prov_tab <- canada_panel |>
  group_by(province) |>
  summarise(
    n_facilities = n_distinct(npri_id),
    n_obs        = n(),
    treated      = max(alberta),
    .groups = "drop"
  ) |>
  arrange(desc(treated), desc(n_obs))
print(as.data.frame(prov_tab))

cat("\n  Mean lsumemission by treatment group:\n")
treat_means <- canada_panel |>
  group_by(
    group   = ifelse(alberta == 1, "Alberta (treated)", "Other provinces (control)"),
    period  = ifelse(year < 2007, "Pre-SGER (2000-2006)", "Post-SGER (2007+)")
  ) |>
  summarise(
    mean_lsumemission = round(mean(lsumemission, na.rm = TRUE), 3),
    mean_lsumreleases = round(mean(lsumreleases, na.rm = TRUE), 3),
    n_obs             = n(),
    .groups = "drop"
  )
print(as.data.frame(treat_means))

cat("\n  Outcome variable summary:\n")
for (v in outcome_vars) {
  x <- canada_panel[[v]]
  cat(sprintf("  %-26s mean=%6.3f  sd=%5.3f  zeros=%4d\n",
              v, mean(x, na.rm=TRUE), sd(x, na.rm=TRUE), sum(x == 0, na.rm=TRUE)))
}

# ── 13. Save outputs ──────────────────────────────────────────────────────────
cat("\n--- Saving outputs ---\n")

# Full panel (all industries, pre-filter) — kept for reference
parquet_path <- here("data/processed/canada_panel.parquet")
write_parquet(canada_panel, parquet_path)
cat("Saved (full)    :", parquet_path, "\n")
cat("  File size:", round(file.size(parquet_path) / 1e6, 2), "MB\n")

# Filtered panel (energy NAICS only) — primary analysis dataset
filtered_path <- here("data/processed/canada_panel_filtered.parquet")
canada_panel_filtered <- canada_panel |> filter(naics %in% NAICS_KEEP)
write_parquet(canada_panel_filtered, filtered_path)
cat("Saved (filtered):", filtered_path, "\n")
cat("  File size:", round(file.size(filtered_path) / 1e6, 2), "MB\n")

cat("\n=== FILTERED PANEL: PROVINCE BREAKDOWN ===\n")
prov_filtered <- canada_panel_filtered |>
  group_by(province) |>
  summarise(
    n_facilities = n_distinct(npri_id),
    n_obs        = n(),
    treated      = max(alberta),
    mean_lsumemission = round(mean(lsumemission, na.rm = TRUE), 3),
    .groups = "drop"
  ) |>
  arrange(desc(treated), desc(n_obs))
print(as.data.frame(prov_filtered))

cat("\nFiltered total  :", nrow(canada_panel_filtered), "obs,",
    length(unique(canada_panel_filtered$npri_id)), "facilities\n")
cat("Alberta (treated):", sum(canada_panel_filtered$alberta == 1), "obs,",
    length(unique(canada_panel_filtered$npri_id[canada_panel_filtered$alberta == 1])),
    "facilities\n")
cat("Control provinces:", sum(canada_panel_filtered$alberta == 0), "obs,",
    length(unique(canada_panel_filtered$npri_id[canada_panel_filtered$alberta == 0])),
    "facilities\n")

# Data dictionary
dict <- tibble(
  variable    = names(canada_panel),
  type        = sapply(canada_panel, function(x) class(x)[1]),
  n_nonmissing = sapply(canada_panel, function(x) sum(!is.na(x))),
  n_missing   = sapply(canada_panel, function(x) sum(is.na(x))),
  description = case_when(
    variable == "npri_id"          ~ "NPRI Facility ID (stable across years; analog of frsid)",
    variable == "province"         ~ "Two-letter province code",
    variable == "year"             ~ "Reporting year",
    variable == "naics"            ~ "6-digit NAICS code from facility locations file",
    variable == "lsumemission"     ~ "log1p(total air releases in kg); analog of lsumemission",
    variable == "lsumreleases"     ~ "log1p(total releases all media in kg); analog of lsumreleases",
    variable == "lsumemission_metal1" ~ "log1p(air releases, metal substances only)",
    variable == "lsumemission_metal0" ~ "log1p(air releases, non-metal substances only)",
    variable == "lsumreleases_metal1" ~ "log1p(total releases, metal substances only)",
    variable == "lsumreleases_metal0" ~ "log1p(total releases, non-metal substances only)",
    variable == "sum_air_kg"       ~ "Raw sum of air releases (kg) before log transform",
    variable == "sum_total_kg"     ~ "Raw sum of all releases (kg) before log transform",
    variable == "sum_air_metal_kg" ~ "Raw sum of metal air releases (kg)",
    variable == "sum_air_nmetal_kg"~ "Raw sum of non-metal air releases (kg)",
    variable == "sum_tot_metal_kg" ~ "Raw sum of metal total releases (kg)",
    variable == "sum_tot_nmetal_kg"~ "Raw sum of non-metal total releases (kg)",
    variable == "alberta"          ~ "=1 if facility in Alberta (treated group); analog of rggi",
    variable == "treat_sger"       ~ "=1 if year >= 2007 (SGER Phase 1 start); analog of treatone",
    variable == "treat_tier"       ~ "=1 if year >= 2020 (TIER introduction); analog of treattwo",
    str_starts(variable, "albertaXy") ~
      paste0("Alberta x I(year==", str_remove(variable, "albertaXy"),
             "); event study dummy; reference year = 2006"),
    variable == "lgdp"             ~ "log(provincial real GDP, chained 2017 CAD); from StatCan 36-10-0222-01",
    variable == "ffuse"            ~ "log(fossil fuel share of provincial final energy demand); from StatCan 25-10-0029-01",
    variable == "coalgas"          ~ "log(coal IPPI / natgas CPI); coal from StatCan 18-10-0268-01 NAPCS Hard coal (national); gas from 18-10-0004-01 (provincial AB/MB/ON/QC/SK, national fallback for NB/NS/NL/PE)",
    TRUE                           ~ ""
  )
)

dict_path <- here("data/processed/canada_panel_dict.csv")
write.csv(dict, dict_path, row.names = FALSE)
cat("Saved:", dict_path, "\n")

cat("\nCompleted:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
