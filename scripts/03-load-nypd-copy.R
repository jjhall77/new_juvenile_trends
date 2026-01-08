# ============================================================================
# 03-load-nypd.R
# Load and prepare NYPD open data arrests (Historic + YTD)
# ============================================================================

source(here::here("scripts", "00-setup.R"))

# ── Load and combine NYPD data ──────────────────────────────────────────────
message("Loading NYPD arrest data...")

nypd_files <- c(files$nypd_ytd, files$nypd_hist)

# Force law_cat_cd to be read as character (not logical)
nypd_arrests <- nypd_files |>
  map(\(f) {
    read_csv(f, col_types = cols(LAW_CAT_CD = col_character())) |>
      mutate(source_file = basename(f))
  }) |>
  list_rbind() |>
  clean_names()

# ── Define valid age groups ─────────────────────────────────────────────────
valid_age_groups <- c("<18", "18-24", "25-44", "45-64", "65+")

# ── Clean and prepare ───────────────────────────────────────────────────────
nypd_arrests <- nypd_arrests |>
  filter(age_group %in% valid_age_groups) |>
  mutate(
    # Parse date (NYPD uses MM/DD/YYYY format)
    arrest_date = mdy(arrest_date),
    year = year(arrest_date),
    
    # Create offense category flags
    is_felony      = (law_cat_cd == "F"),
    is_seven_major = (ky_cd %in% nypd_codes$seven_major),
    is_violent     = (ky_cd %in% nypd_codes$violent),
    
    # Standardize age group factor ordering
    age_group = factor(age_group, levels = valid_age_groups)
  ) |>
  filter(!is.na(year), !is.na(age_group)) %>%
  filter(year < 2025)

# ── Fiscal year (July 1 start) ──────────────────────────────────────────────
nypd_arrests <- nypd_arrests |>
  mutate(
    fiscal_year = if_else(month(arrest_date) >= 7L, year + 1L, year),
    fy_label = paste0("FY", fiscal_year)
  )

# ── Validation ──────────────────────────────────────────────────────────────
message("\n── NYPD data summary ──")
message("Total arrests: ", format(nrow(nypd_arrests), big.mark = ","))
message("Date range: ", min(nypd_arrests$arrest_date, na.rm = TRUE), " to ",
        max(nypd_arrests$arrest_date, na.rm = TRUE))

nypd_arrests |>
  summarize(
    n = n(),
    pct_felony      = mean(is_felony, na.rm = TRUE),
    pct_seven_major = mean(is_seven_major, na.rm = TRUE),
    pct_violent     = mean(is_violent, na.rm = TRUE)
  ) |>
  print()

message("\nArrests by age group:")
nypd_arrests |>
  count(age_group) |>
  mutate(pct = n / sum(n)) |>
  print()

# ── Save ────────────────────────────────────────────────────────────────────
saveRDS(nypd_arrests, here("data", "nypd_arrests_clean.rds"))
message("\nSaved: ", here("data", "nypd_arrests_clean.rds"))
