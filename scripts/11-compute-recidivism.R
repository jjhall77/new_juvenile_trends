# ============================================================================
# 11-compute-recidivism.R
# Compute 365-day felony recidivism tables by borough
# Outputs: RDS files with tidy tables ready for plotting/reporting
# ============================================================================

source(here::here("scripts", "00-setup.R"))

# ── Load survival data ──────────────────────────────────────────────────────
survival_data <- readRDS(here("data", "survival_data.rds"))

# ── Configuration ───────────────────────────────────────────────────────────
recidivism_horizon_days <- 365L

# ── Add variables needed for analysis (BEFORE borough filtering) ────────────
survival_data <- survival_data |>
  mutate(
    # Year of index arrest
    year = year(initial_arrest_date),
    
    # Age bands (use age_at_arrest for consistency with share tables)
    age_at_arrest = as.numeric(age_at_arrest),
    age_band_primary = make_age_band_primary(age_at_arrest),
    age_band_secondary = make_age_band_secondary(age_at_arrest),
    
    # Index offense type flags
    index_felony_flag = top_charge_type_day %in% c("VFO", "Felony Non-VFO"),
    index_vfo_flag = top_charge_type_day == "VFO",
    index_serious_vfo_flag = coalesce(serious_vfo_flag, FALSE),
    index_gun_flag = coalesce(gun_arrests > 0, FALSE)
  )

# ── Compute data end date and exposure window ───────────────────────────────
data_end_date <- max(survival_data$initial_arrest_date, na.rm = TRUE)
message("Data end date: ", data_end_date)

survival_data <- survival_data |>
  mutate(
    # Has full follow-up window?
    exposure_ok = initial_arrest_date <= (data_end_date - days(recidivism_horizon_days)),
    
    # Days to felony reoffense
    days_to_felony = as.numeric(next_felony_date - initial_arrest_date),
    
    # Felony within horizon
    felony_within_horizon = (felony_reoffended == 1L) &
      !is.na(days_to_felony) &
      days_to_felony >= 0 &
      days_to_felony <= recidivism_horizon_days
  )

# ============================================================================
# CITYWIDE TOTALS - COMPUTE FIRST (before any borough filtering)
# ============================================================================

message("Computing citywide recidivism totals (all records)...")

compute_citywide_recid <- function(data, index_flag_col, age_var) {
  flag_sym <- rlang::sym(index_flag_col)
  age_sym <- rlang::sym(age_var)
  
  data |>
    filter(!!flag_sym == TRUE, exposure_ok == TRUE, !is.na(!!age_sym)) |>
    group_by(year, !!age_sym) |>
    summarise(
      n_eligible = n(),
      n_reoffend = sum(felony_within_horizon, na.rm = TRUE),
      rate = n_reoffend / n_eligible,
      .groups = "drop"
    ) |>
    mutate(boro_name = "Citywide")
}

# Primary bands - citywide
citywide_recid_felony_primary <- compute_citywide_recid(
  survival_data, "index_felony_flag", "age_band_primary"
)

citywide_recid_vfo_primary <- compute_citywide_recid(
  survival_data, "index_vfo_flag", "age_band_primary"
)

citywide_recid_serious_vfo_primary <- compute_citywide_recid(
  survival_data, "index_serious_vfo_flag", "age_band_primary"
)

citywide_recid_gun_primary <- compute_citywide_recid(
  survival_data, "index_gun_flag", "age_band_primary"
)

# Secondary bands - citywide
citywide_recid_felony_secondary <- compute_citywide_recid(
  survival_data, "index_felony_flag", "age_band_secondary"
)

citywide_recid_vfo_secondary <- compute_citywide_recid(
  survival_data, "index_vfo_flag", "age_band_secondary"
)

citywide_recid_serious_vfo_secondary <- compute_citywide_recid(
  survival_data, "index_serious_vfo_flag", "age_band_secondary"
)

citywide_recid_gun_secondary <- compute_citywide_recid(
  survival_data, "index_gun_flag", "age_band_secondary"
)

# Quick check: citywide under-18 felony recidivism
message("  Citywide <18 felony recidivism check:")
citywide_recid_felony_primary |>
  filter(age_band_primary == "<18") |>
  select(year, n_eligible, rate) |>
  print(n = 10)

message("  Citywide <18 gun recidivism check:")
citywide_recid_gun_primary |>
  filter(age_band_primary == "<18") |>
  select(year, n_eligible, rate) |>
  print(n = 10)

# ============================================================================
# BOROUGH-LEVEL ANALYSIS - Now add borough codes and filter
# ============================================================================

message("\nPreparing borough-level data...")

# Add borough codes
survival_data <- survival_data |>
  mutate(
    boro_code = substr(arrest_number, 1, 1),
    boro_name = case_match(
      boro_code,
      "M" ~ "Manhattan",
      "K" ~ "Brooklyn",
      "Q" ~ "Queens",
      "B" ~ "Bronx",
      "S" ~ "Staten Island",
      .default = NA_character_
    )
  )

# Check how many records lack valid borough
n_no_boro <- sum(is.na(survival_data$boro_name))
message("  Records without valid borough code: ", format(n_no_boro, big.mark = ","))

# Filter for borough-level analysis
survival_data <- survival_data |>
  filter(!is.na(boro_name))

message("  Records with valid borough: ", format(nrow(survival_data), big.mark = ","))

# ============================================================================
# HELPER: Compute recidivism table for a given index filter and age variable
# ============================================================================

compute_recidivism_table <- function(data, index_flag_col, age_var) {
 
 flag_sym <- rlang::sym(index_flag_col)
 age_sym <- rlang::sym(age_var)
 
 data |>
   filter(!!flag_sym == TRUE, exposure_ok == TRUE, !is.na(!!age_sym)) |>
   group_by(boro_name, year, !!age_sym) |>
   summarise(
     n_eligible = n(),
     n_reoffend = sum(felony_within_horizon, na.rm = TRUE),
     rate = n_reoffend / n_eligible,
     .groups = "drop"
   ) |>
   arrange(boro_name, year, !!age_sym)
}

# ============================================================================
# RECIDIVISM: Primary age bands
# ============================================================================

message("Computing recidivism tables (primary bands)...")

recid_felony_primary <- compute_recidivism_table(
 survival_data, "index_felony_flag", "age_band_primary"
)

recid_vfo_primary <- compute_recidivism_table(
 survival_data, "index_vfo_flag", "age_band_primary"
)

recid_serious_vfo_primary <- compute_recidivism_table(
 survival_data, "index_serious_vfo_flag", "age_band_primary"
)

recid_gun_primary <- compute_recidivism_table(
 survival_data, "index_gun_flag", "age_band_primary"
)

# ============================================================================
# RECIDIVISM: Secondary age bands
# ============================================================================

message("Computing recidivism tables (secondary bands)...")

recid_felony_secondary <- compute_recidivism_table(
 survival_data, "index_felony_flag", "age_band_secondary"
)

recid_vfo_secondary <- compute_recidivism_table(
 survival_data, "index_vfo_flag", "age_band_secondary"
)

recid_serious_vfo_secondary <- compute_recidivism_table(
 survival_data, "index_serious_vfo_flag", "age_band_secondary"
)

recid_gun_secondary <- compute_recidivism_table(
 survival_data, "index_gun_flag", "age_band_secondary"
)

# ============================================================================
# SAVE ALL RECIDIVISM TABLES
# ============================================================================

recid_tables <- list(
 # Primary bands
 felony_primary      = recid_felony_primary,
 vfo_primary         = recid_vfo_primary,
 serious_vfo_primary = recid_serious_vfo_primary,
 gun_primary         = recid_gun_primary,
 
 # Secondary bands
 felony_secondary      = recid_felony_secondary,
 vfo_secondary         = recid_vfo_secondary,
 serious_vfo_secondary = recid_serious_vfo_secondary,
 gun_secondary         = recid_gun_secondary,
 
 # Citywide primary
 citywide_felony_primary      = citywide_recid_felony_primary,
 citywide_vfo_primary         = citywide_recid_vfo_primary,
 citywide_serious_vfo_primary = citywide_recid_serious_vfo_primary,
 citywide_gun_primary         = citywide_recid_gun_primary,
 
 # Citywide secondary
 citywide_felony_secondary      = citywide_recid_felony_secondary,
 citywide_vfo_secondary         = citywide_recid_vfo_secondary,
 citywide_serious_vfo_secondary = citywide_recid_serious_vfo_secondary,
 citywide_gun_secondary         = citywide_recid_gun_secondary,
 
 # Metadata
 metadata = list(
   horizon_days = recidivism_horizon_days,
   data_end_date = data_end_date,
   last_full_year = year(data_end_date - days(recidivism_horizon_days))
 )
)

saveRDS(recid_tables, here("data", "recidivism_tables.rds"))
message("\nSaved: ", here("data", "recidivism_tables.rds"))

# ── Summary ─────────────────────────────────────────────────────────────────
message("\n── Recidivism tables summary ──")
message("Boroughs: ", paste(unique(recid_felony_primary$boro_name), collapse = ", "))
first_cja_year <- min(survival_data$year, na.rm = TRUE)
message("Years with full follow-up: ", first_cja_year, "-", recid_tables$metadata$last_full_year)
message("Horizon: ", recidivism_horizon_days, " days")
