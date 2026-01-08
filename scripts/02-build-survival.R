# ============================================================================
# 02-build-survival.R
# Build survival/recidivism dataset from arrest records
# ============================================================================

source(here::here("scripts", "00-setup.R"))

# ── Load cleaned arrests (or run 01-load-cja.R if needed) ───────────────────
arrests_path <- here("data", "arrests_clean.rds")

if (!file.exists(arrests_path)) {
  message("Running 01-load-cja.R first...")
  source(here("scripts", "01-load-cja.R"))
}

arrests <- readRDS(arrests_path)

# ============================================================================
# STEP 1: Build arrest backbone (one record per arrest with computed age)
# ============================================================================

arrest_backbone <- arrests |>
  select(
    arrest_number, lumid, county,
    offense_date, arrest_date, arrest_time, arrest_date_and_time,
    date_of_birth, sex, race, ethnicity,
    gun_arrests, serious_vfo_flag, age_at_offense, age_at_arrest,
    most_serious_charge_arrest, ms_charge_art_sec,
    ms_charge_attempt_indicator, ms_charge_description_cat
  ) |>
  distinct(arrest_number, .keep_all = TRUE) |>
  mutate(
    # Ensure ages are numeric
    age_at_offense = as.numeric(age_at_offense),
    age_at_arrest  = as.numeric(age_at_arrest),
    
    # Fill missing age_at_arrest from DOB + arrest_date if needed
    age_at_arrest = if_else(
      is.na(age_at_arrest) & !is.na(arrest_date) & !is.na(date_of_birth),
      floor(time_length(interval(date_of_birth, arrest_date), "years")),
      age_at_arrest
    )
  )

# ============================================================================
# STEP 2: Collapse to one record per person per day (highest severity)
# ============================================================================

arrest_person_day <- arrest_backbone |>
  mutate(
    ms_charge_ranked = factor(
      most_serious_charge_arrest,
      levels = charge_rank_levels,
      ordered = TRUE
    )
  ) |>
  group_by(lumid, arrest_date) |>
  mutate(
    num_arrests_day = n(),
    gun_arrests = sum(gun_arrests > 0, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(lumid, arrest_date, ms_charge_ranked) |>
  distinct(lumid, arrest_date, .keep_all = TRUE) |>
  select(-ms_charge_ranked) |>
  rename(
    top_charge_type_day = most_serious_charge_arrest,
    top_charge_description = ms_charge_description_cat
  )

rm(arrest_backbone)

# ============================================================================
# STEP 3: Build reoffense lookup using data.table (fast)
# ============================================================================

message("Building reoffense lookup (this may take a moment)...")

# Convert arrests to data.table for efficient joins
arrests_dt <- as.data.table(arrests)

arrests_dt[, `:=`(
  offense_date = as.Date(offense_date),
  arrest_date  = as.Date(arrest_date)
)]

# Reoffense timing: use offense_date, fallback to arrest_date
arrests_dt[, event_date := fcoalesce(offense_date, arrest_date)]
arrests_dt <- arrests_dt[!is.na(event_date)]

# Create offense type indicators
arrests_dt[, `:=`(
  is_vfo        = (most_serious_charge_arrest == "VFO"),
  is_felony     = (most_serious_charge_arrest %in% c("VFO", "Felony Non-VFO")),
  is_any_arrest = (most_serious_charge_arrest %in% c("VFO", "Felony Non-VFO", "Misdemeanor", "VI", "Unknown"))
)]

max_data_date <- max(arrests_dt$event_date, na.rm = TRUE)

# Prepare lookup table for joins
reoffense_lookup <- arrests_dt[, .(
  lumid,
  re_arrest_number = arrest_number,
  event_date_re    = event_date,
  is_vfo_re        = is_vfo,
  is_felony_re     = is_felony,
  is_any_arrest_re = is_any_arrest
)]
setkey(reoffense_lookup, lumid, event_date_re)

# Prepare index arrests
index_dt <- as.data.table(arrest_person_day)
index_dt[, `:=`(
  initial_arrest_date = as.Date(arrest_date),
  initial_record_id   = .I
)]
setkey(index_dt, lumid, initial_arrest_date)

# ============================================================================
# STEP 4: Find subsequent reoffenses via cartesian join + filter
# ============================================================================

# Join on lumid only, then filter to subsequent events
joined <- index_dt[
  reoffense_lookup,
  on = .(lumid),
  allow.cartesian = TRUE,
  nomatch = NULL,
  .(
    initial_record_id,
    lumid,
    initial_arrest_date,
    initial_arrest_number = arrest_number,
    ms_charge_art_sec_initial = ms_charge_art_sec,
    event_date_re,
    is_vfo_re,
    is_felony_re,
    is_any_arrest_re,
    re_arrest_number
  )
]

# Keep only subsequent, distinct reoffenses
subsequent <- joined[
  event_date_re > initial_arrest_date &
    re_arrest_number != initial_arrest_number
]

rm(joined)

# ============================================================================
# STEP 5: Summarize to first reoffense of each type
# ============================================================================

reoffense_summary <- subsequent[
  , .(
    next_vfo_date        = min(event_date_re[is_vfo_re], na.rm = TRUE),
    vfo_reoffended       = any(is_vfo_re, na.rm = TRUE),
    next_felony_date     = min(event_date_re[is_felony_re], na.rm = TRUE),
    felony_reoffended    = any(is_felony_re, na.rm = TRUE),
    next_any_arrest_date = min(event_date_re[is_any_arrest_re], na.rm = TRUE),
    any_arrest_reoffended = any(is_any_arrest_re, na.rm = TRUE)
  ),
  by = .(initial_record_id, lumid, initial_arrest_date)
]

# Convert Inf (no reoffense found) to NA
date_cols <- c("next_vfo_date", "next_felony_date", "next_any_arrest_date")
for (col in date_cols) {
  reoffense_summary[is.infinite(get(col)), (col) := NA_Date_]
}

rm(subsequent)

# ============================================================================
# STEP 6: Merge back and finalize survival dataset
# ============================================================================

survival_dt <- merge(
  index_dt,
  reoffense_summary,
  by = c("initial_record_id", "lumid", "initial_arrest_date"),
  all.x = TRUE
)

# Fill in censoring dates and indicators
survival_dt[, `:=`(
  # VFO
  next_vfo_date = fifelse(vfo_reoffended == TRUE, next_vfo_date, max_data_date),
  vfo_reoffended = fifelse(vfo_reoffended == TRUE, 1L, 0L),
  
  # Felony
  next_felony_date = fifelse(felony_reoffended == TRUE, next_felony_date, max_data_date),
  felony_reoffended = fifelse(felony_reoffended == TRUE, 1L, 0L),
  
  # Any arrest
  next_any_arrest_date = fifelse(any_arrest_reoffended == TRUE, next_any_arrest_date, max_data_date),
  any_arrest_reoffended = fifelse(any_arrest_reoffended == TRUE, 1L, 0L)
)]

# Clean up temp columns
survival_dt[, initial_record_id := NULL]

# Convert back to tibble
survival_data <- as_tibble(survival_dt) |>
  mutate(county = substr(arrest_number, 1, 1)) |>
  select(
    # Identifiers
    lumid, initial_arrest_date, arrest_number, county,
    # Index arrest info
    num_arrests_day, top_charge_type_day, top_charge_description,
    ms_charge_attempt_indicator, ms_charge_art_sec, serious_vfo_flag, gun_arrests,
    # VFO reoffense
    next_vfo_date, vfo_reoffended,
    # Felony reoffense
    next_felony_date, felony_reoffended,
    # Any arrest reoffense
    next_any_arrest_date, any_arrest_reoffended,
    # Demographics & ages (age_at_arrest for classification, age_at_offense preserved)
    age_at_arrest, age_at_offense,
    everything()
  )

rm(index_dt, reoffense_lookup, reoffense_summary, survival_dt, arrests_dt)

# ── Validation ──────────────────────────────────────────────────────────────
message("\n── Survival data summary ──")
message("Total person-days: ", format(nrow(survival_data), big.mark = ","))
message("Data end date: ", max_data_date)

survival_data |>
  summarize(
    n = n(),
    pct_felony_reoffend = mean(felony_reoffended, na.rm = TRUE),
    pct_vfo_reoffend    = mean(vfo_reoffended, na.rm = TRUE),
    pct_any_reoffend    = mean(any_arrest_reoffended, na.rm = TRUE)
  ) |>
  print()

# Check age_at_arrest availability
message("\nAge at arrest availability:")
message("  Non-missing: ", sum(!is.na(survival_data$age_at_arrest)), 
        " (", round(100 * mean(!is.na(survival_data$age_at_arrest)), 1), "%)")

# ── Save ────────────────────────────────────────────────────────────────────
saveRDS(survival_data, here("data", "survival_data.rds"))
message("\nSaved: ", here("data", "survival_data.rds"))
