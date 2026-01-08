# ============================================================================
# 01-load-cja.R
# Load and prepare CJA arrest data with offense flags
# Gun arrests identified via ms_charge_art_sec (PL 265.03 = CPW 2nd)
# ============================================================================

source(here::here("scripts", "00-setup.R"))

# ── Load raw data ───────────────────────────────────────────────────────────
message("Loading CJA arrests...")
arrests_raw <- read_csv(files$cja_arrests) |>
  clean_names() |>
  null_to_na()

# ── Basic arrest cleaning ───────────────────────────────────────────────────
arrests <- arrests_raw |>
  # Exclude voided arrests
  filter(is.na(arrest_void_flag) | arrest_void_flag != 1) |>
  # Parse dates
  mutate(
    date_of_birth = ymd(date_of_birth, quiet = TRUE),
    arrest_date   = ymd(arrest_date, quiet = TRUE),
    offense_date  = ymd(offense_date, quiet = TRUE)
  )

rm(arrests_raw)

# ── Identify gun arrests from top charge ────────────────────────────────────
# Gun = CPW 2nd (PL 265.03) as top charge
gun_arrest_numbers <- arrests |>
  filter(str_detect(ms_charge_art_sec, "PL 26503")) |>
  distinct(arrest_number) |>
  pull(arrest_number)

# ── Create offense classification flags ─────────────────────────────────────
arrests <- arrests |>
  mutate(
    # Normalize text fields for matching
    ms_text  = str_to_upper(coalesce(most_serious_charge_arrest, "")),
    vfo_text = str_to_upper(coalesce(ms_charge_vfo, "")),
    
    # Felony flag: VFO or Felony Non-VFO
    felony_flag = case_when(
      ms_text %in% c("VFO", "FELONY NON-VFO") ~ TRUE,
      vfo_text %in% c("VFO", "1", "TRUE")     ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # VFO flag
    vfo_flag = case_when(
      ms_text == "VFO"                    ~ TRUE,
      vfo_text %in% c("VFO", "1", "TRUE") ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Gun arrests (integer for summing) and flag (logical)
    gun_arrests = if_else(arrest_number %in% gun_arrest_numbers, 1L, 0L),
    gun_flag = gun_arrests == 1L,
    
    # Serious VFO: Assault 1st (PL 120.10), Murder/Attempted Murder (PL 125.25-27), Robbery 1st (PL 160.15)
    att_val  = replace_na(suppressWarnings(as.numeric(ms_charge_attempt_indicator)), 0),
    a1_flag  = str_detect(ms_charge_art_sec, "^PL\\s*12010"),
    mur_flag = str_detect(ms_charge_art_sec, "^PL\\s*125(25|26|27)"),
    rob1_flag = str_detect(ms_charge_art_sec, "^PL\\s*16015"),
    serious_vfo_flag = a1_flag | (mur_flag & att_val %in% c(0, 1)) | rob1_flag
  ) |>
  select(-ms_text, -vfo_text, -att_val, -a1_flag, -mur_flag, -rob1_flag)

rm(gun_arrest_numbers)

# ── Add age bands ───────────────────────────────────────────────────────────
arrests <- arrests |>
  mutate(
    age_at_offense = as.numeric(age_at_offense),
    age_at_arrest  = as.numeric(age_at_arrest),
    
    # Fill missing age_at_arrest from arrest_date + date_of_birth
    age_at_arrest = if_else(
      is.na(age_at_arrest) & !is.na(arrest_date) & !is.na(date_of_birth),
      as.numeric(time_length(interval(date_of_birth, arrest_date), "years")) |> floor(),
      age_at_arrest
    ),
    
    age_for_band = age_at_arrest  # Use age at arrest for consistency with NYPD data
  ) |>
  mutate(
    age_band_primary   = make_age_band_primary(age_for_band),
    age_band_secondary = make_age_band_secondary(age_for_band),
    year = year(arrest_date)
  ) %>%
  filter(year < 2026)

# ── Validation checks ───────────────────────────────────────────────────────
message("\n── Arrest data summary ──")
message("Total arrests: ", format(nrow(arrests), big.mark = ","))
message("Date range: ", min(arrests$arrest_date, na.rm = TRUE), " to ",
        max(arrests$arrest_date, na.rm = TRUE))

arrests |>
  summarize(
    n = n(),
    pct_felony      = mean(felony_flag, na.rm = TRUE),
    pct_vfo         = mean(vfo_flag, na.rm = TRUE),
    pct_gun         = mean(gun_flag, na.rm = TRUE),
    pct_serious_vfo = mean(serious_vfo_flag, na.rm = TRUE)
  ) |>
  print()

# Check <18 felony counts by year
message("\nUnder-18 felony arrests by year:")
arrests |>
  filter(felony_flag == TRUE, age_band_primary == "<18") |>
  count(year) |>
  print(n = 20)

# ── Save intermediate output ────────────────────────────────────────────────
saveRDS(arrests, here("data", "arrests_clean.rds"))
message("\nSaved: ", here("data", "arrests_clean.rds"))
