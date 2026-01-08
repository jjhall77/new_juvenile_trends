# ============================================================================
# 10-compute-shares.R
# Compute share-of-arrests tables by borough for all offense types
# Outputs: RDS files with tidy tables ready for plotting/reporting
# ============================================================================

source(here::here("scripts", "00-setup.R"))

# ── Load cleaned data ───────────────────────────────────────────────────────
arrests      <- readRDS(here("data", "arrests_clean.rds"))
nypd_arrests <- readRDS(here("data", "nypd_arrests_clean.rds"))

# ============================================================================
# CITYWIDE TOTALS - COMPUTE FIRST (before any borough filtering)
# ============================================================================

message("Computing citywide totals (all records)...")

# Filter to valid records for share analysis (need year and age)
arrests_valid <- arrests |>
  filter(!is.na(year), !is.na(age_at_arrest))

message("  Valid arrests for share analysis: ", format(nrow(arrests_valid), big.mark = ","))

# Citywide primary bands
citywide_felony_share_primary <- arrests_valid |>
  filter(felony_flag == TRUE, !is.na(age_band_primary)) |>
  count(year, age_band_primary, name = "n") |>
  group_by(year) |>
  mutate(total = sum(n), pct = n / total) |>
  ungroup() |>
  mutate(boro_name = "Citywide")

citywide_vfo_share_primary <- arrests_valid |>
  filter(vfo_flag == TRUE, !is.na(age_band_primary)) |>
  count(year, age_band_primary, name = "n") |>
  group_by(year) |>
  mutate(total = sum(n), pct = n / total) |>
  ungroup() |>
  mutate(boro_name = "Citywide")

citywide_serious_vfo_share_primary <- arrests_valid |>
  filter(serious_vfo_flag == TRUE, !is.na(age_band_primary)) |>
  count(year, age_band_primary, name = "n") |>
  group_by(year) |>
  mutate(total = sum(n), pct = n / total) |>
  ungroup() |>
  mutate(boro_name = "Citywide")

citywide_gun_share_primary <- arrests_valid |>
  filter(gun_flag == TRUE, !is.na(age_band_primary)) |>
  count(year, age_band_primary, name = "n") |>
  group_by(year) |>
  mutate(total = sum(n), pct = n / total) |>
  ungroup() |>
  mutate(boro_name = "Citywide")

# Citywide secondary bands
citywide_felony_share_secondary <- arrests_valid |>
  filter(felony_flag == TRUE, !is.na(age_band_secondary)) |>
  count(year, age_band_secondary, name = "n") |>
  group_by(year) |>
  mutate(total = sum(n), pct = n / total) |>
  ungroup() |>
  mutate(boro_name = "Citywide")

citywide_vfo_share_secondary <- arrests_valid |>
  filter(vfo_flag == TRUE, !is.na(age_band_secondary)) |>
  count(year, age_band_secondary, name = "n") |>
  group_by(year) |>
  mutate(total = sum(n), pct = n / total) |>
  ungroup() |>
  mutate(boro_name = "Citywide")

citywide_serious_vfo_share_secondary <- arrests_valid |>
  filter(serious_vfo_flag == TRUE, !is.na(age_band_secondary)) |>
  count(year, age_band_secondary, name = "n") |>
  group_by(year) |>
  mutate(total = sum(n), pct = n / total) |>
  ungroup() |>
  mutate(boro_name = "Citywide")

citywide_gun_share_secondary <- arrests_valid |>
  filter(gun_flag == TRUE, !is.na(age_band_secondary)) |>
  count(year, age_band_secondary, name = "n") |>
  group_by(year) |>
  mutate(total = sum(n), pct = n / total) |>
  ungroup() |>
  mutate(boro_name = "Citywide")

# Citywide NYPD (no borough filter needed - use all NYPD records)
citywide_nypd_7major <- nypd_arrests |>
  filter(is_seven_major == TRUE, !is.na(age_group)) |>
  count(year, age_group, name = "n") |>
  group_by(year) |>
  mutate(total = sum(n), pct = n / total) |>
  ungroup() |>
  mutate(boro_name = "Citywide")

# Quick check: citywide under-18 felony counts
message("  Citywide <18 felony check:")
citywide_felony_share_primary |>
  filter(age_band_primary == "<18") |>
  select(year, n) |>
  print(n = 10)

message("  Citywide <18 gun check:")
citywide_gun_share_primary |>
  filter(age_band_primary == "<18") |>
  select(year, n) |>
  print(n = 10)

rm(arrests_valid)

# ============================================================================
# BOROUGH-LEVEL ANALYSIS - Now add borough codes and filter
# ============================================================================

message("\nPreparing borough-level data...")

# ── Add borough to CJA arrests ──────────────────────────────────────────────
arrests <- arrests |>
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
n_no_boro <- sum(is.na(arrests$boro_name))
message("  Records without valid borough code: ", format(n_no_boro, big.mark = ","))

# Now filter for borough-level analysis
arrests <- arrests |>
  filter(!is.na(boro_name))

message("  Records with valid borough: ", format(nrow(arrests), big.mark = ","))

# ── Add borough to NYPD arrests ─────────────────────────────────────────────
nypd_arrests <- nypd_arrests |>
  mutate(
    boro_name = case_match(
      arrest_boro,
      "M" ~ "Manhattan",
      "K" ~ "Brooklyn",
      "Q" ~ "Queens",
      "B" ~ "Bronx",
      "S" ~ "Staten Island",
      .default = NA_character_
    )
  ) |>
  filter(!is.na(boro_name))

# ============================================================================
# HELPER: Compute share table for a given filter
# ============================================================================

compute_share_table <- function(data, filter_expr, age_var, group_var = "year") {
 
 age_sym <- rlang::sym(age_var)
 
 data |>
   filter({{ filter_expr }}) |>
   filter(!is.na(!!age_sym)) |>
   count(boro_name, .data[[group_var]], !!age_sym, name = "n") |>
   group_by(boro_name, .data[[group_var]]) |>
   mutate(
     total = sum(n),
     pct = n / total
   ) |>
   ungroup() |>
   arrange(boro_name, .data[[group_var]], !!age_sym)
}

# ============================================================================
# CJA SHARES: Primary age bands (<18, 18-24, etc.)
# ============================================================================

message("Computing CJA share tables (primary bands)...")

# All felonies
cja_felony_share_primary <- compute_share_table(
 arrests,
 felony_flag == TRUE,
 "age_band_primary"
)

# VFOs
cja_vfo_share_primary <- compute_share_table(
 arrests,
 vfo_flag == TRUE,
 "age_band_primary"
)

# Serious VFOs
cja_serious_vfo_share_primary <- compute_share_table(
 arrests,
 serious_vfo_flag == TRUE,
 "age_band_primary"
)

# Gun arrests (CPW 2nd as top charge)
cja_gun_share_primary <- compute_share_table(
 arrests,
 gun_flag == TRUE,
 "age_band_primary"
)

# ============================================================================
# CJA SHARES: Secondary age bands (13-15, 16-17, 18-19, 20+)
# ============================================================================

message("Computing CJA share tables (secondary bands)...")

cja_felony_share_secondary <- compute_share_table(
 arrests,
 felony_flag == TRUE,
 "age_band_secondary"
)

cja_vfo_share_secondary <- compute_share_table(
 arrests,
 vfo_flag == TRUE,
 "age_band_secondary"
)

cja_serious_vfo_share_secondary <- compute_share_table(
 arrests,
 serious_vfo_flag == TRUE,
 "age_band_secondary"
)

cja_gun_share_secondary <- compute_share_table(
 arrests,
 gun_flag == TRUE,
 "age_band_secondary"
)

# ============================================================================
# NYPD SHARES: 7 Major felonies (long-run, 2006+)
# ============================================================================

message("Computing NYPD share tables...")

nypd_7major_share <- nypd_arrests |>
 filter(is_seven_major == TRUE, !is.na(age_group)) |>
 count(boro_name, year, age_group, name = "n") |>
 group_by(boro_name, year) |>
 mutate(
   total = sum(n),
   pct = n / total
 ) |>
 ungroup() |>
 arrange(boro_name, year, age_group)

# ============================================================================
# SAVE ALL SHARE TABLES
# ============================================================================

share_tables <- list(
 # CJA primary
 cja_felony_primary      = cja_felony_share_primary,
 cja_vfo_primary         = cja_vfo_share_primary,
 cja_serious_vfo_primary = cja_serious_vfo_share_primary,
 cja_gun_primary         = cja_gun_share_primary,
 
 # CJA secondary
 cja_felony_secondary      = cja_felony_share_secondary,
 cja_vfo_secondary         = cja_vfo_share_secondary,
 cja_serious_vfo_secondary = cja_serious_vfo_share_secondary,
 cja_gun_secondary         = cja_gun_share_secondary,
 
 # NYPD long-run
 nypd_7major = nypd_7major_share,
 
 # Citywide primary (for comparison plots)
 citywide_felony_primary      = citywide_felony_share_primary,
 citywide_vfo_primary         = citywide_vfo_share_primary,
 citywide_serious_vfo_primary = citywide_serious_vfo_share_primary,
 citywide_gun_primary         = citywide_gun_share_primary,
 
 # Citywide secondary
 citywide_felony_secondary      = citywide_felony_share_secondary,
 citywide_vfo_secondary         = citywide_vfo_share_secondary,
 citywide_serious_vfo_secondary = citywide_serious_vfo_share_secondary,
 citywide_gun_secondary         = citywide_gun_share_secondary,
 
 # Citywide NYPD
 citywide_nypd_7major = citywide_nypd_7major
)

saveRDS(share_tables, here("data", "share_tables.rds"))
message("\nSaved: ", here("data", "share_tables.rds"))

# ── Summary ─────────────────────────────────────────────────────────────────
message("\n── Share tables summary ──")
message("Boroughs: ", paste(unique(cja_felony_share_primary$boro_name), collapse = ", "))
message("CJA years: ", min(arrests$year, na.rm = TRUE), "-", max(arrests$year, na.rm = TRUE))
message("NYPD years: ", min(nypd_arrests$year, na.rm = TRUE), "-", max(nypd_arrests$year, na.rm = TRUE))
