# ============================================================================
# 00-setup.R
# Project setup: packages, paths, helper functions
# ============================================================================

# ── Packages ────────────────────────────────────────────────────────────────
library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(data.table)

# Suppress readr column type messages globally
options(readr.show_col_types = FALSE)

# ── Project paths (using here) ──────────────────────────────────────────────
paths <- list(
  data     = here("data"),
  output   = here("output"),
  scripts  = here("scripts")
)

list.files(here("data"))

# Create output directory if needed
if (!dir.exists(paths$output)) dir.create(paths$output, recursive = TRUE)

# ── Data file paths ─────────────────────────────────────────────────────────
# CJA data (single arrest file - gun arrests identified via ms_charge_art_sec)
# NYPD open data files
files <- list(
  cja_arrests  = here("data", "cja_arrests.csv"),
  nypd_ytd     = here("data", "NYPD_Arrest_Data_(Year_to_Date)_20260105.csv"),
  nypd_hist    = here("data", "NYPD_Arrests_Data_(Historic)_20260105.csv")
)

# ── Helper functions ────────────────────────────────────────────────────────

#' Convert "NULL" strings to NA across all character columns
null_to_na <- function(df) {
  df |>
    mutate(across(where(is.character), \(x) na_if(x, "NULL")))
}

#' Create primary age bands (<18, 18-24, 25-29, 30-34, 35-44, 45-54, 55+)
make_age_band_primary <- function(age) {
  cut(
    age,
    breaks = c(-Inf, 18, 25, 30, 35, 45, 55, Inf),
    labels = c("<18", "18-24", "25-29", "30-34", "35-44", "45-54", "55+"),
    right = FALSE
  )
}

#' Create secondary age bands for youth focus (13-15, 16-17, 18-19, 20+)
make_age_band_secondary <- function(age) {
  cut(
    age,
    breaks = c(13, 16, 18, 20, Inf),
    labels = c("13-15", "16-17", "18-19", "20+"),
    right = FALSE,
    include.lowest = TRUE
  )
}

#' Charge severity ranking (for same-day collapse)
charge_rank_levels <- c("VFO", "Felony Non-VFO", "Misdemeanor", "VI", "Unknown", NA_character_)

# ── NYPD offense codes ──────────────────────────────────────────────────────
nypd_codes <- list(
  seven_major = c(101L, 104L, 105L, 106L, 107L, 109L, 110L),
  violent     = c(101L, 104L, 105L, 106L)
)

# ── Status message ──────────────────────────────────────────────────────────
message("Setup complete. Project root: ", here())

