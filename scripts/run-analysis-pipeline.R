# ============================================================================
# run-analysis-pipeline.R
# Master script to run all analysis and generate reports
# ============================================================================

library(here)

# ── Configuration ────────────────────────────────────────────────────────────
# Set to FALSE to use cached data when available (faster for iterating on reports)
# Set to TRUE to regenerate everything from scratch
FORCE_REBUILD <- TRUE

message("=" |> strrep(70))
message("JUVENILE ARRESTS & RECIDIVISM - BOROUGH REPORT PIPELINE")
message("=" |> strrep(70))
message("Project root: ", here())
message("Force rebuild: ", FORCE_REBUILD)
message("Started: ", Sys.time())
message("=" |> strrep(70), "\n")

# ============================================================================
# STEP 1: Data Preparation
# ============================================================================

message("\n[STEP 1] Data preparation...")

if (FORCE_REBUILD || !file.exists(here("data", "arrests_clean.rds"))) {
  message("  Running 01-load-cja.R...")
  source(here("scripts", "01-load-cja.R"))
} else {
  message("  ✓ CJA data already prepared (skipping)")
}

if (FORCE_REBUILD || !file.exists(here("data", "survival_data.rds"))) {
  message("  Running 02-build-survival.R...")
  source(here("scripts", "02-build-survival.R"))
} else {
  message("  ✓ Survival data already prepared (skipping)")
}

if (FORCE_REBUILD || !file.exists(here("data", "nypd_arrests_clean.rds"))) {
  message("  Running 03-load-nypd.R...")
  source(here("scripts", "03-load-nypd.R"))
} else {
  message("  ✓ NYPD data already prepared (skipping)")
}

# ============================================================================
# STEP 2: Compute Analysis Tables
# ============================================================================

message("\n[STEP 2] Computing analysis tables...")

if (FORCE_REBUILD || !file.exists(here("data", "share_tables.rds"))) {
  message("  Running 10-compute-shares.R...")
  source(here("scripts", "10-compute-shares.R"))
} else {
  message("  ✓ Share tables already computed (skipping)")
}

if (FORCE_REBUILD || !file.exists(here("data", "recidivism_tables.rds"))) {
  message("  Running 11-compute-recidivism.R...")
  source(here("scripts", "11-compute-recidivism.R"))
} else {
  message("  ✓ Recidivism tables already computed (skipping)")
}

# ============================================================================
# STEP 3: Generate Plots
# ============================================================================

message("\n[STEP 3] Generating plots...")

if (FORCE_REBUILD || !file.exists(here("output", "all_plots.rds"))) {
  message("  Running 21-generate-borough-plots.R...")
  source(here("scripts", "21-generate-borough-plots.R"))
  message("  Running 22-generate-citywide-plots.R...")
  source(here("scripts", "22-generate-citywide-plots.R"))
} else {
  # Check if citywide plots exist even if file exists
  all_plots <- readRDS(here("output", "all_plots.rds"))
  if (!"Citywide" %in% names(all_plots)) {
    message("  Running 22-generate-citywide-plots.R...")
    source(here("scripts", "22-generate-citywide-plots.R"))
  } else {
    message("  ✓ All plots already generated (skipping)")
  }
}

# ============================================================================
# STEP 4: Render Reports
# ============================================================================

message("\n[STEP 4] Rendering PDF reports...")
source(here("scripts", "30-render-reports.R"))

# ============================================================================
# STEP 5: Render Citywide Report
# ============================================================================

message("\n[STEP 5] Rendering citywide report...")
source(here("scripts", "31-render-citywide-report.R"))

# ============================================================================
# SUMMARY
# ============================================================================

message("\n", "=" |> strrep(70))
message("PIPELINE COMPLETE")
message("=" |> strrep(70))
message("Finished: ", Sys.time())

message("\nOutputs:")
message("  Data:    ", here("data"))
message("  Plots:   ", here("output", "plots"))
message("  Reports: ", here("output", "reports"))

message("\nTo use cached data (faster):")
message('  Set FORCE_REBUILD <- FALSE at top of script')

message("\nTo regenerate everything:")
message('  Set FORCE_REBUILD <- TRUE at top of script (default)')
