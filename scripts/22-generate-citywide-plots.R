# ============================================================================
# 22-generate-citywide-plots.R
# Generate citywide plots for the citywide report
# ============================================================================

source(here::here("scripts", "00-setup.R"))
source(here::here("scripts", "20-plot-functions.R"))

# ── Load computed tables ────────────────────────────────────────────────────
share_tables <- readRDS(here("data", "share_tables.rds"))
recid_tables <- readRDS(here("data", "recidivism_tables.rds"))

# ── Create output directory ─────────────────────────────────────────────────
plots_dir <- here("output", "plots")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# ── Storage for citywide plots ──────────────────────────────────────────────
citywide_plots <- list()

message("\n", strrep("=", 60))
message("Generating CITYWIDE plots")
message(strrep("=", 60))

# ============================================================================
# NYPD Long-run (2006-present) - Citywide
# ============================================================================

message("  NYPD long-run trend (citywide)...")

# Check if citywide NYPD data exists
if ("citywide_nypd_7major" %in% names(share_tables)) {
  citywide_plots$nypd_longrun <- plot_nypd_longrun(
    data = share_tables$citywide_nypd_7major,
    borough = "Citywide",
    age_group = "<18",
    title = "NYC Citywide: Under-18 Share of 7 Major Felony Arrests",
    save_path = here(plots_dir, "citywide_nypd_longrun.png")
  )
} else {
  message("    (citywide NYPD data not available - re-run 10-compute-shares.R)")
}

# ============================================================================
# CJA Share: Under-18 (Citywide)
# ============================================================================

message("  CJA share under-18 (citywide)...")

citywide_plots$cja_felony_u18 <- plot_cja_share_under18(
  data = share_tables$citywide_felony_primary,
  borough = "Citywide",
  offense_type = "Felony",
  title = "NYC Citywide: Under-18 Share of Felony Arrests",
  save_path = here(plots_dir, "citywide_cja_felony_u18.png")
)

citywide_plots$cja_vfo_u18 <- plot_cja_share_under18(
  data = share_tables$citywide_vfo_primary,
  borough = "Citywide",
  offense_type = "VFO",
  title = "NYC Citywide: Under-18 Share of VFO Arrests",
  save_path = here(plots_dir, "citywide_cja_vfo_u18.png")
)

citywide_plots$cja_serious_vfo_u18 <- plot_cja_share_under18(
  data = share_tables$citywide_serious_vfo_primary,
  borough = "Citywide",
  offense_type = "Serious VFO",
  title = "NYC Citywide: Under-18 Share of Serious VFO Arrests",
  subtitle = "Murder, Attempted Murder, Assault 1st, Robbery 1st",
  save_path = here(plots_dir, "citywide_cja_serious_vfo_u18.png")
)

citywide_plots$cja_gun_u18 <- plot_cja_share_under18(
  data = share_tables$citywide_gun_primary,
  borough = "Citywide",
  offense_type = "Gun (CPW 2nd)",
  title = "NYC Citywide: Under-18 Share of Gun Arrests",
  subtitle = "Criminal Possession of a Weapon 2nd Degree as Top Charge",
  save_path = here(plots_dir, "citywide_cja_gun_u18.png")
)

# ============================================================================
# CJA Share: Secondary bands (Citywide)
# ============================================================================

message("  CJA share secondary bands (citywide)...")

if ("citywide_felony_secondary" %in% names(share_tables)) {
  citywide_plots$cja_felony_secondary <- plot_secondary_lines(
    data = share_tables$citywide_felony_secondary,
    borough = "Citywide",
    value_var = "pct",
    offense_type = "Felony Arrest Share",
    metric_label = "Share of arrests",
    title = "NYC Citywide: Felony Arrest Share by Youth Age Group",
    save_path = here(plots_dir, "citywide_cja_felony_secondary.png")
  )
  
  citywide_plots$cja_vfo_secondary <- plot_secondary_lines(
    data = share_tables$citywide_vfo_secondary,
    borough = "Citywide",
    value_var = "pct",
    offense_type = "VFO Arrest Share",
    metric_label = "Share of arrests",
    title = "NYC Citywide: VFO Arrest Share by Youth Age Group",
    save_path = here(plots_dir, "citywide_cja_vfo_secondary.png")
  )
  
  citywide_plots$cja_serious_vfo_secondary <- plot_secondary_lines(
    data = share_tables$citywide_serious_vfo_secondary,
    borough = "Citywide",
    value_var = "pct",
    offense_type = "Serious VFO Arrest Share",
    metric_label = "Share of arrests",
    title = "NYC Citywide: Serious VFO Arrest Share by Youth Age Group",
    save_path = here(plots_dir, "citywide_cja_serious_vfo_secondary.png")
  )
  
  citywide_plots$cja_gun_secondary <- plot_secondary_lines(
    data = share_tables$citywide_gun_secondary,
    borough = "Citywide",
    value_var = "pct",
    offense_type = "Gun Arrest Share",
    metric_label = "Share of arrests",
    title = "NYC Citywide: Gun Arrest Share by Youth Age Group",
    save_path = here(plots_dir, "citywide_cja_gun_secondary.png")
  )
} else {
  message("    (citywide secondary data not available - re-run 10-compute-shares.R)")
}

# ============================================================================
# Recidivism: Under-18 (Citywide)
# ============================================================================

message("  Recidivism under-18 (citywide)...")

citywide_plots$recid_felony_u18 <- plot_recidivism_trend(
  data = recid_tables$citywide_felony_primary,
  borough = "Citywide",
  age_filter = "<18",
  offense_type = "Felony Index",
  title = "NYC Citywide: Under-18 Felony 365-Day Recidivism",
  save_path = here(plots_dir, "citywide_recid_felony_u18.png")
)

citywide_plots$recid_vfo_u18 <- plot_recidivism_trend(
  data = recid_tables$citywide_vfo_primary,
  borough = "Citywide",
  age_filter = "<18",
  offense_type = "VFO Index",
  title = "NYC Citywide: Under-18 VFO 365-Day Recidivism",
  save_path = here(plots_dir, "citywide_recid_vfo_u18.png")
)

citywide_plots$recid_serious_vfo_u18 <- plot_recidivism_trend(
  data = recid_tables$citywide_serious_vfo_primary,
  borough = "Citywide",
  age_filter = "<18",
  offense_type = "Serious VFO Index",
  title = "NYC Citywide: Under-18 Serious VFO 365-Day Recidivism",
  save_path = here(plots_dir, "citywide_recid_serious_vfo_u18.png")
)

citywide_plots$recid_gun_u18 <- plot_recidivism_trend(
  data = recid_tables$citywide_gun_primary,
  borough = "Citywide",
  age_filter = "<18",
  offense_type = "Gun Index",
  title = "NYC Citywide: Under-18 Gun 365-Day Recidivism",
  save_path = here(plots_dir, "citywide_recid_gun_u18.png")
)

# ============================================================================
# Recidivism: Secondary bands (Citywide)
# ============================================================================

message("  Recidivism secondary bands (citywide)...")

if ("citywide_felony_secondary" %in% names(recid_tables)) {
  citywide_plots$recid_felony_secondary <- plot_secondary_lines(
    data = recid_tables$citywide_felony_secondary,
    borough = "Citywide",
    value_var = "rate",
    offense_type = "Felony 365-Day Recidivism",
    metric_label = "Recidivism rate",
    title = "NYC Citywide: Felony Recidivism by Youth Age Group",
    show_n = TRUE,
    n_var = "n_eligible",
    save_path = here(plots_dir, "citywide_recid_felony_secondary.png")
  )
  
  citywide_plots$recid_vfo_secondary <- plot_secondary_lines(
    data = recid_tables$citywide_vfo_secondary,
    borough = "Citywide",
    value_var = "rate",
    offense_type = "VFO 365-Day Recidivism",
    metric_label = "Recidivism rate",
    title = "NYC Citywide: VFO Recidivism by Youth Age Group",
    show_n = TRUE,
    n_var = "n_eligible",
    save_path = here(plots_dir, "citywide_recid_vfo_secondary.png")
  )
  
  citywide_plots$recid_serious_vfo_secondary <- plot_secondary_lines(
    data = recid_tables$citywide_serious_vfo_secondary,
    borough = "Citywide",
    value_var = "rate",
    offense_type = "Serious VFO 365-Day Recidivism",
    metric_label = "Recidivism rate",
    title = "NYC Citywide: Serious VFO Recidivism by Youth Age Group",
    show_n = TRUE,
    n_var = "n_eligible",
    save_path = here(plots_dir, "citywide_recid_serious_vfo_secondary.png")
  )
  
  citywide_plots$recid_gun_secondary <- plot_secondary_lines(
    data = recid_tables$citywide_gun_secondary,
    borough = "Citywide",
    value_var = "rate",
    offense_type = "Gun 365-Day Recidivism",
    metric_label = "Recidivism rate",
    title = "NYC Citywide: Gun Recidivism by Youth Age Group",
    show_n = TRUE,
    n_var = "n_eligible",
    save_path = here(plots_dir, "citywide_recid_gun_secondary.png")
  )
} else {
  message("    (citywide secondary recid data not available - re-run 11-compute-recidivism.R)")
}

# ============================================================================
# SAVE CITYWIDE PLOTS
# ============================================================================

# Load existing plots and add citywide
if (file.exists(here("output", "all_plots.rds"))) {
  all_plots <- readRDS(here("output", "all_plots.rds"))
} else {
  all_plots <- list()
}

all_plots[["Citywide"]] <- citywide_plots

saveRDS(all_plots, here("output", "all_plots.rds"))
message("\nUpdated: ", here("output", "all_plots.rds"))

# ── Summary ─────────────────────────────────────────────────────────────────
message("\n", strrep("=", 60))
message("CITYWIDE PLOT GENERATION COMPLETE")
message(strrep("=", 60))
message("PNG files saved to: ", plots_dir)
message("\nTo access citywide plots:")
message('  all_plots <- readRDS(here("output", "all_plots.rds"))')
message('  all_plots$Citywide$cja_felony_u18')
