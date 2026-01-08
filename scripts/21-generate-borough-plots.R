# ============================================================================
# 21-generate-borough-plots.R
# Generate all plots for each borough, save as RDS (ggplot objects) + PNG
# ============================================================================

source(here::here("scripts", "00-setup.R"))
source(here::here("scripts", "20-plot-functions.R"))

# ── Load computed tables ────────────────────────────────────────────────────
share_tables <- readRDS(here("data", "share_tables.rds"))
recid_tables <- readRDS(here("data", "recidivism_tables.rds"))

# ── Create output directories ───────────────────────────────────────────────
plots_dir <- here("output", "plots")
if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)

# ── Borough list ────────────────────────────────────────────────────────────
boroughs <- c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")

# ── Storage for all plot objects ────────────────────────────────────────────
all_plots <- list()

# ============================================================================
# GENERATE PLOTS FOR EACH BOROUGH
# ============================================================================

for (boro in boroughs) {
 
 message("\n", strrep("=", 60))
 message("Generating plots for: ", boro)
 message(strrep("=", 60))
 
 boro_slug <- tolower(gsub(" ", "_", boro))
 boro_plots <- list()
 
 # ── NYPD Long-run (2006-present) ─────────────────────────────────────────
 message("  NYPD long-run trend...")
 boro_plots$nypd_longrun <- plot_nypd_longrun(
   data = share_tables$nypd_7major,
   borough = boro,
   age_group = "<18",
   save_path = here(plots_dir, paste0(boro_slug, "_nypd_longrun.png"))
 )
 
 # ── CJA Share: Under-18 ─────────────────────────────────────────────────
 message("  CJA share under-18 (felony, VFO, serious VFO, gun)...")
 
 boro_plots$cja_felony_u18 <- plot_cja_share_under18(
   data = share_tables$cja_felony_primary,
   borough = boro,
   offense_type = "Felony",
   save_path = here(plots_dir, paste0(boro_slug, "_cja_felony_u18.png"))
 )
 
 boro_plots$cja_vfo_u18 <- plot_cja_share_under18(
   data = share_tables$cja_vfo_primary,
   borough = boro,
   offense_type = "VFO",
   save_path = here(plots_dir, paste0(boro_slug, "_cja_vfo_u18.png"))
 )
 
 boro_plots$cja_serious_vfo_u18 <- plot_cja_share_under18(
   data = share_tables$cja_serious_vfo_primary,
   borough = boro,
   offense_type = "Serious VFO",
   subtitle = "Murder, Attempted Murder, Assault 1st, Robbery 1st",
   save_path = here(plots_dir, paste0(boro_slug, "_cja_serious_vfo_u18.png"))
 )
 
 boro_plots$cja_gun_u18 <- plot_cja_share_under18(
   data = share_tables$cja_gun_primary,
   borough = boro,
   offense_type = "Gun (CPW 2nd)",
   subtitle = "Criminal Possession of a Weapon 2nd Degree as Top Charge",
   save_path = here(plots_dir, paste0(boro_slug, "_cja_gun_u18.png"))
 )
 
 # ── CJA Share: Secondary age bands ──────────────────────────────────────
 message("  CJA share secondary bands (13-15, 16-17, 18-19)...")
 
 boro_plots$cja_felony_secondary <- plot_secondary_lines(
   data = share_tables$cja_felony_secondary,
   borough = boro,
   value_var = "pct",
   offense_type = "Felony Arrest Share",
   metric_label = "Share of arrests",
   save_path = here(plots_dir, paste0(boro_slug, "_cja_felony_secondary.png"))
 )
 
 boro_plots$cja_vfo_secondary <- plot_secondary_lines(
   data = share_tables$cja_vfo_secondary,
   borough = boro,
   value_var = "pct",
   offense_type = "VFO Arrest Share",
   metric_label = "Share of arrests",
   save_path = here(plots_dir, paste0(boro_slug, "_cja_vfo_secondary.png"))
 )
 
 boro_plots$cja_serious_vfo_secondary <- plot_secondary_lines(
   data = share_tables$cja_serious_vfo_secondary,
   borough = boro,
   value_var = "pct",
   offense_type = "Serious VFO Arrest Share",
   metric_label = "Share of arrests",
   save_path = here(plots_dir, paste0(boro_slug, "_cja_serious_vfo_secondary.png"))
 )
 
 boro_plots$cja_gun_secondary <- plot_secondary_lines(
   data = share_tables$cja_gun_secondary,
   borough = boro,
   value_var = "pct",
   offense_type = "Gun Arrest Share",
   metric_label = "Share of arrests",
   save_path = here(plots_dir, paste0(boro_slug, "_cja_gun_secondary.png"))
 )
 
 # ── Recidivism: Under-18 ────────────────────────────────────────────────
 message("  Recidivism under-18...")
 
 boro_plots$recid_felony_u18 <- plot_recidivism_trend(
   data = recid_tables$felony_primary,
   borough = boro,
   age_filter = "<18",
   offense_type = "Felony Index",
   save_path = here(plots_dir, paste0(boro_slug, "_recid_felony_u18.png"))
 )
 
 boro_plots$recid_vfo_u18 <- plot_recidivism_trend(
   data = recid_tables$vfo_primary,
   borough = boro,
   age_filter = "<18",
   offense_type = "VFO Index",
   save_path = here(plots_dir, paste0(boro_slug, "_recid_vfo_u18.png"))
 )
 
 boro_plots$recid_serious_vfo_u18 <- plot_recidivism_trend(
   data = recid_tables$serious_vfo_primary,
   borough = boro,
   age_filter = "<18",
   offense_type = "Serious VFO Index",
   save_path = here(plots_dir, paste0(boro_slug, "_recid_serious_vfo_u18.png"))
 )
 
 boro_plots$recid_gun_u18 <- plot_recidivism_trend(
   data = recid_tables$gun_primary,
   borough = boro,
   age_filter = "<18",
   offense_type = "Gun Index",
   save_path = here(plots_dir, paste0(boro_slug, "_recid_gun_u18.png"))
 )
 
 # ── Recidivism: Secondary bands ─────────────────────────────────────────
 message("  Recidivism secondary bands...")
 
 boro_plots$recid_felony_secondary <- plot_secondary_lines(
   data = recid_tables$felony_secondary,
   borough = boro,
   value_var = "rate",
   offense_type = "Felony 365-Day Recidivism",
   metric_label = "Recidivism rate",
   show_n = TRUE,
   n_var = "n_eligible",
   save_path = here(plots_dir, paste0(boro_slug, "_recid_felony_secondary.png"))
 )
 
 boro_plots$recid_vfo_secondary <- plot_secondary_lines(
   data = recid_tables$vfo_secondary,
   borough = boro,
   value_var = "rate",
   offense_type = "VFO 365-Day Recidivism",
   metric_label = "Recidivism rate",
   show_n = TRUE,
   n_var = "n_eligible",
   save_path = here(plots_dir, paste0(boro_slug, "_recid_vfo_secondary.png"))
 )
 
 boro_plots$recid_serious_vfo_secondary <- plot_secondary_lines(
   data = recid_tables$serious_vfo_secondary,
   borough = boro,
   value_var = "rate",
   offense_type = "Serious VFO 365-Day Recidivism",
   metric_label = "Recidivism rate",
   show_n = TRUE,
   n_var = "n_eligible",
   save_path = here(plots_dir, paste0(boro_slug, "_recid_serious_vfo_secondary.png"))
 )
 
 boro_plots$recid_gun_secondary <- plot_secondary_lines(
   data = recid_tables$gun_secondary,
   borough = boro,
   value_var = "rate",
   offense_type = "Gun 365-Day Recidivism",
   metric_label = "Recidivism rate",
   show_n = TRUE,
   n_var = "n_eligible",
   save_path = here(plots_dir, paste0(boro_slug, "_recid_gun_secondary.png"))
 )
 
 # Store in master list
 all_plots[[boro]] <- boro_plots
 
 message("  Done with ", boro)
}

# ============================================================================
# CROSS-BOROUGH COMPARISON PLOTS
# ============================================================================

message("\n", strrep("=", 60))
message("Generating cross-borough comparison plots...")
message(strrep("=", 60))

comparison_plots <- list()

# Get the most recent full year
latest_year <- max(share_tables$cja_felony_primary$year)
latest_recid_year <- recid_tables$metadata$last_full_year

# Combine borough + citywide data for comparisons
combined_felony <- bind_rows(
 share_tables$cja_felony_primary,
 share_tables$citywide_felony_primary
)

combined_vfo <- bind_rows(
 share_tables$cja_vfo_primary,
 share_tables$citywide_vfo_primary
)

combined_serious_vfo <- bind_rows(
 share_tables$cja_serious_vfo_primary,
 share_tables$citywide_serious_vfo_primary
)

combined_gun <- bind_rows(
 share_tables$cja_gun_primary,
 share_tables$citywide_gun_primary
)

combined_recid_felony <- bind_rows(
 recid_tables$felony_primary,
 recid_tables$citywide_felony_primary
)

combined_recid_gun <- bind_rows(
 recid_tables$gun_primary,
 recid_tables$citywide_gun_primary
)

# Share comparisons
comparison_plots$felony_share <- plot_borough_comparison(
 data = combined_felony,
 year_filter = latest_year,
 age_filter = "<18",
 value_var = "pct",
 metric_label = "Share of Felony Arrests",
 title = paste0("Under-18 Share of Felony Arrests by Borough (", latest_year, ")"),
 save_path = here(plots_dir, "comparison_felony_share.png")
)

comparison_plots$vfo_share <- plot_borough_comparison(
 data = combined_vfo,
 year_filter = latest_year,
 age_filter = "<18",
 value_var = "pct",
 metric_label = "Share of VFO Arrests",
 title = paste0("Under-18 Share of VFO Arrests by Borough (", latest_year, ")"),
 save_path = here(plots_dir, "comparison_vfo_share.png")
)

comparison_plots$serious_vfo_share <- plot_borough_comparison(
 data = combined_serious_vfo,
 year_filter = latest_year,
 age_filter = "<18",
 value_var = "pct",
 metric_label = "Share of Serious VFO Arrests",
 title = paste0("Under-18 Share of Serious VFO Arrests by Borough (", latest_year, ")"),
 save_path = here(plots_dir, "comparison_serious_vfo_share.png")
)

comparison_plots$gun_share <- plot_borough_comparison(
 data = combined_gun,
 year_filter = latest_year,
 age_filter = "<18",
 value_var = "pct",
 metric_label = "Share of Gun Arrests",
 title = paste0("Under-18 Share of Gun Arrests by Borough (", latest_year, ")"),
 save_path = here(plots_dir, "comparison_gun_share.png")
)

# Recidivism comparisons
comparison_plots$recid_felony <- plot_borough_comparison(
 data = combined_recid_felony,
 year_filter = latest_recid_year,
 age_filter = "<18",
 value_var = "rate",
 metric_label = "365-Day Recidivism Rate",
 title = paste0("Under-18 Felony Recidivism by Borough (", latest_recid_year, ")"),
 save_path = here(plots_dir, "comparison_recid_felony.png")
)

comparison_plots$recid_gun <- plot_borough_comparison(
 data = combined_recid_gun,
 year_filter = latest_recid_year,
 age_filter = "<18",
 value_var = "rate",
 metric_label = "365-Day Recidivism Rate",
 title = paste0("Under-18 Gun Recidivism by Borough (", latest_recid_year, ")"),
 save_path = here(plots_dir, "comparison_recid_gun.png")
)

all_plots[["comparison"]] <- comparison_plots

# ============================================================================
# SAVE ALL PLOT OBJECTS
# ============================================================================

saveRDS(all_plots, here("output", "all_plots.rds"))
message("\nSaved all plot objects: ", here("output", "all_plots.rds"))

# ── Summary ─────────────────────────────────────────────────────────────────
message("\n", strrep("=", 60))
message("PLOT GENERATION COMPLETE")
message(strrep("=", 60))
message("PNG files saved to: ", plots_dir)
message("Plot objects saved to: ", here("output", "all_plots.rds"))
message("\nTo load plots in R:")
message('  all_plots <- readRDS(here("output", "all_plots.rds"))')
message('  all_plots$Manhattan$cja_felony_u18  # access individual plot')
