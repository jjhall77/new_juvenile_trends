# ============================================================================
# 31-render-citywide-report.R
# Render the citywide PDF report
# ============================================================================

library(here)
library(rmarkdown)

# ── Configuration ───────────────────────────────────────────────────────────
template_path <- here("reports", "citywide-report.Rmd")
output_dir <- here("output", "reports")

# Create output directory
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ── Verify dependencies exist ───────────────────────────────────────────────
required_files <- c(
 here("data", "share_tables.rds"),
 here("data", "recidivism_tables.rds"),
 here("output", "all_plots.rds")
)

missing <- required_files[!file.exists(required_files)]
if (length(missing) > 0) {
 stop("Missing required files. Run the pipeline first:\n",
      paste0("  - ", missing, collapse = "\n"),
      "\n\nRun: source(here('scripts', 'run-analysis-pipeline.R'))")
}

# Check if citywide plots exist
all_plots <- readRDS(here("output", "all_plots.rds"))
if (!"Citywide" %in% names(all_plots)) {
 message("Citywide plots not found. Generating...")
 source(here("scripts", "22-generate-citywide-plots.R"))
}

# ── Render citywide report ──────────────────────────────────────────────────
message("\n", strrep("=", 60))
message("RENDERING CITYWIDE REPORT")
message(strrep("=", 60))

output_file <- "juvenile_report_citywide.pdf"
message("\nRendering: Citywide → ", output_file)

tryCatch({
 rmarkdown::render(
   input = template_path,
   output_file = output_file,
   output_dir = output_dir,
   envir = new.env(),
   quiet = TRUE
 )
 message("  ✓ Success: ", file.path(output_dir, output_file))
}, error = function(e) {
 message("  ✗ Failed: ", e$message)
})

# ── Summary ─────────────────────────────────────────────────────────────────
message("\n", strrep("=", 60))
message("CITYWIDE REPORT COMPLETE")
message(strrep("=", 60))
message("\nOutput: ", file.path(output_dir, output_file))
