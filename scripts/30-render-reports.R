# ============================================================================
# 30-render-reports.R
# Render PDF reports for each borough
# ============================================================================

library(here)
library(rmarkdown)

# ── Configuration ───────────────────────────────────────────────────────────
boroughs <- c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")
template_path <- here("reports", "borough-report.Rmd")
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

# ── Render each borough ─────────────────────────────────────────────────────
message("\n", strrep("=", 60))
message("RENDERING BOROUGH REPORTS")
message(strrep("=", 60))

for (boro in boroughs) {
  
  boro_slug <- tolower(gsub(" ", "_", boro))
  output_file <- paste0("juvenile_report_", boro_slug, ".pdf")
  
  message("\nRendering: ", boro, " → ", output_file)
  
  tryCatch({
    rmarkdown::render(
      input = template_path,
      output_file = output_file,
      output_dir = output_dir,
      params = list(borough = boro),
      envir = new.env(),
      quiet = TRUE
    )
    message("  ✓ Success: ", file.path(output_dir, output_file))
  }, error = function(e) {
    message("  ✗ Failed: ", e$message)
  })
}

# ── Summary ─────────────────────────────────────────────────────────────────
message("\n", strrep("=", 60))
message("REPORT GENERATION COMPLETE")
message(strrep("=", 60))

# List generated reports
reports <- list.files(output_dir, pattern = "\\.pdf$", full.names = TRUE)
message("\nGenerated reports:")
for (r in reports) {
  message("  - ", basename(r))
}

message("\nOutput directory: ", output_dir)

