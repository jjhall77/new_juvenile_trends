# Juvenile Arrests & Recidivism: Borough Report Pipeline

## Overview

This project generates publication-quality PDF reports on juvenile felony arrests and recidivism for each NYC borough (District Attorney). The architecture separates data preparation, analysis computation, plot generation, and report rendering for modularity and extensibility.

**Key Features:**
- Supports 2015-2025 CJA data (or any year range present in your data)
- Gun arrests identified via `ms_charge_art_sec` field (no separate charges file needed)
- Auto-detects year ranges from data
- Generates both borough-specific and citywide reports

## Project Structure

```
project/
├── data/
│   ├── Arrest20250613.csv              # CJA arrests (raw)
│   ├── NYPD_Arrest_Data__Year_to_Date__*.csv
│   ├── NYPD_Arrests_Data__Historic__*.csv
│   │
│   ├── arrests_clean.rds               # Cleaned CJA arrests
│   ├── survival_data.rds               # Recidivism-ready data
│   ├── nypd_arrests_clean.rds          # Cleaned NYPD data
│   ├── share_tables.rds                # Pre-computed share tables
│   └── recidivism_tables.rds           # Pre-computed recidivism tables
│
├── output/
│   ├── plots/                          # PNG files for each borough + citywide
│   │   ├── manhattan_nypd_longrun.png
│   │   ├── citywide_nypd_longrun.png
│   │   └── ...
│   ├── reports/                        # Generated PDFs
│   │   ├── juvenile_report_manhattan.pdf
│   │   ├── juvenile_report_brooklyn.pdf
│   │   ├── juvenile_report_citywide.pdf
│   │   └── ...
│   └── all_plots.rds                   # ggplot objects for RMarkdown
│
├── reports/
│   ├── borough-report.Rmd              # Parameterized borough report template
│   ├── citywide-report.Rmd             # Citywide report template
│   └── preamble.tex                    # LaTeX styling
│
└── scripts/
    ├── 00-setup.R                      # Packages, paths, helpers
    ├── 01-load-cja.R                   # Load & clean CJA data
    ├── 02-build-survival.R             # Build recidivism dataset
    ├── 03-load-nypd.R                  # Load & clean NYPD data
    ├── 10-compute-shares.R             # Compute share-of-arrests tables
    ├── 11-compute-recidivism.R         # Compute recidivism tables
    ├── 20-plot-functions.R             # Reusable plotting functions
    ├── 21-generate-borough-plots.R     # Generate borough plots
    ├── 22-generate-citywide-plots.R    # Generate citywide plots
    ├── 30-render-reports.R             # Render borough PDF reports
    ├── 31-render-citywide-report.R     # Render citywide PDF report
    └── run-analysis-pipeline.R         # Full pipeline (data → reports)
```

## Quick Start

### Run the full pipeline:
```r
source(here::here("scripts", "run-analysis-pipeline.R"))
```

This will:
1. Load and clean CJA + NYPD data
2. Build survival/recidivism dataset
3. Compute all share and recidivism tables by borough + citywide
4. Generate all plots (saved as PNG + RDS)
5. Render PDF reports for each borough
6. Render citywide PDF report

### Run individual steps:
```r
# Just data prep
source(here("scripts", "01-load-cja.R"))
source(here("scripts", "02-build-survival.R"))
source(here("scripts", "03-load-nypd.R"))

# Just compute tables (after data prep)
source(here("scripts", "10-compute-shares.R"))
source(here("scripts", "11-compute-recidivism.R"))

# Just generate plots (after tables computed)
source(here("scripts", "21-generate-borough-plots.R"))
source(here("scripts", "22-generate-citywide-plots.R"))

# Just render reports (after plots generated)
source(here("scripts", "30-render-reports.R"))       # Borough reports
source(here("scripts", "31-render-citywide-report.R")) # Citywide report
```

### Access pre-computed objects:
```r
# Load tables for custom analysis
share_tables <- readRDS(here("data", "share_tables.rds"))
recid_tables <- readRDS(here("data", "recidivism_tables.rds"))

# Load plot objects
all_plots <- readRDS(here("output", "all_plots.rds"))
all_plots$Manhattan$cja_felony_u18  # Access specific plot
```

## Report Contents

Each borough PDF includes:

### 1. Long-Term Context (NYPD Data)
- Under-18 share of 7 Major Felony arrests (2006–present)
- Auto-labeled: first year, last year, peak, and trough

### 2. Arrest Share Analysis (CJA Data)
- **Felony arrests**: Under-18 trend + secondary bands (13-15, 16-17, 18-19)
- **VFO arrests**: Same breakdown
- **Serious VFO arrests**: Murder, attempted murder, assault 1st, robbery 1st

### 3. Recidivism Analysis
- 365-day felony rearrest rates
- By index offense type: Felony, VFO, Serious VFO
- Under-18 and secondary age bands

### 4. Borough Comparisons
- Arrest share rankings across boroughs
- Recidivism rate rankings

### 5. Gun Arrests Analysis
- CPW 2nd (PL 265.03) as top charge
- Share and recidivism trends

### 6. Data Tables
- Wide-format tables with years across columns
- N and % for share tables
- N eligible and rate for recidivism tables

## Data Requirements

### CJA Arrest File
Single pipe-delimited file with columns including:
- `cid`, `cid_person`, `arrest_number`, `lum_id`
- `arrest_date`, `offense_date`, `date_of_birth`
- `age_at_arrest`, `age_at_offense`
- `most_serious_charge_arrest`, `ms_charge_vfo`, `ms_charge_art_sec`
- `ms_charge_attempt_indicator`, `ms_charge_description_cat`
- `arrest_void_flag`, `county`, `sex`, `race`, `ethnicity`

**Gun arrest identification**: Uses `ms_charge_art_sec` containing "PL 26503" (CPW 2nd degree)

### NYPD Files
Standard NYPD Open Data arrest files (Historic + Year-to-Date)

## Extensibility

### Adding new years:
- Update raw data files in `data/`
- Re-run pipeline — code auto-detects year ranges

### Adding new offense types:
1. Add flag in `01-load-cja.R`
2. Add computation in `10-compute-shares.R` and `11-compute-recidivism.R`
3. Add plot calls in `21-generate-borough-plots.R`
4. Add sections in `borough-report.Rmd`

### Custom plots:
- Source `20-plot-functions.R` for reusable functions:
  - `plot_nypd_longrun()` — long-term NYPD trend
  - `plot_cja_share_under18()` — single-line under-18 trend
  - `plot_secondary_lines()` — multi-line youth age groups
  - `plot_borough_comparison()` — cross-borough bar chart
  - `plot_recidivism_trend()` — recidivism rate trend

## Key Definitions

- **Felony**: VFO or Felony Non-VFO charge
- **VFO**: Violent Felony Offense (NY State statute)
- **Serious VFO**: Assault 1st (PL 120.10), Murder (PL 125.25-27), Attempted Murder, Robbery 1st (PL 160.15)
- **Gun Arrest**: CPW 2nd (PL 265.03) as top charge
- **Recidivism**: New felony arrest within 365 days of index arrest
- **Index arrest**: First arrest per person-day, classified by most serious charge
- **lum_id**: Links records (not NYSID)

## Borough Codes

| Code | Borough | NYPD Code |
|------|---------|-----------|
| M | Manhattan | M |
| K | Brooklyn | K |
| Q | Queens | Q |
| B | Bronx | B |
| S | Staten Island | S |

## Dependencies

```r
install.packages(c(
  "here", "tidyverse", "janitor", "lubridate", "data.table",
  "ggplot2", "ggrepel", "scales",
  "rmarkdown", "knitr", "kableExtra"
))
```

For PDF output: LaTeX distribution (TinyTeX recommended)
```r
tinytex::install_tinytex()
```

## Changes from Previous Version

1. **Removed arrest_charges dependency**: Gun arrests now identified via `ms_charge_art_sec` field in main arrest file
2. **Dynamic year ranges**: All reports auto-detect year ranges from data
3. **Extended CJA coverage**: Now supports 2015-2025 data (previously 2018-present)
