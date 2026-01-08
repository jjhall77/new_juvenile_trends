# ============================================================================
# 20-plot-functions.R
# Reusable plotting functions for borough reports
# Publication-quality charts with consistent styling
# ============================================================================

library(ggplot2)
library(ggrepel)
library(scales)
library(dplyr)

# ============================================================================
# THEME: Consistent publication styling
# ============================================================================

theme_publication <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Text
      plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0),
      plot.subtitle = element_text(size = rel(0.9), color = "grey40", hjust = 0),
      plot.caption = element_text(size = rel(0.8), color = "grey50", hjust = 1),
      
      # Axes
      axis.title = element_text(size = rel(0.9)),
      axis.text = element_text(size = rel(0.85)),
      axis.line = element_line(color = "grey70", linewidth = 0.3),
      
      # Grid
      panel.grid.major = element_line(color = "#E6E6E6", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      
      # Legend
      legend.position = "bottom",
      legend.title = element_text(size = rel(0.9)),
      legend.text = element_text(size = rel(0.85)),
      legend.key.size = unit(0.8, "lines"),
      
      # Facets
      strip.text = element_text(face = "bold", size = rel(1.0)),
      strip.background = element_rect(fill = "grey95", color = NA),
      
      # Margins
      plot.margin = margin(12, 12, 12, 12)
    )
}

# ============================================================================
# COLOR PALETTES
# ============================================================================

# Trend colors (for inc/dec/steady classification)
trend_fills <- c(dec = "#CFE3E7", steady = "#E6F4F2", inc = "#CFE6F7")
trend_lines <- c(dec = "#064556", steady = "#01AD93", inc = "#0291C9")

# Secondary age bands (colorblind-friendly)
secondary_palette <- c(
  "13-15" = "#0072B2",
  "16-17" = "#E69F00",
  "18-19" = "#009E73"
)

# Borough colors
borough_palette <- c(
  "Manhattan"     = "#E41A1C",
  "Brooklyn"      = "#377EB8",
  "Queens"        = "#4DAF4A",
  "Bronx"         = "#984EA3",
  "Staten Island" = "#FF7F00",
  "Citywide"      = "#333333"
)

# ============================================================================
# PLOT: Long-run NYPD trend (2006-present) with smart labels
# ============================================================================

plot_nypd_longrun <- function(
    data,
    borough,
    age_group = "<18",
    title = NULL,
    subtitle = NULL,
    label_years = "auto",
    base_size = 12,
    line_color = "#0072B2",
    save_path = NULL,
    width = 8, height = 4, dpi = 300
) {
  
  # Filter to borough and age group
  df <- data |>
    filter(boro_name == borough, age_group == !!age_group) |>
    arrange(year)
  
  if (nrow(df) == 0) {
    warning("No data for borough: ", borough, " age_group: ", age_group)
    return(NULL)
  }
  
  # Auto-detect key years if requested
  if (identical(label_years, "auto")) {
    year_min <- min(df$year)
    year_max <- max(df$year)
    year_peak <- df$year[which.max(df$pct)]
    year_low <- df$year[which.min(df$pct)]
    
    # Always include first and last
    label_years <- unique(c(year_min, year_max))
    
    # Add peak/low if different from first/last
    if (!year_peak %in% label_years) label_years <- c(label_years, year_peak)
    if (!year_low %in% label_years) label_years <- c(label_years, year_low)
    label_years <- sort(label_years)
  }
  
  # Label data
  pts <- df |> filter(year %in% label_years)
  
  # Default titles
  if (is.null(title)) {
    title <- paste0(borough, ": Under-18 Share of 7 Major Felony Arrests")
  }
  if (is.null(subtitle)) {
    subtitle <- paste0("NYPD data, ", min(df$year), "-", max(df$year))
  }
  
  p <- ggplot(df, aes(x = year, y = pct)) +
    geom_line(color = line_color, linewidth = 1) +
    geom_point(
      data = pts,
      color = line_color, size = 2.5, shape = 21, fill = "white", stroke = 1.2
    ) +
    geom_text_repel(
      data = pts,
      aes(label = paste0(percent(pct, accuracy = 0.1), "\n(", year, ")")),
      size = 3.2, color = line_color,
      nudge_y = 0.01, direction = "y",
      box.padding = 0.4, point.padding = 0.3,
      min.segment.length = 0, segment.color = "grey60",
      show.legend = FALSE
    ) +
    scale_x_continuous(
      breaks = seq(floor(min(df$year)/5)*5, ceiling(max(df$year)/5)*5, by = 5),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      expand = expansion(mult = c(0.02, 0.15))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL, y = "Share of arrests",
      caption = "Source: NYPD Open Data"
    ) +
    theme_publication(base_size = base_size) +
    coord_cartesian(clip = "off")
  
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = width, height = height, dpi = dpi)
  }
  
  p
}

# ============================================================================
# PLOT: CJA share trend (2018-present) - single line for under-18
# Labels: n & % on first year and last full year; % only on partial last year
# ============================================================================

plot_cja_share_under18 <- function(
    data,
    borough,
    offense_type = "Felony",
    title = NULL,
    subtitle = NULL,
    last_full_year = NULL,
    base_size = 12,
    line_color = "#0072B2",
    save_path = NULL,
    width = 8, height = 4, dpi = 300
) {
  
  # Filter to under-18
  df <- data |>
    filter(boro_name == borough, age_band_primary == "<18") |>
    arrange(year)
  
  if (nrow(df) == 0) {
    warning("No data for borough: ", borough)
    return(NULL)
  }
  
  # Determine years
  first_year <- min(df$year)
  last_year <- max(df$year)
  
  # Auto-detect last full year if not provided (assume current partial year)
  if (is.null(last_full_year)) {
    last_full_year <- last_year - 1
  }
  
  # Build label points
  # First year: n & %
  # Last full year: n & % (if different from first)
  # Last year: % only (if partial/different from last full year)
  
  label_years <- unique(c(first_year, last_full_year, last_year))
  pts <- df |> filter(year %in% label_years)
  
  pts <- pts |>
    mutate(
      label_text = case_when(
        # First year gets n & %
        year == first_year ~ paste0(percent(pct, accuracy = 0.1), "\n(n=", comma(n), ")"),
        # Last full year gets n & % (if different from first)
        year == last_full_year & year != first_year ~ paste0(percent(pct, accuracy = 0.1), "\n(n=", comma(n), ")"),
        # Last year (if partial) gets % only
        year == last_year & year != last_full_year ~ percent(pct, accuracy = 0.1),
        # Fallback
        TRUE ~ percent(pct, accuracy = 0.1)
      )
    )
  
  # Titles
  if (is.null(title)) {
    title <- paste0(borough, ": Under-18 Share of ", offense_type, " Arrests")
  }
  if (is.null(subtitle)) {
    subtitle <- paste0(first_year, "-", last_year)
  }
  
  p <- ggplot(df, aes(x = year, y = pct)) +
    geom_line(color = line_color, linewidth = 1) +
    geom_point(
      data = pts,
      color = line_color, size = 2.5, shape = 21, fill = "white", stroke = 1.2
    ) +
    geom_text_repel(
      data = pts,
      aes(label = label_text),
      size = 3.2, color = line_color,
      nudge_y = 0.005, direction = "y",
      box.padding = 0.4, point.padding = 0.3,
      min.segment.length = 0, segment.color = "grey60",
      show.legend = FALSE
    ) +
    scale_x_continuous(
      breaks = seq(first_year, last_year, by = 1),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL, y = "Share of arrests"
    ) +
    theme_publication(base_size = base_size) +
    coord_cartesian(clip = "off")
  
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = width, height = height, dpi = dpi)
  }
  
  p
}

# ============================================================================
# PLOT: Secondary age bands (13-15, 16-17, 18-19) - multi-line
# Labels: n & % on first year and last full year; % only on partial last year
# ============================================================================

plot_secondary_lines <- function(
    data,
    borough,
    value_var = "pct",
    offense_type = "Felony",
    metric_label = "Share of arrests",
    title = NULL,
    subtitle = NULL,
    last_full_year = NULL,
    keep_ages = c("13-15", "16-17", "18-19"),
    palette = secondary_palette,
    show_n = TRUE,
    n_var = "n",
    base_size = 12,
    save_path = NULL,
    width = 8, height = 4, dpi = 300
) {
  
  age_var <- if ("age_band_secondary" %in% names(data)) {
    "age_band_secondary"
  } else {
    names(data)[grepl("age", names(data), ignore.case = TRUE)][1]
  }
  
  df <- data |>
    filter(
      boro_name == borough,
      .data[[age_var]] %in% keep_ages
    ) |>
    arrange(year, .data[[age_var]])
  
  if (nrow(df) == 0) {
    warning("No data for borough: ", borough)
    return(NULL)
  }
  
  # Determine years
  first_year <- min(df$year)
  last_year <- max(df$year)
  
  # Auto-detect last full year if not provided
  if (is.null(last_full_year)) {
    last_full_year <- last_year - 1
  }
  
  # Build label points for first, last full, and last year
  label_years <- unique(c(first_year, last_full_year, last_year))
  pts <- df |> filter(year %in% label_years)
  
  # Format labels based on year
  if (value_var == "pct" || value_var == "rate") {
    pts <- pts |>
      mutate(
        label_text = case_when(
          # First year: n & %
          year == first_year & show_n & n_var %in% names(pts) ~ 
            paste0(percent(.data[[value_var]], accuracy = 0.1), "\n(n=", comma(.data[[n_var]]), ")"),
          year == first_year ~ percent(.data[[value_var]], accuracy = 0.1),
          # Last full year: n & % (if different from first)
          year == last_full_year & year != first_year & show_n & n_var %in% names(pts) ~ 
            paste0(percent(.data[[value_var]], accuracy = 0.1), "\n(n=", comma(.data[[n_var]]), ")"),
          year == last_full_year & year != first_year ~ percent(.data[[value_var]], accuracy = 0.1),
          # Last year (if partial): % only
          year == last_year & year != last_full_year ~ percent(.data[[value_var]], accuracy = 0.1),
          # Fallback
          TRUE ~ percent(.data[[value_var]], accuracy = 0.1)
        )
      )
    y_labels <- percent_format(accuracy = 1)
  } else {
    pts <- pts |>
      mutate(label_text = comma(.data[[value_var]]))
    y_labels <- comma
  }
  
  # Titles
  if (is.null(title)) {
    title <- paste0(borough, ": ", offense_type, " by Youth Age Group")
  }
  if (is.null(subtitle)) {
    subtitle <- paste0(first_year, "-", last_year)
  }
  
  p <- ggplot(df, aes(x = year, y = .data[[value_var]],
                      color = .data[[age_var]], group = .data[[age_var]])) +
    geom_line(linewidth = 1) +
    geom_point(
      data = pts,
      size = 2.5, shape = 21, fill = "white", stroke = 1.2
    ) +
    geom_text_repel(
      data = pts,
      aes(label = label_text),
      size = 3, direction = "y",
      nudge_x = ifelse(pts$year == min(pts$year), -0.3, 0.3),
      box.padding = 0.3, point.padding = 0.2,
      min.segment.length = 0, segment.color = "grey60",
      show.legend = FALSE
    ) +
    scale_color_manual(values = palette, name = "Age group") +
    scale_x_continuous(
      breaks = seq(first_year, last_year, by = 1),
      expand = expansion(mult = c(0.08, 0.08))
    ) +
    scale_y_continuous(
      labels = y_labels,
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL, y = metric_label
    ) +
    theme_publication(base_size = base_size) +
    theme(legend.position = "bottom") +
    coord_cartesian(clip = "off")
  
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = width, height = height, dpi = dpi)
  }
  
  p
}

# ============================================================================
# PLOT: Cross-borough comparison (bar or lollipop)
# ============================================================================

plot_borough_comparison <- function(
    data,
    year_filter,
    age_filter = "<18",
    age_var = "age_band_primary",
    value_var = "pct",
    metric_label = "Share",
    title = NULL,
    subtitle = NULL,
    palette = borough_palette,
    include_citywide = TRUE,
    base_size = 12,
    save_path = NULL,
    width = 7, height = 5, dpi = 300
) {
  
  df <- data |>
    filter(
      year == year_filter,
      .data[[age_var]] == age_filter
    )
  
  if (!include_citywide) {
    df <- df |> filter(boro_name != "Citywide")
  }
  
  # Order by value
  df <- df |>
    arrange(desc(.data[[value_var]])) |>
    mutate(boro_name = factor(boro_name, levels = boro_name))
  
  # Titles
  if (is.null(title)) {
    title <- paste0("Under-18 Share of ", metric_label, " Arrests by Borough (", year_filter, ")")
  }
  
  # Format
  if (value_var == "pct" || value_var == "rate") {
    label_fun <- \(x) percent(x, accuracy = 0.1)
    y_labels <- percent_format(accuracy = 1)
  } else {
    label_fun <- comma
    y_labels <- comma
  }
  
  p <- ggplot(df, aes(x = boro_name, y = .data[[value_var]], fill = boro_name)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(
      aes(label = label_fun(.data[[value_var]])),
      vjust = -0.5, size = 3.5
    ) +
    scale_fill_manual(values = palette) +
    scale_y_continuous(
      labels = y_labels,
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL, y = paste0("Share of ", metric_label, " Arrests")
    ) +
    theme_publication(base_size = base_size) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
  
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = width, height = height, dpi = dpi)
  }
  
  p
}

# ============================================================================
# PLOT: Recidivism rate trend
# Labels show n_eligible (index arrests), not reoffense count
# ============================================================================

plot_recidivism_trend <- function(
    data,
    borough,
    age_filter = "<18",
    age_var = "age_band_primary",
    offense_type = "Felony",
    title = NULL,
    subtitle = NULL,
    label_years = "endpoints",
    show_n = TRUE,
    base_size = 12,
    line_color = "#D55E00",
    save_path = NULL,
    width = 8, height = 4, dpi = 300
) {
  
  df <- data |>
    filter(boro_name == borough, .data[[age_var]] == age_filter) |>
    arrange(year)
  
  if (nrow(df) == 0) {
    warning("No data for borough: ", borough, " age: ", age_filter)
    return(NULL)
  }
  
  # Label years
  if (identical(label_years, "endpoints")) {
    label_years <- c(min(df$year), max(df$year))
  }
  
  pts <- df |>
    filter(year %in% label_years) |>
    mutate(
      # n_eligible = number of index arrests (NOT reoffense count)
      label_text = if (show_n) {
        paste0(percent(rate, accuracy = 0.1), "\n(n=", comma(n_eligible), ")")
      } else {
        percent(rate, accuracy = 0.1)
      }
    )
  
  # Titles
  if (is.null(title)) {
    title <- paste0(borough, ": <18 ", offense_type, " Index 365-Day Recidivism")
  }
  if (is.null(subtitle)) {
    subtitle <- "Felony rearrest within 365 days of index arrest"
  }
  
  p <- ggplot(df, aes(x = year, y = rate)) +
    geom_line(color = line_color, linewidth = 1) +
    geom_point(
      data = pts,
      color = line_color, size = 2.5, shape = 21, fill = "white", stroke = 1.2
    ) +
    geom_text_repel(
      data = pts,
      aes(label = label_text),
      size = 3.2, color = line_color,
      nudge_y = 0.01, direction = "y",
      box.padding = 0.4, point.padding = 0.3,
      min.segment.length = 0, segment.color = "grey60",
      show.legend = FALSE
    ) +
    scale_x_continuous(
      breaks = seq(min(df$year), max(df$year), by = 1),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL, y = "Recidivism rate"
    ) +
    theme_publication(base_size = base_size) +
    coord_cartesian(clip = "off")
  
  if (!is.null(save_path)) {
    ggsave(save_path, plot = p, width = width, height = height, dpi = dpi)
  }
  
  p
}

# ============================================================================
# TABLE: Format share table for report (years across top)
# ============================================================================

format_share_table <- function(data, borough, age_filter = "<18", age_var = "age_band_primary") {
  
  data |>
    filter(boro_name == borough, .data[[age_var]] == age_filter) |>
    select(year, n, pct) |>
    mutate(pct = percent(pct, accuracy = 0.1)) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = c(n, pct),
      names_glue = "{year}_{.value}"
    ) |>
    # Reorder so each year has n then pct
    select(order(names(.)))
}

# ============================================================================
# TABLE: Format recidivism table for report (years across top)
# ============================================================================

format_recidivism_table <- function(data, borough, age_filter = "<18", age_var = "age_band_primary") {
  
  data |>
    filter(boro_name == borough, .data[[age_var]] == age_filter) |>
    select(year, n_eligible, rate) |>
    mutate(rate = percent(rate, accuracy = 0.1)) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = c(n_eligible, rate),
      names_glue = "{year}_{.value}"
    ) |>
    select(order(names(.)))
}

message("Plot functions loaded.")
