# helpers.R

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Data loading -------------------------------------------

read_fig <- function(name) {
  read_csv(file.path(DATA_DIR, paste0(name, ".csv")), show_col_types = FALSE)
}

read_levels <- function(
    name,
    value_col    = "value",
    levels_order = c("Bachelors", "Masters", "Doctorate")
) {
  read_fig(name) %>%
    select(Year, all_of(levels_order)) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(-Year, names_to = "Level", values_to = value_col) %>%
    mutate(Level = factor(Level, levels = levels_order))
}

fill_gaps <- function(x, y) {
  zoo::na.approx(y, x = x, na.rm = FALSE)
}

pivot_observed_projected <- function(
    df,
    year_col         = "Year",
    projected_suffix = "\\(projected\\)",
    country_name     = "Country"
) {
  df %>%
    mutate(across(-all_of(year_col), as.numeric)) %>%
    pivot_longer(-all_of(year_col), names_to = "series", values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(
      period       = if_else(str_detect(series, projected_suffix), "Projected", "Observed"),
      !!country_name := str_remove(series, paste0(" ", projected_suffix)) %>% str_trim()
    ) %>%
    select(all_of(year_col), all_of(country_name), period, value)
}

# ---- Generic country/group line charts ----------------------
# Used across many figures: 013, 018, 019, 023, 062, 063, 064,
# 066, 069, 070, 078, 080, 081, and others.
#
# read_country_lines: reads a wide CSV (Year in col 1, one column
# per group) and returns long format ordered by latest value.
#
# plot_country_lines: renders the standard right-labeled line chart.
# label_fmt:   a function applied to the value for the repel label,
#              e.g. label_comma(), fmt_pct_0, fmt_pct_1.
# y_scale:     "continuous" (default) or "log10".
# pad_right:   defaults to PAD_RIGHT_STANDARD.
# color_scale: a ggplot scale object; defaults to scale_color_tpa().
# y_args:      named list of extra arguments forwarded to
#              scale_y_continuous / scale_y_log10.

read_country_lines <- function(name,
                               group_col    = "Country",
                               value_col    = "Count",
                               filter_na    = TRUE,
                               extra_filter = NULL) {
  df <- read_fig(name) %>%
    rename(Year = 1) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(-Year, names_to = group_col, values_to = value_col)
  
  if (filter_na) {
    df <- df %>% filter(!is.na(.data[[value_col]]))
  }
  
  if (!is.null(extra_filter)) {
    df <- df %>% filter(!!extra_filter)
  }
  
  df %>% order_by_latest_str(group_col, value_col)
}

plot_country_lines <- function(df, meta, y_label,
                               group_col   = "Country",
                               value_col   = "Count",
                               label_fmt   = label_comma(),
                               y_scale     = "continuous",
                               pad_right   = PAD_RIGHT_STANDARD,
                               color_scale = scale_color_tpa(),
                               x_by        = NULL,
                               y_args      = list()) {
  labels   <- last_by_group(df, Year, .data[[group_col]])
  label_fn <- if (is.function(label_fmt)) label_fmt else identity
  
  p <- ggplot(df, aes(Year, .data[[value_col]],
                      color = .data[[group_col]],
                      group = .data[[group_col]])) +
    geom_line(linewidth = LINE_WIDTH) +
    geom_label_repel_tpa(
      data    = labels,
      mapping = aes(label = paste0(.data[[group_col]], ": ",
                                   label_fn(.data[[value_col]]))),
      nudge_x      = 0.5,
      hjust        = 0,
      direction    = "y",
      with_segment = TRUE
    ) +
    scale_x_years(df$Year, by = x_by, pad_right = pad_right) +
    color_scale +
    guides(color = "none") +
    labs_meta(meta, y = y_label) +
    theme_tpa()
  
  y_defaults <- list(labels = label_comma(), expand = EXPAND_LINE)
  y_final    <- modifyList(y_defaults, y_args)
  
  if (y_scale == "log10") {
    p <- p + do.call(scale_y_log10, y_final)
  } else {
    p <- p + do.call(scale_y_continuous, y_final)
  }
  
  p
}

# ---- Employment sector line charts --------------------------
# Used by figs 069, 070.
# Expects a wide CSV with Year in col 1 and one column per sector.
# Averages across any duplicate Year/Sector combinations.

read_sector_employment <- function(name) {
  read_fig(name) %>%
    filter(!is.na(Year)) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(-Year, names_to = "Sector", values_to = "Share") %>%
    filter(!is.na(Share)) %>%
    mutate(Share = Share * 100) %>%
    group_by(Year, Sector) %>%
    summarise(Share = mean(Share), .groups = "drop") %>%
    order_by_latest(Sector, Share)
}

plot_sector_employment <- function(df, meta, y_label,
                                   y_limits = NULL,
                                   y_breaks = NULL) {
  labels <- last_by_group(df, Year, Sector)
  
  y_args <- list(labels = label_number(suffix = "%"), expand = EXPAND_BAR_TIGHT)
  if (!is.null(y_limits)) y_args$limits <- y_limits
  if (!is.null(y_breaks)) y_args$breaks <- y_breaks
  
  ggplot(df, aes(Year, Share, color = Sector, group = Sector)) +
    geom_line(linewidth = LINE_WIDTH) +
    geom_label_repel_tpa(
      data    = labels,
      mapping = aes(label = paste0(Sector, ": ", fmt_pct_0(Share))),
      nudge_x      = 0.5,
      hjust        = 0,
      direction    = "y",
      with_segment = TRUE
    ) +
    scale_x_years(df$Year, pad_right = PAD_RIGHT_WIDE) +
    do.call(scale_y_continuous, y_args) +
    scale_color_tpa() +
    guides(color = "none") +
    labs_meta(meta, y = y_label) +
    theme_tpa()
}

# ---- Citizenship area charts --------------------------------
# Used by figs 015, 016, 017, 095, 096, 097.
# Expects a CSV with Year in col 1, US citizens in col 2,
# temporary visa holders in col 3.
# Colors come from citizenship_colors defined in theme.R.

read_citizenship_area <- function(name) {
  read_fig(name) %>%
    rename(Year = 1,
           `U.S. citizens and permanent residents` = 2,
           `Temporary visa holders`                = 3) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(-Year, names_to = "Status", values_to = "Count") %>%
    filter(!is.na(Year), !is.na(Count)) %>%
    order_by_total(Status, Count)
}

plot_citizenship_area <- function(df, meta, y_label) {
  ggplot(df, aes(Year, Count, fill = Status)) +
    geom_area(alpha = ALPHA_FILL) +
    scale_x_years(df$Year) +
    scale_y_continuous(labels = label_comma(), expand = EXPAND_BAR) +
    scale_fill_citizenship() +
    labs_meta(meta, y = y_label) +
    theme_tpa()
}

# ---- Patent / technology line charts ------------------------
# Used by figs 059, 060, 061.
# Expects a CSV with Year in col 1 and one column per country.
# Recodes the two most common long country names automatically.

read_patents <- function(name) {
  read_fig(name) %>%
    rename(Year = 1) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(-Year, names_to = "Country", values_to = "Count") %>%
    filter(!is.na(Count), Count > 0) %>%
    mutate(Country = case_when(
      Country == "United States of America" ~ "United States",
      Country == "Republic of Korea"        ~ "South Korea",
      TRUE                                  ~ Country
    )) %>%
    order_by_latest(Country, Count)
}

plot_patents <- function(df, meta, y_label = "Patents granted (log scale)") {
  labels <- last_by_group(df, Year, Country)
  
  ggplot(df, aes(Year, Count, color = Country, group = Country)) +
    geom_line(linewidth = LINE_WIDTH) +
    geom_label_repel_tpa(
      data    = labels,
      mapping = aes(label = paste0(Country, ": ", label_comma()(Count))),
      nudge_x      = 0.5,
      hjust        = 0,
      direction    = "y",
      with_segment = TRUE
    ) +
    scale_x_years(df$Year, pad_right = PAD_RIGHT_STANDARD) +
    scale_y_log10(labels = label_comma(),
                  breaks = c(10, 100, 1000, 5000, 10000, 50000),
                  expand = EXPAND_LINE) +
    scale_color_tpa() +
    guides(color = "none") +
    labs_meta(meta, y = y_label) +
    theme_tpa()
}

# ---- Discrete single-series bar ----
# Used by figs 010, 022, 039.
# Expects a CSV with Year in col 1 and the value series in col 2.
# year_as_factor = TRUE converts Year to a character/factor so
# scale_x_discrete applies; set FALSE for continuous year axes.

read_discrete_bar <- function(name, value_col = "value",
                              year_as_factor = TRUE) {
  df <- read_fig(name) %>%
    rename(Year = 1, !!value_col := 2) %>%
    mutate(across(everything(), as.numeric)) %>%
    filter(!is.na(Year), !is.na(.data[[value_col]]))
  
  if (year_as_factor) df <- df %>% mutate(Year = factor(Year))
  df
}

plot_discrete_bar <- function(df, meta, y_label,
                              fill    = tpa_colors[1],
                              value_col = "value",
                              label_fn  = label_comma()) {
  labels <- first_last_peak(df, Year, .data[[value_col]])
  
  ggplot(df, aes(Year, .data[[value_col]])) +
    geom_col(fill = fill, width = 0.7, alpha = ALPHA_COL) +
    geom_text_tpa(
      data    = labels,
      mapping = aes(label = label_fn(.data[[value_col]])),
      color   = fill
    ) +
    coord_cartesian(clip = "off") +
    scale_x_discrete(expand = expansion(add = c(1, 1))) +
    scale_y_continuous(labels = label_fn, expand = EXPAND_BAR_TALL) +
    labs_meta(meta, y = y_label) +
    theme_tpa()
}

# ---- H-1B denial line charts --------------------------------
# Used by figs 073, 075, 087, 088.
# Expects a CSV with Year in col 1 and two denial-type columns.
# col1/col2 name the factor levels; colors come from hb_denial_colors
# in theme.R, keyed to "New Denials" and "Renewal Denials" by default.
# Pass col1/col2 explicitly when the CSV uses different column names.

read_hb_denials <- function(name,
                            col1 = "New Denials",
                            col2 = "Renewal Denials") {
  read_fig(name) %>%
    filter(!is.na(Year)) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(-Year, names_to = "Type", values_to = "Count") %>%
    filter(!is.na(Count)) %>%
    mutate(Type = factor(Type, levels = c(col1, col2)))
}

plot_hb_denials <- function(df, meta, y_label,
                            col1 = "New Denials",
                            col2 = "Renewal Denials") {
  palette <- setNames(hb_denial_colors, c(col1, col2))
  
  ggplot(df, aes(Year, Count, color = Type, group = Type)) +
    geom_line(linewidth = LINE_WIDTH) +
    geom_point(size = 1.5) +
    scale_x_years(df$Year, by = 1) +
    scale_y_continuous(labels = label_comma(), expand = EXPAND_LINE) +
    scale_color_manual(values = palette) +
    labs_meta(meta, y = y_label) +
    theme_tpa()
}

# ---- Nonresident share by field line charts -----------------
# Used by figs 091, 092, 093.
# Canonical rename vector for NCES field column headers to short display names.
# Defined once here so all three degree-level charts use identical field labels
# and field_colors mappings stay consistent.

FIELD_RENAME_NCES <- c(
  "Biological and Biomedical Sciences"                     = "Biological and biomedical sciences",
  "Computer and Information Sciences and Support Services" = "Computer Sciences",
  "Engineering"                                            = "Engineering",
  "Health Professions and Related Programs"                = "Health Sciences",
  "Mathematics and Statistics"                             = "Mathematics",
  "Physical Sciences"                                      = "Physical Sciences",
  "Psychology"                                             = "Psychology",
  "Social Sciences"                                        = "Social Sciences"
)

read_nonresident_field <- function(name) {
  read_fig(name) %>%
    filter(!is.na(Year)) %>%
    select(Year, all_of(names(FIELD_RENAME_NCES))) %>%
    rename(!!!setNames(names(FIELD_RENAME_NCES), FIELD_RENAME_NCES)) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(-Year, names_to = "Field", values_to = "Share") %>%
    filter(!is.na(Share)) %>%
    order_by_latest(Field, Share)
}

plot_nonresident_field <- function(df, meta, y_label) {
  labels <- last_by_group(df, Year, Field)
  
  ggplot(df, aes(Year, Share, color = Field, group = Field)) +
    geom_line(linewidth = LINE_WIDTH) +
    geom_label_repel_tpa(
      data    = labels,
      mapping = aes(label = paste0(Field, ": ", fmt_pct_1(Share))),
      nudge_x      = 0.5,
      hjust        = 0,
      direction    = "y",
      with_segment = TRUE
    ) +
    scale_x_years(df$Year, pad_right = PAD_RIGHT_WIDE) +
    scale_y_continuous(labels = label_number(suffix = "%"),
                       expand = EXPAND_LINE) +
    scale_color_fields() +
    guides(color = "none") +
    labs_meta(meta, y = y_label) +
    theme_tpa()
}

# ---- Title / source metadata --------------------------------

format_source <- function(src) {
  src %>%
    gsub("<[^>]+>", "", .) %>%
    gsub("https?://\\S+", "", .) %>%
    gsub("www\\.\\S+", "", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws() %>%
    paste0("Data Source: ", .)
}

titles_and_sources <- read_csv("titles_and_sources.csv", show_col_types = FALSE)

get_meta <- function(fig_no) {
  m <- titles_and_sources %>% filter(.data$fig_no == !!fig_no)
  
  if (nrow(m) == 0) {
    stop("No titles_and_sources.csv entry for ", fig_no)
  }
  
  short <- if (
    "source_short" %in% names(m) &&
    !is.na(m$source_short[1]) &&
    nzchar(m$source_short[1])
  ) m$source_short[1] else m$source[1]
  
  list(
    title    = m$title[1],
    subtitle = CHART_LINE,
    caption  = format_source(short)
  )
}

labs_meta <- function(meta, y = NULL, x = NULL, fill = NULL, color = NULL) {
  labs(
    title    = meta$title,
    subtitle = meta$subtitle,
    caption  = meta$caption,
    x        = x,
    y        = y,
    fill     = fill,
    color    = color
  )
}

# ---- Label selection ----------------------------------------

first_last_peak <- function(df, x, y, include_peak = TRUE) {
  rows <- list(
    df %>% slice_min({{ x }}, n = 1),
    df %>% slice_max({{ x }}, n = 1)
  )
  
  if (include_peak) {
    rows <- c(rows, list(df %>% slice_max({{ y }}, n = 1)))
  }
  
  bind_rows(rows) %>% distinct({{ x }}, .keep_all = TRUE)
}

flp_by_group <- function(df, x, y, group, include_peak = TRUE) {
  df %>%
    group_by({{ group }}) %>%
    group_modify(~ first_last_peak(.x, {{ x }}, {{ y }}, include_peak)) %>%
    ungroup()
}

fl_by_group <- function(df, x, y, group) {
  flp_by_group(df, {{ x }}, {{ y }}, {{ group }}, include_peak = FALSE)
}

last_by_group <- function(df, x, group) {
  df %>%
    group_by({{ group }}) %>%
    slice_max({{ x }}, n = 1) %>%
    ungroup()
}

order_by_latest <- function(df, group_col, value_col, x_col = Year) {
  ord <- df %>%
    filter({{ x_col }} == max({{ x_col }})) %>%
    arrange(desc({{ value_col }})) %>%
    pull({{ group_col }})
  
  df %>% mutate({{ group_col }} := factor({{ group_col }}, levels = ord))
}

order_by_latest_str <- function(df, group_col, value_col, x_col = "Year") {
  g <- sym(group_col)
  v <- sym(value_col)
  x <- sym(x_col)
  
  ord <- df %>%
    filter(!!x == max(!!x, na.rm = TRUE)) %>%
    arrange(desc(!!v)) %>%
    pull(!!g)
  
  df %>% mutate(!!g := factor(!!g, levels = ord))
}

order_by_total <- function(df, group_col, value_col) {
  ord <- df %>%
    group_by({{ group_col }}) %>%
    summarise(.total = sum({{ value_col }}, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(.total)) %>%
    pull({{ group_col }})
  
  df %>% mutate({{ group_col }} := factor({{ group_col }}, levels = rev(ord)))
}

nudge_by_series <- function(labels_df, series_col, above, amount = 1.2) {
  labels_df %>%
    mutate(nudge_y = if_else({{ series_col }} %in% above, amount, -amount))
}

# ---- Number / chart utilities -------------------------------

proj_start_year <- function(df) {
  df %>% filter(period == "Projected") %>% pull(Year) %>% min()
}

y_top_round <- function(values, by) {
  ceiling(max(values, na.rm = TRUE) / by) * by
}

fmt_pct_0    <- function(x) sprintf("%.0f%%", x)
fmt_pct_1    <- function(x) sprintf("%.1f%%", x)
fmt_num_k    <- function(x) paste0(round(x / 1e3), "K")
fmt_num_m    <- function(x) paste0(round(x / 1e6, 1), "M")
fmt_num_b    <- function(x) paste0(round(x / 1e9, 1), "B")
fmt_dollar   <- function(x) sprintf("$%.2f", x)
fmt_dollar_m <- function(x) sprintf("$%.1fM", x)
fmt_dollar_b <- function(x) sprintf("$%.1fB", x)

# ---- Stacked-chart palettes ---------------------------------

stacked_palette <- function(n = NULL, ordinal = FALSE, reverse = FALSE) {
  vals <- if (ordinal && !is.null(n) && n <= 3) {
    tail(navy_tints, n)
  } else if (!is.null(n)) {
    tpa_colors[seq_len(n)]
  } else {
    tpa_colors
  }
  
  if (reverse) rev(vals) else vals
}

scale_fill_stacked <- function(
    n               = NULL,
    ordinal         = FALSE,
    reverse_palette = TRUE,
    reverse_legend  = TRUE,
    ...
) {
  scale_fill_manual(
    values = stacked_palette(n, ordinal, reverse = reverse_palette),
    guide  = guide_legend(reverse = reverse_legend),
    ...
  )
}

scale_color_stacked <- function(
    n               = NULL,
    ordinal         = FALSE,
    reverse_palette = FALSE,
    reverse_legend  = ordinal,
    ...
) {
  scale_color_manual(
    values = stacked_palette(n, ordinal, reverse = reverse_palette),
    guide  = guide_legend(reverse = reverse_legend),
    ...
  )
}

# ---- Export -------------------------------------------------

save_chart <- function(plot, name, size = "standard") {
  dims <- switch(
    size,
    standard = SIZE_STANDARD,
    long     = SIZE_LONG,
    xlong    = SIZE_XLONG,
    stop("Unknown size: ", size, ". Use 'standard', 'long', or 'xlong'.")
  )
  
  dir.create("final_charts", showWarnings = FALSE)
  
  ggsave(
    file.path("final_charts", paste0(name, ".png")),
    plot,
    width  = dims$w,
    height = dims$h,
    dpi    = 300,
    bg     = "white"
  )
  
  plot
}