# tables.R

# ---- Theme --------------------------------------------------

theme_gt_tpa <- function(gt_tbl, meta = NULL) {
  tbl <- gt_tbl %>%
    gt_theme_538() %>%
    opt_table_font(font = list(google_font("Source Sans 3"), default_fonts())) %>%
    tab_style(
      style     = cell_borders(sides = "top", color = tpa_colors[1], weight = px(3)),
      locations = cells_column_labels(everything())
    ) %>%
    tab_options(
      table.font.size        = px(13),
      table.font.color       = "grey25",
      table.background.color = "white",
      
      column_labels.font.weight    = "bold",
      column_labels.font.size      = px(11),
      column_labels.text_transform = "uppercase",
      column_labels.padding        = px(10),
      
      row.striping.background_color = "#F5F2EC",
      data_row.padding              = px(7),
      
      heading.title.font.size    = px(16),
      heading.title.font.weight  = "bold",
      heading.subtitle.font.size = px(10),
      heading.padding            = px(8),
      heading.align              = "left",
      
      source_notes.font.size = px(10),
      source_notes.padding   = px(8)
    ) %>%
    opt_align_table_header(align = "left")
  
  if (!is.null(meta)) {
    tbl <- tbl %>%
      tab_header(title = meta$title, subtitle = meta$subtitle) %>%
      tab_source_note(source_note = meta$caption) %>%
      tab_style(
        style     = cell_text(color = "grey45"),
        locations = cells_title(groups = "subtitle")
      ) %>%
      tab_style(
        style     = cell_text(color = "grey45"),
        locations = cells_source_notes()
      )
  }
  
  tbl
}

# ---- Combined share-change table (all degrees) --------------
# Used by tab 006d.
# Expects Field plus {Degree}_FirstYear / _LastYear / _PctChangeInShare
# columns for Bachelors, Masters, Doctorate. Row filtering and any
# field merges are done upstream in the calculation chunk, not here.

build_change_table_wide <- function(name, meta) {
  df <- read_fig(name) %>%
    mutate(across(-Field, ~ suppressWarnings(as.numeric(.))))
  
  degrees     <- c("Bachelors", "Masters", "Doctorate")
  change_cols <- paste0(degrees, "_PctChangeInShare")
  year_cols   <- c(paste0(degrees, "_FirstYear"),
                   paste0(degrees, "_LastYear"))
  
  # rank by mean change across the three degrees, then drop the helper col
  df <- df %>%
    mutate(.avg = rowMeans(across(all_of(change_cols)), na.rm = TRUE)) %>%
    arrange(desc(.avg)) %>%
    select(-.avg)
  
  # symmetric domain so 0 is the color midpoint for the change columns
  lim <- max(abs(df[change_cols]), na.rm = TRUE)
  
  tbl <- df %>%
    gt(rowname_col = "Field", id = name) %>%
    fmt_number(columns = ends_with("FirstYear"), decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = ends_with("LastYear"),  decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = ends_with("PctChangeInShare"),
               decimals = 1, force_sign = TRUE, pattern = "{x}%")
  
  for (deg in degrees) {
    tbl <- tbl %>%
      tab_spanner(label = deg, columns = starts_with(paste0(deg, "_")))
  }
  
  tbl <- tbl %>%
    cols_label(
      ends_with("FirstYear")        ~ "1995",
      ends_with("LastYear")         ~ "2024",
      ends_with("PctChangeInShare") ~ "Change"
    ) %>%
    # red/blue diverging gradient on the change columns
    data_color(
      columns = all_of(change_cols),
      fn = scales::col_numeric(
        palette = c(tpa_colors[2], "white", tpa_colors[1]),
        domain  = c(-lim, lim)
      )
    ) %>%
    # white background on the year columns
    tab_style(
      style     = cell_fill(color = "white"),
      locations = cells_body(columns = all_of(year_cols))
    ) %>%
    cols_align(align = "right", columns = -Field)
  
  # bold year-column values above 50
  for (col in year_cols) {
    tbl <- tbl %>%
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body(columns = all_of(col),
                               rows = .data[[col]] > 50)
      )
  }
  
  tbl %>%
    tab_footnote(
      footnote  = paste("Values are the percent of degree completions awarded to",
                        "international (nonresident) students. Change is the relative",
                        "change in that share from 1995 to 2024."),
      locations = cells_column_spanners(spanners = degrees)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1]
    )) %>%
    save_table(name, size = "long")
}

# ---- Field share-change table -------------------------------
# Used by tab 006.
# Expects a CSV with Field, FirstYear, LastYear, PctChangeInShare.
# FirstYear/LastYear are nonresident shares (%); PctChangeInShare is
# the relative change between them.

build_change_table <- function(name, meta) {
  df <- read_fig(name) %>%
    mutate(
      FirstYear        = as.numeric(FirstYear),
      LastYear         = as.numeric(LastYear),
      PctChangeInShare = as.numeric(PctChangeInShare)
    ) %>%
    arrange(desc(PctChangeInShare))
  
  df %>%
    gt(rowname_col = "Field") %>%
    fmt_number(columns = c(FirstYear, LastYear), decimals = 1) %>%
    fmt_number(columns = PctChangeInShare, decimals = 1,
               force_sign = TRUE, pattern = "{x}%") %>%
    cols_label(
      FirstYear        = "1995",
      LastYear         = "2024",
      PctChangeInShare = "Change"
    ) %>%
    gt_color_rows(columns  = PctChangeInShare,
                  palette  = c(TPA_RED_LIGHT, tpa_colors[1]),
                  pal_type = "continuous") %>%
    cols_align(align = "right",
               columns = c(FirstYear, LastYear, PctChangeInShare)) %>%
    tab_style(style     = cell_text(weight = "bold"),
              locations = cells_body(columns = PctChangeInShare)) %>%
    theme_gt_tpa(meta = meta) %>%
    save_table(name)
}

# ---- University ranking tables ------------------------------
# Used by tabs 001, 002.
# integer_cols = TRUE uses fmt_integer on the four field columns (tab 001);
# FALSE uses fmt_number(decimals = 2) instead (tab 002).

build_ranking_table <- function(name, meta, integer_cols = TRUE) {
  df <- read_fig(name) %>%
    mutate(Year  = as.character(Year),
           across(-Year, ~ suppressWarnings(as.numeric(.))))
  
  field_cols <- c("Physical Sciences", "Life Sciences",
                  "Engineering", "Computer Science")
  
  tbl <- df %>% gt(rowname_col = "Year")
  
  if (integer_cols) {
    tbl <- fmt_integer(tbl, columns = all_of(field_cols))
  } else {
    tbl <- fmt_number(tbl, columns = all_of(field_cols), decimals = 2)
  }
  
  tbl %>%
    fmt_number(columns = Average, decimals = 2) %>%
    sub_missing(missing_text = "—") %>%
    gt_color_rows(columns  = all_of(field_cols),
                  palette  = c(TPA_RED_LIGHT, tpa_colors[1]),
                  pal_type = "continuous") %>%
    cols_align(align   = "right",
               columns = all_of(c(field_cols, "Average"))) %>%
    tab_style(style     = cell_text(weight = "bold"),
              locations = cells_body(columns = Average)) %>%
    theme_gt_tpa(meta = meta) %>%
    save_table(name)
}

# ---- EB visa wait time tables -------------------------------
# Used by tabs 003, 004, 005.
# Expects a CSV with Year, EB1, EB1.1, EB2, EB2.1, EB3, EB3.1 columns.
# Merges each numeric/label pair, colors rows on a shared domain
# anchored to the max of EB3 (the longest waits).

build_eb_table <- function(name, meta) {
  df <- read_fig(name) %>%
    mutate(
      Year = as.character(Year),
      EB1  = as.numeric(EB1),
      EB2  = as.numeric(EB2),
      EB3  = as.numeric(EB3)
    )
  
  df %>%
    gt(rowname_col = "Year") %>%
    cols_merge(columns = c(EB1, `EB1.1`), pattern = "{2}") %>%
    cols_merge(columns = c(EB2, `EB2.1`), pattern = "{2}") %>%
    cols_merge(columns = c(EB3, `EB3.1`), pattern = "{2}") %>%
    cols_label(EB1 = "EB1", EB2 = "EB2", EB3 = "EB3") %>%
    gt_color_rows(columns  = c(EB1, EB2, EB3),
                  palette  = c(TPA_RED_LIGHT, tpa_colors[1]),
                  pal_type = "continuous",
                  domain   = c(0, max(df$EB3, na.rm = TRUE))) %>%
    cols_align(align = "right", columns = c(EB1, EB2, EB3)) %>%
    theme_gt_tpa(meta = meta) %>%
    save_table(name)
}

# ---- Export -------------------------------------------------

save_table <- function(gt_tbl, name, size = "standard") {
  dir.create("final_tables", showWarnings = FALSE)
  
  dims <- switch(
    size,
    standard = SIZE_STANDARD,
    long     = SIZE_LONG,
    xlong    = SIZE_XLONG,
    stop("Unknown size: ", size, ". Use 'standard', 'long', or 'xlong'.")
  )
  
  png_path  <- file.path("final_tables", paste0(name, ".png"))
  html_path <- file.path("final_tables", paste0(name, ".html"))
  
  gtsave(gt_tbl, html_path)
  gtsave(gt_tbl, png_path,
         vwidth  = round(dims$w * TABLE_DPI),
         vheight = round(dims$h * TABLE_DPI),
         zoom    = 2)
  
  gt_tbl
}