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

# ---- Regional enrollment change table -----------------------
# Expects a CSV with:
# Year, Asia, Africa, Sub-saharan, Europe,
# Latin America and Carribian, North America,
# Oceania, Middle East and North Africa, Stateless

build_region_table <- function(name, meta) {
  
  df <- read_fig(name)
  
  first_year <- min(df$Year, na.rm = TRUE)
  last_year <- max(df$Year, na.rm = TRUE)
  
  first <- df %>%
    filter(Year == first_year)
  
  last <- df %>%
    filter(Year == last_year)
  
  region_cols <- c(
    "Asia",
    "Africa, Sub-Saharan",
    "Europe",
    "Latin America and Caribbean",
    "North America",
    "Oceania",
    "Middle East and North Africa"
  )
  
  first_total <- sum(first[region_cols], na.rm = TRUE)
  last_total <- sum(last[region_cols], na.rm = TRUE)
  
  tbl_df <- tibble(
    Region = region_cols,
    
    FirstYearCount = as.numeric(first[1, region_cols]),
    
    FirstYearShare =
      100 * as.numeric(first[1, region_cols]) / first_total,
    
    LastYearCount = as.numeric(last[1, region_cols]),
    
    LastYearShare =
      100 * as.numeric(last[1, region_cols]) / last_total
    
  ) %>%
    mutate(
      PctChangeCount =
        ((LastYearCount - FirstYearCount) / FirstYearCount) * 100,
      
      PctChangeShare =
        ((LastYearShare - FirstYearShare) / FirstYearShare) * 100
    ) %>%
    arrange(desc(PctChangeShare))
  
  count_lim <- max(abs(tbl_df$PctChangeCount), na.rm = TRUE)
  share_lim <- max(abs(tbl_df$PctChangeShare), na.rm = TRUE)
  
  tbl_df %>%
    gt(rowname_col = "Region", id = name) %>%
    
    fmt_integer(
      columns = c(FirstYearCount, LastYearCount)
    ) %>%
    
    fmt_number(
      columns = c(FirstYearShare, LastYearShare),
      decimals = 1,
      pattern = "{x}%"
    ) %>%
    
    fmt_number(
      columns = c(PctChangeCount, PctChangeShare),
      decimals = 1,
      force_sign = TRUE,
      pattern = "{x}%"
    ) %>%
    
    tab_spanner(
      label = "Enrollment",
      columns = c(
        FirstYearCount,
        LastYearCount,
        PctChangeCount
      )
    ) %>%
    
    tab_spanner(
      label = "Share of Total",
      columns = c(
        FirstYearShare,
        LastYearShare,
        PctChangeShare
      )
    ) %>%
    
    cols_label(
      FirstYearCount = as.character(first_year),
      LastYearCount  = as.character(last_year),
      PctChangeCount = "Change",
      
      FirstYearShare = as.character(first_year),
      LastYearShare  = as.character(last_year),
      PctChangeShare = "Change"
    ) %>%
    
    data_color(
      columns = PctChangeCount,
      fn = scales::col_numeric(
        palette = c(tpa_colors[2], "white", tpa_colors[1]),
        domain = c(-count_lim, count_lim)
      )
    ) %>%
    
    data_color(
      columns = PctChangeShare,
      fn = scales::col_numeric(
        palette = c(tpa_colors[2], "white", tpa_colors[1]),
        domain = c(-share_lim, share_lim)
      )
    ) %>%
    
    cols_align(
      align = "right",
      columns = c(
        FirstYearCount,
        LastYearCount,
        PctChangeCount,
        FirstYearShare,
        LastYearShare,
        PctChangeShare
      )
    ) %>%
    
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = c(PctChangeCount, PctChangeShare)
      )
    ) %>%
    
    tab_footnote(
      footnote = paste(
        "Counts represent international students by region of origin.",
        "Shares are calculated as a percentage of all international students.",
        "Change is the percent change between",
        first_year, "and", last_year, "."
      ),
      locations = cells_column_spanners(
        spanners = c("Enrollment", "Share of Total")
      )
    ) %>%
    
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1]
    )) %>%
    save_table(name, size = "long")
  
}

# ---- STEM labor-force citizenship table ----------------------
# Used by tab_013. Source is fig_025. One row per labor-force
# category; columns are the U.S.-citizen/permanent-resident share
# and the temporary-visa-holder share (they sum to 100%, so only
# the temp-visa column carries information and gets the color scale).
# Row ordering and the Total-last placement happen upstream.
build_labor_citizenship_table <- function(df, name, meta) {
  
  # single continuous scale on the temp-visa share -- the meaningful
  # number -- so the S&E-core vs. periphery contrast reads as color
  df %>%
    gt(rowname_col = "Category", id = name) %>%
    fmt_number(columns = c(Citizen, TVH), decimals = 1, pattern = "{x}%") %>%
    cols_label(Citizen = "U.S. citizen or\npermanent resident",
               TVH     = "Temporary\nvisa holder") %>%
    gt_color_rows(columns  = TVH,
                  palette  = c(TPA_RED_LIGHT, tpa_colors[1]),
                  pal_type = "continuous") %>%
    cols_align(align = "right", columns = c(Citizen, TVH)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = TVH)) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE)) %>%
    tab_footnote(
      footnote = paste(
        "Each row is the citizenship split within that labor-force category;",
        "the two shares sum to 100%. Rows are ordered from occupations most",
        "directly tied to science and engineering to those least tied, with",
        "the overall total shown last."
      ),
      locations = cells_column_labels(columns = TVH)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    save_table(name, size = "long")
}

# ---- H-1B top-10 employer table (union of both years) --------
# Used by tab_012. One flat table of every firm that ranked top-10
# in either the first or last year. Each row shows that firm's share
# in both years plus the relative change. Built from a df with
# Company, Y1, Y2, Change.
build_h1b_top10_table <- function(df, name, meta, first_year, last_year) {
  
  chg_lim <- max(abs(df$Change), na.rm = TRUE)
  
  df %>%
    gt(rowname_col = "Company", id = name) %>%
    fmt_number(columns = c(Y1, Y2), decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = Change, decimals = 1, force_sign = TRUE, pattern = "{x}%") %>%
    sub_missing(columns = Change, missing_text = "new") %>%
    cols_label(Y1 = as.character(first_year),
               Y2 = as.character(last_year),
               Change = "Change") %>%
    data_color(
      columns = Change,
      fn = scales::col_numeric(
        palette = c(tpa_colors[2], "white", tpa_colors[1]),
        domain  = c(-chg_lim, chg_lim),
        na.color = "white"
      )
    ) %>%
    cols_align(align = "right", columns = c(Y1, Y2, Change)) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = Change)) %>%
    tab_style(style = cell_text(align = "left"),
              locations = cells_stub(rows = TRUE)) %>%
    tab_footnote(
      footnote = paste0("Share of all H-1B approvals in NAICS 54 (professional, ",
                        "scientific, and technical services). Firms shown ranked ",
                        "among the ten largest employers in ", first_year, " or ",
                        last_year, ". Change is the relative change in share from ",
                        first_year, " to ", last_year, "."),
      locations = cells_column_labels(columns = Change)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    save_table(name, size = "standard")
}

# ---- Occupation share table (bookends + year-over-year change) ----
# Used by tab_011. Source data is fig_026. Takes a df already
# pivoted wide with Occupation, raw shares for the first and last
# year (level_cols, e.g. "Y2017", "Y2023"), year-over-year relative
# change columns for the interior years (chg_cols, e.g. "Chg2019",
# "Chg2021"), and a Change column (relative change, first to last
# year). Year selection, recoding, and row ordering happen upstream
# in the calculation chunk -- this function only builds and styles.
build_occupation_share_table <- function(df, name, meta, level_cols, chg_cols) {
  
  all_chg_cols <- c(chg_cols, "Change")
  span_order   <- c(level_cols[1], chg_cols, level_cols[2])
  numeric_cols <- c(level_cols, all_chg_cols)   # every non-stub column
  
  level_labels <- setNames(gsub("^Y", "", level_cols), level_cols)
  chg_labels   <- setNames(paste0(gsub("^Chg", "", chg_cols), " Δ"), chg_cols)
  
  # target table width, and column widths derived from it directly --
  # Occupation gets a fixed wider allotment, remaining space splits
  # evenly across every numeric column, so cols_width and table.width
  # can never drift out of sync with each other
  table_width_px <- SIZE_STANDARD$w * TABLE_DPI * 0.5 
  occ_width_px   <- 260
  num_width_px   <- (table_width_px - occ_width_px) / length(numeric_cols)
  
  tbl <- df %>%
    gt(rowname_col = "Occupation", id = name) %>%
    fmt_number(columns = all_of(level_cols), decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = all_of(all_chg_cols), decimals = 1,
               force_sign = TRUE, pattern = "{x}%") %>%
    tab_spanner(label = "Temporary visa holder share",
                columns = all_of(span_order)) %>%
    cols_label(!!!level_labels, !!!chg_labels, Change = "Net Change")
  
  for (col in all_chg_cols) {
    lim <- max(abs(df[[col]]), na.rm = TRUE)
    tbl <- tbl %>%
      data_color(
        columns = all_of(col),
        fn = scales::col_numeric(
          palette = c(tpa_colors[2], "white", tpa_colors[1]),
          domain  = c(-lim, lim)
        )
      )
  }
  
  tbl <- tbl %>%
    tab_style(
      style     = cell_fill(color = "white"),
      locations = cells_body(columns = all_of(level_cols))
    ) %>%
    tab_style(
      style     = cell_text(align = "left"),
      locations = cells_stub(rows = TRUE)
    ) %>%
    cols_align(align = "right", columns = c(all_of(level_cols), all_of(all_chg_cols))) %>%
    cols_width(rlang::new_formula(quote(Occupation), rlang::expr(px(!!occ_width_px))))
  
  for (col in numeric_cols) {
    tbl <- tbl %>%
      cols_width(rlang::new_formula(rlang::sym(col), rlang::expr(px(!!num_width_px))))
  }
  
  tbl %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(columns = Change)
    ) %>%
    tab_footnote(
      footnote = paste(
        "2017 is the temporary visa holder share of employment within each STEM",
        "occupation category. 2019, 2021, and 2023 show the relative change from",
        "the prior survey year rather than the share itself. Net Change is the",
        "relative change in share from 2017 to 2023. Rows are ordered from",
        "occupations most directly tied to science and engineering to those",
        "least tied."
      ),
      locations = cells_column_spanners(spanners = "Temporary visa holder share")
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1]
    )) %>%
    save_table(name, size = "standard")
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

# ---- Field-share change table -------------------------------
# Used by tab 008.
# Takes an already-assembled wide df with Country plus, for each field
# label, {label}_First / {label}_Last / {label}_Change columns
# (First = 2009 share, Last = 2024 share, Change = relative change).
# field_labels is the character vector of spanner labels (also the
# column-name prefixes). Each Change column gets its own symmetric
# color domain because per-field change magnitudes differ a lot.

build_field_share_table <- function(df, name, meta, field_labels) {
  change_cols <- paste0(field_labels, "_Change")
  value_cols  <- c(paste0(field_labels, "_First"),
                   paste0(field_labels, "_Last"))
  
  tbl <- df %>%
    gt(rowname_col = "Country", id = name) %>%
    fmt_number(columns = ends_with("_First"),  decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = ends_with("_Last"),   decimals = 1, pattern = "{x}%") %>%
    fmt_number(columns = ends_with("_Change"),
               decimals = 1, force_sign = TRUE, pattern = "{x}%") %>%
    sub_missing(missing_text = "—")
  
  for (lab in field_labels) {
    tbl <- tbl %>%
      tab_spanner(label = lab, columns = starts_with(paste0(lab, "_")))
  }
  
  tbl <- tbl %>%
    cols_label(
      ends_with("_First")  ~ "2009",
      ends_with("_Last")   ~ "2024",
      ends_with("_Change") ~ "Change"
    )
  
  # per-field diverging gradient: blue (decline) -> white (0) -> red (rise)
  for (col in change_cols) {
    lim <- max(abs(df[[col]]), na.rm = TRUE)
    tbl <- tbl %>%
      data_color(
        columns = all_of(col),
        fn = scales::col_numeric(
          palette = c(tpa_colors[2], "white", tpa_colors[1]),
          domain  = c(-lim, lim)
        )
      )
  }
  
  tbl %>%
    tab_style(
      style     = cell_fill(color = "white"),
      locations = cells_body(columns = all_of(value_cols))
    ) %>%
    cols_align(align = "right", columns = -Country) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(columns = all_of(change_cols))
    ) %>%
    cols_width(
      Country              ~ px(150),
      ends_with("_First")  ~ px(72),
      ends_with("_Last")   ~ px(72),
      ends_with("_Change") ~ px(88)
    ) %>%
    tab_footnote(
      footnote = paste(
        "Values are the share of each origin country's international students",
        "enrolled in the given field in 2009 and 2024, and the relative change",
        "between the two years. Countries are sorted by total international",
        "student enrollment, with the largest first."
      ),
      locations = cells_column_spanners(spanners = field_labels)
    ) %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1]
    )) %>%
    save_table(name, size = "long")
}


build_citizenship_table <- function(df, name, meta,
                                    first_year, last_year,
                                    footnote = NULL,
                                    value_is_share = FALSE,
                                    share_group = FALSE) {
  
  change_cols <- c("USC_Change", "TVH_Change")
  value_cols  <- c("USC_First", "USC_Last", "TVH_First", "TVH_Last")
  if (share_group) {
    change_cols <- c(change_cols, "SHR_Change")
    share_vals  <- c("SHR_First", "SHR_Last")
  }
  
  tbl <- df %>%
    gt(rowname_col = "Field", id = name)
  
  if (value_is_share) {
    tbl <- tbl %>% fmt_number(columns = all_of(value_cols), decimals = 1, pattern = "{x}%")
  } else {
    tbl <- tbl %>% fmt_integer(columns = all_of(value_cols))
  }
  if (share_group) {
    tbl <- tbl %>% fmt_number(columns = all_of(share_vals), decimals = 1, pattern = "{x}%")
  }
  
  tbl <- tbl %>%
    fmt_number(columns = all_of(change_cols),
               decimals = 1, force_sign = TRUE, pattern = "{x}%") %>%
    tab_spanner(label = "U.S. citizens & permanent residents",
                columns = c(USC_First, USC_Last, USC_Change)) %>%
    tab_spanner(label = "Temporary visa holders",
                columns = c(TVH_First, TVH_Last, TVH_Change))
  
  if (share_group) {
    tbl <- tbl %>%
      tab_spanner(label = "International share",
                  columns = c(SHR_First, SHR_Last, SHR_Change))
  }
  
  tbl <- tbl %>%
    cols_label(
      USC_First = as.character(first_year), USC_Last = as.character(last_year),
      USC_Change = "Change",
      TVH_First = as.character(first_year), TVH_Last = as.character(last_year),
      TVH_Change = "Change"
    )
  if (share_group) {
    tbl <- tbl %>%
      cols_label(
        SHR_First = as.character(first_year), SHR_Last = as.character(last_year),
        SHR_Change = "Change"
      )
  }
  
  # per-group symmetric diverging gradient on each change column
  for (col in change_cols) {
    lim <- max(abs(df[[col]]), na.rm = TRUE)
    tbl <- tbl %>%
      data_color(
        columns = all_of(col),
        fn = scales::col_numeric(
          palette = c(tpa_colors[2], "white", tpa_colors[1]),
          domain  = c(-lim, lim)
        )
      )
  }
  
  white_cols <- value_cols
  if (share_group) white_cols <- c(white_cols, share_vals)
  
  tbl <- tbl %>%
    tab_style(style = cell_fill(color = "white"),
              locations = cells_body(columns = all_of(white_cols))) %>%
    cols_align(align = "right", columns = -Field) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = all_of(change_cols))) %>%
    cols_width(
      Field                ~ px(210),
      ends_with("_First")  ~ px(64),
      ends_with("_Last")   ~ px(64),
      ends_with("_Change") ~ px(82)
    )
  
  if (!is.null(footnote)) {
    spanners <- c("U.S. citizens & permanent residents", "Temporary visa holders")
    if (share_group) spanners <- c(spanners, "International share")
    tbl <- tbl %>%
      tab_footnote(footnote = footnote,
                   locations = cells_column_spanners(spanners = spanners))
  }
  
  tbl %>%
    theme_gt_tpa(meta = meta) %>%
    opt_css(css = sprintf(
      "#%s .gt_column_spanner { border-bottom-color: %s !important; }",
      name, tpa_colors[1])) %>%
    save_table(name, size = "long")
}

# ---- Export -------------------------------------------------

save_table <- function(gt_tbl, name, size = "standard") {
  dir.create("final_tables", showWarnings = FALSE)
  
  dims <- switch(
    size,
    standard = SIZE_STANDARD,
    long     = SIZE_LONG,
    xlong    = SIZE_XLONG,
    short    = SIZE_SHORT,
    stop("Unknown size: ", size, ". Use 'standard', 'long', 'xlong', 'short'.")
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