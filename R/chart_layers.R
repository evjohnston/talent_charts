# chart_layers.R

# ---- Text / label layers ------------------------------------

geom_label_repel_tpa <- function(
    data,
    mapping,
    with_segment = FALSE,
    direction    = "y",
    ...
) {
  ggrepel::geom_text_repel(
    data               = data,
    mapping            = mapping,
    size               = DATA_LABEL_SIZE / .pt,
    fontface           = "bold",
    family             = FONT_FAMILY,
    direction          = direction,
    box.padding        = 0.25,
    point.padding      = 0.25,
    min.segment.length = if (with_segment) 0 else Inf,
    segment.alpha      = if (with_segment) 0.5 else NA,
    segment.size       = 0.35,
    show.legend        = FALSE,
    ...
  )
}

geom_text_tpa <- function(data, mapping, color = NULL, ...) {
  layer_args <- list(
    data     = data,
    mapping  = mapping,
    size     = DATA_LABEL_SIZE / .pt,
    fontface = "bold",
    family   = FONT_FAMILY,
    vjust    = -0.6,
    ...
  )
  
  if (!is.null(color)) layer_args$color <- color
  
  do.call(geom_text, layer_args)
}

# ---- Axis scales --------------------------------------------

scale_x_years <- function(years, by = NULL, pad_right = 0.01) {
  span <- diff(range(years))
  
  by <- by %||% (
    if (span > 50) 5
    else if (span > 20) 2
    else 1
  )
  
  scale_x_continuous(
    breaks = seq(min(years), max(years), by),
    expand = expansion(mult = c(0.01, pad_right))
  )
}

# ---- Observed / projected lines -----------------------------

geom_op_lines <- function(df, linewidth = LINE_WIDTH, dot_pattern = "dotted") {
  list(
    geom_line(
      data      = filter(df, period == "Observed"),
      aes(linetype = period),
      linewidth = linewidth
    ),
    geom_line(
      data      = filter(df, period == "Projected"),
      aes(linetype = period),
      linewidth = linewidth
    ),
    scale_linetype_manual(
      name   = NULL,
      values = c("Observed" = "solid", "Projected" = dot_pattern),
      breaks = c("Observed", "Projected")
    ),
    guides(
      linetype = guide_legend(
        override.aes = list(color = "grey35", linewidth = 1.4)
      )
    )
  )
}

scale_linetype_observed_projected <- function(dot_pattern = "dotted") {
  list(
    scale_linetype_manual(
      name   = NULL,
      values = c("Observed" = "solid", "Projected" = dot_pattern),
      breaks = c("Observed", "Projected")
    ),
    guides(
      linetype = guide_legend(
        override.aes = list(color = "grey35", linewidth = 1.4)
      )
    )
  )
}

geom_hbar_segments <- function(df, fill_col, bar_height = 0.35) {
  geom_rect(
    data = df,
    aes(xmin = share_start,
        xmax = share_end,
        ymin = as.numeric(Category) - bar_height,
        ymax = as.numeric(Category) + bar_height,
        fill = {{ fill_col }})
  )
}

annotate_projected <- function(
    start_x,
    end_x,
    fill    = "grey94",
    alpha   = 1,
    label_y = NULL,
    label_x = NULL,
    ymin    = -Inf,
    ymax    = Inf
) {
  layers <- list(
    annotate(
      "rect",
      xmin  = start_x - 0.5,
      xmax  = end_x + 0.5,
      ymin  = ymin,
      ymax  = ymax,
      fill  = fill,
      alpha = alpha
    )
  )
  
  if (!is.null(label_y)) {
    layers <- c(
      layers,
      list(
        annotate(
          "text",
          x        = label_x %||% start_x,
          y        = label_y,
          label    = "Projected",
          hjust    = 0,
          vjust    = 0,
          family   = FONT_FAMILY,
          size     = AXIS_SIZE / .pt,
          color    = "grey45",
          fontface = "italic"
        )
      )
    )
  }
  
  layers
}

annotate_milestones <- function(
    milestones,
    y_top,
    gap_low  = 0.30,
    gap_high = 0.58,
    bg_for   = NULL
) {
  if (is.null(bg_for)) bg_for <- function(year) "white"
  
  m <- milestones %>%
    mutate(
      label_ymin = case_when(
        nchar(label) > 40 ~ y_top * gap_low,
        TRUE              ~ y_top * gap_high
      ),
      label_ymax = y_top * 0.97,
      bg         = vapply(year, bg_for, character(1))
    )
  
  list(
    geom_segment(
      data = m,
      aes(x = year, xend = year, y = 0, yend = label_ymin),
      inherit.aes = FALSE,
      linetype    = "dotted",
      color       = "grey50",
      linewidth   = 0.3
    ),
    geom_segment(
      data = m,
      aes(x = year, xend = year, y = label_ymax, yend = y_top),
      inherit.aes = FALSE,
      linetype    = "dotted",
      color       = "grey50",
      linewidth   = 0.3
    ),
    geom_label(
      data          = m,
      aes(x = year, y = y_top * 0.96, label = label, fill = bg),
      inherit.aes   = FALSE,
      angle         = 90,
      hjust         = 1,
      vjust         = 0.5,
      family        = FONT_FAMILY,
      size          = (AXIS_SIZE - 3.5) / .pt,
      color         = "grey25",
      label.size    = NA,
      label.padding = unit(0.75, "lines"),
      label.r       = unit(0, "pt"),
      lineheight    = 0.95,
      show.legend   = FALSE
    ),
    scale_fill_identity()
  )
}

# ---- Dual-axis charts ---------------------------------------

dual_axis_scale <- function(primary_max, secondary_max) {
  primary_max / secondary_max
}

sec_axis_scaled <- function(scale_factor, name, labels = waiver()) {
  sec_axis(
    transform = ~ . / scale_factor,
    name      = name,
    labels    = labels
  )
}

# ---- Block-arrow chart helper -------------------------------
# Fat block arrows growing up (positive) or down (negative) from a
# y = 0 baseline, one per row. Vertex builder for a single arrow set.
arrow_polygons <- function(df, x_col, len_col,
                           head_len   = 2.2,
                           head_frac  = NULL,   # set to override head_len with a fraction
                           shaft_half = 0.07,
                           head_half  = 0.16) {
  cx <- as.numeric(dplyr::pull(df, {{ x_col }}))
  h  <- dplyr::pull(df, {{ len_col }})
  id <- seq_len(nrow(df))
  
  verts <- purrr::pmap(
    list(id = id, cx = cx, h = h),
    function(id, cx, h) {
      mag <- abs(h)
      hl  <- if (!is.null(head_frac)) mag * head_frac else head_len
      hl  <- min(hl, mag * 0.85)        # head never exceeds 85% of the arrow
      st  <- sign(h) * (mag - hl)        # y where the head starts
      tibble(
        .aid = id,
        vx = c(cx - shaft_half, cx - shaft_half, cx - head_half, cx,
               cx + head_half,  cx + shaft_half, cx + shaft_half),
        vy = c(0, st, st, h, st, st, 0)
      )
    }
  ) %>%
    bind_rows()
  
  verts %>%
    left_join(mutate(df, .aid = id), by = ".aid")
}

# Full layer set for a block-arrow chart. Drawn length is a compressed
# transform of the value (exponent < 1 stretches small arrows, squashes
# big ones) so +/-1% still renders a complete arrow, matching the
# non-linear Excel original. Labels carry the TRUE values. Returns a
# list of layers; add to ggplot() with labs + theme_arrows().
block_arrow_layers <- function(df, category_col, value_col, fills,
                               divider_after = NULL,
                               exponent    = 0.46,
                               length_mult = 3.5,
                               label_fmt   = function(v) sprintf("%+d%%", v),
                               head_len    = 2.2,
                               shaft_half  = 0.07,
                               head_half   = 0.16,
                               label_gap   = 1.3,
                               pad         = 3,
                               x_spacing   = 1,      # <1 packs categories closer
                               x_expand    = 0.08,   # was 0.6: outer whitespace
                               base_over   = 0.3) {  # was 0.45: baseline stub past ends
  d <- df %>%
    mutate(
      .cat  = {{ category_col }},
      .val  = {{ value_col }},
      .x    = as.integer(factor(.cat, levels = levels(.cat) %||% unique(.cat))) * x_spacing,
      .draw = sign(.val) * abs(.val)^exponent * length_mult
    )
  
  poly <- arrow_polygons(d, .x, .draw,
                         head_len   = head_len,
                         shaft_half = shaft_half,
                         head_half  = head_half)
  
  labs_df <- d %>%
    mutate(.lab   = label_fmt(.val),
           .lab_y = .draw + sign(.draw) * label_gap)
  
  y_hi <- max(d$.draw) + pad
  y_lo <- min(d$.draw) - pad
  
  divider <- if (!is.null(divider_after)) {
    list(annotate("segment",
                  x = (divider_after + 0.5) * x_spacing,
                  xend = (divider_after + 0.5) * x_spacing,
                  y = y_lo, yend = y_hi,
                  linetype = "dotted", color = "grey55", linewidth = 0.4))
  } else list()
  
  c(
    divider,
    list(
      geom_polygon(data = poly,
                   aes(vx, vy, group = .aid, fill = .cat)),
      geom_segment(aes(x = min(.x) - base_over, xend = max(.x) + base_over,
                       y = 0, yend = 0),
                   data = d, color = "grey10", linewidth = 1.4),
      geom_label(data = d,
                 aes(x = .x, y = 0, label = .cat),
                 fill = "white", label.size = NA,
                 label.padding = unit(5, "pt"),
                 color = "grey20", family = FONT_FAMILY,
                 size = AXIS_SIZE / .pt),
      geom_text(data = labs_df,
                aes(x = .x, y = .lab_y, label = .lab, color = .cat),
                fontface = "bold", family = FONT_FAMILY,
                size = DATA_LABEL_SIZE / .pt),
      scale_fill_manual(values = fills),
      scale_color_manual(values = fills),
      scale_x_continuous(breaks = NULL,
                         expand = expansion(add = c(x_expand, x_expand))),
      scale_y_continuous(expand = expansion(mult = c(0, 0))),
      coord_cartesian(ylim = c(y_lo, y_hi), clip = "off"),
      guides(fill = "none", color = "none")
    )
  )
}

# ---- Stacked-bar charts -------------------------------------

geom_col_stacked <- function(width = 0.7, alpha = ALPHA_COL) {
  geom_col(width = width, alpha = alpha)
}

# ---- Grouped column charts ----------------------------------

geom_col_grouped <- function(width = 0.7, dodge = 0.8, alpha = ALPHA_COL) {
  geom_col(position = position_dodge(width = dodge),
           width = width, alpha = alpha)
}

pos_dodge_grouped <- function(dodge = 0.8) {
  position_dodge(width = dodge)
}

# ---- Trend lines --------------------------------------------

geom_trend <- function(mapping = NULL, color = "grey40", linetype = "dotted",
                       linewidth = 0.5, alpha = ALPHA_TREND) {
  geom_smooth(mapping   = mapping,
              method    = "lm",
              se        = FALSE,
              color     = color,
              linetype  = linetype,
              linewidth = linewidth,
              alpha     = alpha)
}

# ---- Highlight palette --------------------------------------

highlight_colors <- function(all_values, highlighted,
                             palette = tpa_colors,
                             default = "grey80") {
  all_values <- unique(as.character(all_values))
  out        <- setNames(rep(default, length(all_values)), all_values)
  n          <- min(length(highlighted), length(palette))
  if (n > 0) out[highlighted[1:n]] <- palette[1:n]
  out
}

# ---- Boxlike distribution charts (figs 054-058) -------------

company_palette_ai <- c(
  "Anthropic" = tpa_colors[1],
  "DeepMind"  = tpa_colors[2],
  "DeepSeek"  = tpa_colors[4],
  "OpenAI"    = tpa_colors[5]
)

read_boxlike_authors <- function() {
  read_fig("figs_54through58")
}

plot_boxlike_fig <- function(df, metric, x_label, log_scale = TRUE,
                             breaks = waiver()) {
  stats <- summary_boxlike_stats(df, metric, use_log = log_scale)
  
  y_labs <- setNames(
    paste0(stats$Company, "  (n=", stats$n, ")"),
    as.character(stats$Company)
  )
  
  p <- ggplot(stats, aes(y = Company)) +
    geom_summary_boxlike(stats) +
    scale_fill_manual(values = company_palette_ai, guide = "none") +
    scale_y_discrete(labels = y_labs) +
    labs(x = x_label, y = NULL)
  
  if (log_scale) {
    p <- p +
      scale_x_log10(labels = label_comma(), breaks = breaks) +
      labs(x = paste0(x_label, " (log scale)"))
  } else {
    p <- p + scale_x_continuous(labels = label_comma(), breaks = breaks)
  }
  
  p + theme_horizontal_bar()
}

# ---- Summary box-like chart (whiskers + two-tone box + mean dot) ----

geom_summary_boxlike <- function(stats,
                                 row_height = 0.5,
                                 whisker_lw = 0.6,
                                 box_lw     = 0.5,
                                 mean_size  = 2.5) {
  half <- row_height / 4
  
  list(
    # left whisker: min to Q1
    geom_segment(data = stats,
                 aes(x = min_v, xend = q1, y = Company, yend = Company),
                 color       = "grey25",
                 linewidth   = whisker_lw,
                 inherit.aes = FALSE),
    # right whisker: Q3 to max
    geom_segment(data = stats,
                 aes(x = q3, xend = max_v, y = Company, yend = Company),
                 color       = "grey25",
                 linewidth   = whisker_lw,
                 inherit.aes = FALSE),
    # whisker caps
    geom_segment(data = stats,
                 aes(x    = min_v,
                     xend = min_v,
                     y    = as.numeric(Company) - half,
                     yend = as.numeric(Company) + half),
                 color       = "grey25",
                 linewidth   = whisker_lw,
                 inherit.aes = FALSE),
    geom_segment(data = stats,
                 aes(x    = max_v,
                     xend = max_v,
                     y    = as.numeric(Company) - half,
                     yend = as.numeric(Company) + half),
                 color       = "grey25",
                 linewidth   = whisker_lw,
                 inherit.aes = FALSE),
    # Q1 -> Median (light half)
    geom_rect(data = stats,
              aes(xmin = q1,
                  xmax = median_v,
                  ymin = as.numeric(Company) - half,
                  ymax = as.numeric(Company) + half,
                  fill = Company),
              alpha       = 0.45,
              color       = "grey25",
              linewidth   = box_lw,
              inherit.aes = FALSE),
    # Median -> Q3 (dark half)
    geom_rect(data = stats,
              aes(xmin = median_v,
                  xmax = q3,
                  ymin = as.numeric(Company) - half,
                  ymax = as.numeric(Company) + half,
                  fill = Company),
              color       = "grey25",
              linewidth   = box_lw,
              inherit.aes = FALSE),
    # mean dot
    geom_point(data = stats,
               aes(x = mean_v, y = Company),
               color       = "black",
               size        = mean_size,
               inherit.aes = FALSE)
  )
}

summary_boxlike_stats <- function(df, metric, use_log = FALSE) {
  x <- df %>%
    select(Company, value = all_of(metric)) %>%
    mutate(value = as.numeric(value)) %>%
    filter(!is.na(value))
  
  if (use_log) x <- filter(x, value > 0)
  
  x %>%
    group_by(Company) %>%
    summarise(
      n        = n(),
      mean_v   = mean(value),
      min_v    = min(value),
      q1       = quantile(value, 0.25),
      median_v = median(value),
      q3       = quantile(value, 0.75),
      max_v    = max(value),
      .groups  = "drop"
    ) %>%
    arrange(median_v) %>%
    mutate(Company = factor(Company, levels = Company))
}