get_world_basemap <- function(scale = "medium") {
  if (!exists(".tpa_world_cache", envir = .GlobalEnv)) {
    .tpa_world_cache <<- ne_countries(scale = scale, returnclass = "sf")
  }
  .tpa_world_cache
}

join_country_data <- function(df, by_col = "Country", match_type = "name") {
  world <- get_world_basemap()
  
  join_field <- switch(match_type,
                       name   = "name_long",
                       iso_a3 = "iso_a3",
                       stop("match_type must be 'name' or 'iso_a3'"))
  
  by_spec <- setNames(by_col, join_field)
  
  world %>%
    left_join(df, by = by_spec)
}

geom_choropleth <- function(data, mapping,
                            border_color = "white",
                            border_width = 0.2,
                            na_fill = "grey92") {
  list(
    geom_sf(data = data, mapping = mapping,
            color = border_color, linewidth = border_width),
    scale_fill_continuous(na.value = na_fill)
  )
}

geom_country_labels <- function(data, value_col = "Count",
                                color = "white", min_value = 1, ...) {
  labeled <- data %>%
    filter(.data[[value_col]] >= min_value)
  
  geom_sf_text(
    data = labeled,
    aes(label = .data[[value_col]]),
    color = color,
    fontface = "bold",
    family = FONT_FAMILY,
    size = DATA_LABEL_SIZE / .pt,
    ...
  )
}

theme_map <- function() {
  theme_tpa() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.line  = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}