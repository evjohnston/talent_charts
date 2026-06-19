# theme.R

font_add_google("Source Sans 3", "source", regular.wt = 400, bold.wt = 700)
showtext_auto()
showtext_opts(dpi = 300)

FONT_FAMILY <- "source"
DATA_DIR    <- "data"

# ---- Chart size presets -------------------------------------
SIZE_STANDARD <- list(w = 8.0, h = 5.5)
SIZE_LONG     <- list(w = 8.0, h = 11.0)
SIZE_TALL     <- list(w = 6.0, h = 8.0)
SIZE_WIDE     <- list(w = 12.0, h = 5.5)
SIZE_XLONG    <- list(w = 8.0, h = 22.0)
CHART_MARGIN  <- 0.15
TABLE_DPI     <- 175

# ---- Style constants ----------------------------------------

# linewidth
LINE_WIDTH       <- 1     # primary series
LINE_WIDTH_DENSE <- 0.9   # charts with 8+ overlapping series
LINE_WIDTH_FACET <- 0.7   # small facet panels

# alpha
ALPHA_FILL      <- 0.95   # area fills and grouped bars
ALPHA_COL       <- 0.90   # standard bar/col
ALPHA_COL_LIGHT <- 0.60   # bars behind a line overlay
ALPHA_TREND     <- 0.80   # geom_smooth trend lines
ALPHA_BUBBLE    <- 0.75   # overlapping bubble/point charts

# pad_right (passed to scale_x_years)
PAD_RIGHT_NARROW   <- 0.18   # few series, short labels
PAD_RIGHT_STANDARD <- 0.20   # default
PAD_RIGHT_WIDE     <- 0.30   # many label characters needed
PAD_RIGHT_XWIDE    <- 0.40   # very long labels or many series

# expand helpers
EXPAND_BAR       <- expansion(mult = c(0,    0.05))
EXPAND_BAR_TIGHT <- expansion(mult = c(0,    0.02))
EXPAND_LINE      <- expansion(mult = c(0.02, 0.05))
EXPAND_LINE_WIDE <- expansion(mult = c(0.05, 0.08))
EXPAND_LINE_TALL <- expansion(mult = c(0.05, 0.10))
EXPAND_NONE      <- expansion(mult = c(0,    0))     # heatmap tiles, 100%-share bars
EXPAND_BAR_TALL  <- expansion(mult = c(0,    0.10))  # discrete year-axis bars with tall text labels
EXPAND_BAR_XLARGE <- expansion(mult = c(0,   0.12))  # dodged bars with labels above that need extra room

# Line widths 
LINE_WIDTH        <- 1       # all geom_line data series
LINE_WIDTH_LIGHT  <- 0.9     # 8+ overlapping series
LINE_WIDTH_REF    <- 0.4     # geom_hline / geom_vline reference lines

# Alpha ----
ALPHA_AREA        <- 0.95    # geom_area (rename from ALPHA_FILL)
ALPHA_COL         <- 0.90    # standard geom_col
ALPHA_COL_LIGHT   <- 0.60    # bar layer in dual-axis bar+line
ALPHA_BUBBLE      <- 0.75    # geom_point bubble charts
ALPHA_TREND       <- 0.80    # geom_smooth trend overlays

# Point size 
POINT_SIZE        <- 2       # geom_point on line charts

# Bar widths 
BAR_WIDTH         <- 0.7     # standard geom_col width
BAR_WIDTH_NARROW  <- 0.65    # compact horizontal stacked bars (2–4 rows)
BAR_WIDTH_SINGLE  <- 0.4     # single-row horizontal bar

# Expand 
EXPAND_BAR        <- expansion(mult = c(0, 0.05))   # y starts at zero
EXPAND_BAR_TALL   <- expansion(mult = c(0, 0.10))   # zero-start + label headroom
EXPAND_BAR_TIGHT  <- expansion(mult = c(0, 0))      # renamed: was EXPAND_NONE; fixed y-limits, no padding needed
EXPAND_LINE       <- expansion(mult = c(0.02, 0.05))
EXPAND_LINE_WIDE  <- expansion(mult = c(0.05, 0.08))
EXPAND_LINE_TALL  <- expansion(mult = c(0.05, 0.10))
EXPAND_NONE       <- expansion(mult = c(0, 0))      # tile/heatmap axes
EXPAND_TIGHT_TOP  <- expansion(mult = c(0, 0.02))   # grouped bars, label clears bar top

# pad_right
PAD_RIGHT_TIGHT    <- 0.03   # dual-axis bar+line, no endpoint labels needed
PAD_RIGHT_NARROW   <- 0.18   # short endpoint labels
PAD_RIGHT_STANDARD <- 0.20   # labels up to ~15 chars
PAD_RIGHT_WIDE     <- 0.30   # country + value labels, wrapped text
PAD_RIGHT_XWIDE    <- 0.40   # very long wrapped labels

# ---- Colors -------------------------------------------------
tpa_colors <- c(
  "#8c1515", "#2F5D7C", "#F5B427",
  "#92A045", "#5E3762", "#F68F1F",
  "#5D4738", "#918881", "#B9B4AC"
)

arrow_fill <- c(
  "Total"         = tpa_colors[1],  
  "Undergraduate" = tpa_colors[2],
  "Graduate"      = tpa_colors[3],
  "Non-Degree"    = tpa_colors[4],
  "OPT"           = tpa_colors[5]
)

# Named single-use hex values
TPA_NAVY_DARK <- "#1F3D52"   # darkest navy, used in mobility gradient
TPA_RED_MID   <- "#C46C5D"   # mid red, used in mobility gradient
TPA_RED_LIGHT <- "#F4D6D6"   # light red, used as gradient low end in tables

level_colors <- c(
  "Bachelors" = "#F5B427",
  "Masters"   = "#2F5D7C",
  "Doctorate" = "#8C1515",
  "All Doctorates (IPEDS)" = "#8C1515",
  "Research Doctorates (SED)" = "#92A045"
)

acad_level_colors <- c(
  "Bachelors"              = tpa_colors[3],
  "Masters"                = tpa_colors[2],
  "Doctorate"              = tpa_colors[1],
  "OPT"                    = tpa_colors[4],
  "Associates"             = tpa_colors[5],
  "Non-degree"             = tpa_colors[6],
  "Graduate (unspecified)" = tpa_colors[7],
  "Professional"           = tpa_colors[8]
)

navy_tints <- c(
  "#A6C5DE",
  "#5C8AAE",
  tpa_colors[2]
)

field_colors <- c(
  "Biological and biomedical sciences" = tpa_colors[2],
  "Engineering"                        = tpa_colors[4],
  "Physical Sciences"                  = tpa_colors[3],
  "Social Sciences"                    = tpa_colors[1],
  "Health Sciences"                    = tpa_colors[5],
  "Computer Sciences"                  = tpa_colors[6],
  "Mathematics"                        = tpa_colors[7],
  "Psychology"                         = tpa_colors[8],
  "Non-sciences"                       = tpa_colors[9]
)

stem_gradient_colors <- c(
  "Agriculture"                = TPA_NAVY_DARK,
  "Engineering"                = tpa_colors[2],
  "Health professions"         = "#5C8AAE",
  "Math and computer science"  = "#A6C5DE",
  "Physical and life sciences" = "#CDDEEC",
  "Social sciences"            = "#E8A99E",
  "Non-STEM"                   = tpa_colors[1]
)

# Citizenship status — used by figs 015-017, 090, 095-097.
# Standard mapping: red = US citizens, green = temp visa holders.
citizenship_colors <- c(
  "U.S. citizens and permanent residents" = tpa_colors[1],
  "Temporary visa holders"                = tpa_colors[4]
)

# Inverted citizenship mapping — used by figs 021, 025.
# Navy = citizens, red = TVH; same names, swapped colors.
citizenship_colors_alt <- c(
  "U.S. citizens and permanent residents" = tpa_colors[2],
  "Temporary visa holders"                = tpa_colors[1]
)

# H-1B denial palette — used by figs 073, 075, 087, 088.
hb_denial_colors <- c(
  "New Denials"     = tpa_colors[2],
  "Renewal Denials" = tpa_colors[1]
)

# China / US two-country palette — used by figs 044, 045, 047, 048.
china_us_colors <- c(
  "China"         = tpa_colors[1],
  "United States" = tpa_colors[2]
)

# Degree-level employment palette (four levels).
# Used by figs 027, 028. Extends level_colors with Professional.
degree_employment_colors <- c(
  "Doctorates"   = tpa_colors[1],
  "Professional" = tpa_colors[5],
  "Masters"      = tpa_colors[2],
  "Bachelors"    = tpa_colors[3]
)

# Researcher mobility gradient — used by figs 052, 053.
# The two "Inbound" keys are renamed at the chunk level to include
# the destination country ("US" or "China").
mobility_colors_base <- c(
  "Purely Domestic"                                 = TPA_NAVY_DARK,
  "Started Domestic, Trained Abroad, Returned"      = tpa_colors[2],
  "Inbound, Stayed"                                 = "#A6C5DE",
  "Started Domestic, Trained Abroad, Stayed Abroad" = "#E8A99E",
  "Inbound, Left"                                   = TPA_RED_MID,
  "Purely Abroad"                                   = tpa_colors[1]
)


# Diverging heatmap gradient: tpa_colors[2] → "#F5F2EC" → tpa_colors[1]
# Use this order in scale_fill_gradientn for pct-change heatmaps (fig_004)
HEATMAP_MID <- "#F5F2EC"

# ---- Scale helpers ------------------------------------------
scale_color_fields <- function(...) scale_color_manual(values = field_colors, ...)
scale_fill_fields  <- function(...) scale_fill_manual(values = field_colors, ...)
scale_color_tpa    <- function(...) scale_color_manual(values = tpa_colors, ...)
scale_fill_tpa     <- function(...) scale_fill_manual(values = tpa_colors, ...)

scale_color_levels <- function(...) {
  scale_color_manual(values = level_colors, breaks = names(level_colors), ...)
}

scale_fill_levels <- function(reverse_legend = FALSE, ...) {
  scale_fill_manual(
    values = level_colors,
    breaks = names(level_colors),
    guide  = guide_legend(reverse = reverse_legend),
    ...
  )
}

scale_fill_citizenship <- function(...) {
  scale_fill_manual(values = citizenship_colors, ...)
}

scale_color_china_us <- function(...) {
  scale_color_manual(values = china_us_colors, ...)
}

# ---- Typography ---------------------------------------------
TITLE_SIZE      <- 14
SUBTITLE_SIZE   <- 10
CAPTION_SIZE    <- 10
AXIS_SIZE       <- 10
SWATCH_SIZE     <- 7.5
DATA_LABEL_SIZE <- 10
BASE_SIZE       <- 10

CHART_LINE <- "Chart: Hoover Institution Technology Policy Accelerator, 2026"

# ---- Theme helpers ------------------------------------------
base_text <- function(size = AXIS_SIZE, color = "grey25", face = NULL) {
  element_text(family = FONT_FAMILY, size = size, color = color, face = face)
}

# ---- Theme --------------------------------------------------
theme_tpa <- function() {
  theme_minimal(base_size = BASE_SIZE, base_family = FONT_FAMILY) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_textbox_simple(
        family     = FONT_FAMILY,
        face       = "bold",
        size       = TITLE_SIZE,
        color      = "black",
        lineheight = 1.15,
        margin     = margin(b = 5)
      ),
      plot.subtitle = element_textbox_simple(
        family     = FONT_FAMILY,
        size       = SUBTITLE_SIZE,
        color      = "grey45",
        lineheight = 1.3,
        margin     = margin(b = 14)
      ),
      plot.caption = element_textbox_simple(
        family = FONT_FAMILY,
        size   = CAPTION_SIZE,
        color  = "grey45",
        hjust  = 0,
        margin = margin(t = 12)
      ),
      
      axis.title.x = element_blank(),
      axis.title.y = element_text(
        family = FONT_FAMILY,
        margin = margin(r = 10),
        color  = "grey25",
        size   = AXIS_SIZE
      ),
      axis.text   = base_text(),
      axis.text.x = element_text(
        family = FONT_FAMILY,
        color  = "grey25",
        size   = AXIS_SIZE,
        angle  = 45,
        hjust  = 1,
        margin = margin(t = 4)
      ),
      
      axis.line         = element_line(color = "grey50", linewidth = 0.4),
      axis.ticks        = element_line(color = "grey50", linewidth = 0.35),
      axis.ticks.length = unit(3, "pt"),
      
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      
      legend.position      = "bottom",
      legend.justification = "center",
      legend.margin        = margin(t = 8, b = 4),
      legend.title         = element_blank(),
      legend.text          = base_text(),
      legend.key.width     = unit(SWATCH_SIZE, "pt"),
      legend.key.height    = unit(SWATCH_SIZE, "pt"),
      
      plot.margin = margin(
        CHART_MARGIN * 72,
        CHART_MARGIN * 72,
        CHART_MARGIN * 72,
        CHART_MARGIN * 72
      )
    )
}

# ---- Heatmap variant ----------------------------------------
theme_heatmap <- function() {
  theme_tpa() +
    theme(
      panel.grid = element_blank(),
      axis.line  = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(
        family = FONT_FAMILY,
        color  = "grey25",
        size   = AXIS_SIZE,
        angle  = 0,
        hjust  = 0.5
      ),
      legend.position   = "right",
      legend.key.height = unit(40, "pt"),
      legend.key.width  = unit(10, "pt"),
      legend.title      = base_text()
    )
}

# ---- Faceted variant ----------------------------------------
theme_facet <- function() {
  theme_tpa() +
    theme(
      strip.text = element_text(
        family = FONT_FAMILY,
        face   = "bold",
        size   = AXIS_SIZE,
        color  = "grey20",
        hjust  = 0,
        margin = margin(b = 4)
      ),
      strip.background = element_blank(),
      panel.spacing.x  = unit(20, "pt"),
      panel.spacing.y  = unit(16, "pt"),
      axis.text.x = element_text(
        family = FONT_FAMILY,
        color  = "grey25",
        size   = AXIS_SIZE - 1,
        angle  = 45,
        hjust  = 1
      ),
      axis.text.y = base_text(size = AXIS_SIZE - 1)
    )
}

# ---- Observed/Projected variant -----------------------------
theme_op <- function() {
  theme_tpa() + theme(legend.key.width = unit(40, "pt"))
}

# ---- Dual-axis variant --------------------------------------
theme_dual_axis <- function() {
  theme_tpa() +
    theme(
      axis.title.y.right = element_text(
        family = FONT_FAMILY,
        margin = margin(l = 10),
        color  = "grey25",
        size   = AXIS_SIZE
      )
    )
}

# ---- Horizontal bar variant ---------------------------------
theme_horizontal_bar <- function() {
  theme_tpa() +
    theme(
      axis.text.x = element_text(
        family = FONT_FAMILY,
        color  = "grey25",
        size   = AXIS_SIZE,
        angle  = 0,
        hjust  = 0.5,
        margin = margin(t = 4)
      ),
      axis.title.y       = element_blank(),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      axis.line.x        = element_line(color = "grey50", linewidth = 0.4),
      axis.line.y        = element_blank(),
      axis.ticks.x       = element_line(color = "grey50", linewidth = 0.35),
      axis.ticks.y       = element_blank()
    )
}

# ---- Donut/pie variant --------------------------------------
theme_donut <- function() {
  theme_tpa() +
    theme(
      axis.title  = element_blank(),
      axis.text   = element_blank(),
      axis.line   = element_blank(),
      axis.ticks  = element_blank(),
      panel.grid  = element_blank(),
      legend.position = "bottom"
    )
}

# ---- Block-arrow variant ------------------------------------
theme_arrows <- function() {
  theme_tpa() +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.text.x = element_blank(),   # theme_tpa sets this separately; override it
      axis.ticks = element_blank(),
      axis.line  = element_blank(),
      panel.grid = element_blank()
    )
}

# ---- Cleveland dot-plot variant -----------------------------
theme_dotplot <- function() {
  theme_tpa() +
    theme(
      axis.title   = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x  = element_blank(),
      axis.line.y  = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
      axis.text.y = element_text(
        family = FONT_FAMILY, color = "grey25", size = AXIS_SIZE,
        lineheight = 0.9, margin = margin(r = 8)     # smaller r = labels closer to panel
      ),
      legend.justification = "right"
    )
}

# ---- Bullet / progress-bar variant --------------------------
theme_bullet <- function() {
  theme_tpa() +
    theme(
      axis.title  = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks  = element_blank(),
      axis.line   = element_blank(),
      panel.grid  = element_blank(),
      axis.text.y = element_text(
        family = FONT_FAMILY, color = "grey25",
        size = AXIS_SIZE, lineheight = 0.9
      ),
      strip.text = element_text(
        family = FONT_FAMILY, face = "bold",
        size = TITLE_SIZE, color = "grey15",
        hjust = 0.5, margin = margin(b = 0)
      ),
      strip.background = element_blank(),
      panel.spacing.x  = unit(28, "pt"),
      legend.position  = "none"
    )
}