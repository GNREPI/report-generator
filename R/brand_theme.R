# File: brand_theme.R----
# Purpose:
# Author: Allene Stephens
# Date: 12.17.2025

#-------------------------------------------------------------------------------

library(here)
# Packages are loaded separately in the Rmd setup chunk, so we skip sourcing here
# to avoid redundant p_load() checks that produce console clutter.
# source(here("R","packages.R"))


# ---------------------------
# Fonts
# ---------------------------


# 1. Register the font
# We use the third URL in your text because it is the OpenType format
sysfonts::font_add(
  family = "brandon-grotesque",
  regular = here("R", "brandon_bold.otf"))


# This command is CRANTIAL: it tells R to use showtext to render plots
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300) # Improves clarity for high-res exports

# 2. Create a reusable theme function; only works for ggplot. Won't affect plotly.
theme_brand <- function() {
  theme_minimal(base_size = 12) +
    theme(
      text = element_text(family = "brandon_grotesque_bold", "Arial"),
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank()
    )
}


# 3. Define your District Brand font string
brand_fonts <- list(

  size = 14,

chart_title = "Brandon Grotesque Black, Arial, Sans-Serif",

body = "Roboto, Arial, Sans-Serif",

hover = "Roboto Medium, Arial, Sans-Serif",

footnote = "Roboto Italic, Arial, Sans-Serif",

axis = "Roboto Medium, Arial, Sans-Serif")


# ---------------------------
# Colors
# ---------------------------

# Define the WCAG-compliant color sequence
surv_colors <- c(
  # Primary Brand Colors:
   "GNRgreen" = "#08786B",
   "GNRcoral" = "#f27866",
   "GNRgold" ="#faa81a",
   # Accessibility Colors
   "Aqua" = "#1B9E77",
   "Lavendar" = "#7570B3",
                "Orange" = "#D95F02",
   "Blue" = "#03045e",
   "Green" = "#006600",
                "Violet" = "#660066",
                "Teal" = "#008080",
               "Gold" = "#F9a602",
               # "Grey"   = "#666666",
               "Light_blue" = "#ADD8E6",
              # "Lavender" = "#B373bd",
              "Dark_brown" = "#3c1006",
             # "Banana" = "#f9cf6c",
            "Yellow" = "#fef250",
   "Brown" = "#A0522D"

)


# ---------------------------
# Patterns
# ---------------------------

# Define a standard sequence of accessible patterns
# brand_patterns <- c("", "/", ".", "x", "+", "\\")


# ---------------------------
# Plotly Templates
# ---------------------------

# 1. Create a global default template
plot_template <- list(

  layout = list(
    font = list(family = brand_fonts$body),
    colorway = surv_colors,

     paper_bgcolor = "white",
    plot_bgcolor = "#F8F9FA", # A very light grey to help the white gridlines pop

    title = list(font = list(family = brand_fonts$chart_title, size = 18, color = "#1a2a3a")),

    xaxis = list( automargin = TRUE,
      categoryorder = "total descending",
      title = list(font = list(family = brand_fonts$axis)),
                 tickfont = list(family = brand_fonts$body, size = 12, color = "#2B2B2B")),

    yaxis = list(title = list(font = list(family = brand_fonts$axis)),
                 tickfont = list(family = brand_fonts$body, size = 12, color = "#2B2B2B")
    ),

  legend = list(
    orientation = "h",
    x = 0.5,
    y = -0.15,
    xanchor = "center",
    font = list(family = brand_fonts$body, size = 12),
    title = list(text = "")),

  dragmode = "pan",
  hovermode = "closest"),

  hoverlabel = list(bgcolor = "#333333",       # Dark background for the tooltip
                    bordercolor = "#008080",   #
                    font = list(family = brand_fonts$hover, size = 15, color = "white")),


data = list(
  # 2. Pie Trace Defaults

   pie = list(
    hole = 0.5,              # Donut-ready; You can still override hole = 0 for pies.
    sort = FALSE,            # Preserve data order
    direction = "clockwise",
    textinfo = "percent",
    textposition = "outside",
    insidetextorientation = "radial",
    marker = list(
      line = list(color = "white", width = 2)
    )
  ),
  bar = list(
    marker = list(
      line = list(width = 0)
    ),
   hovertemplate =
      "<b>%{x}</b><br>%{y}<extra></extra>"),


  scatter = list(
    mode = "lines+markers",
    line = list(width = 3),
    marker = list(size = 7),
    hovertemplate =
      "<b>%{x}</b><br>%{y}<extra></extra>"),

  table = list(
    header = list(
      fill = list(color = "#E9ECEF"),
      font = list(family = brand_fonts$axis, size = 13),
      align = "left"
    ),
    cells = list(
      font = list(family = brand_fonts$body, size = 12),
      align = "left",
      height = 28
    )
  )
)
)


# If it affects how a trace type looks, it goes in data.
# If it affects the canvas or axes, it goes in layout.


# 4️⃣ Things you should NOT put in the template
#
# These belong in helpers or guards, not templates:
#
#   ❌ Axis ranges
# ❌ dtick logic
# ❌ Accessible color enforcement
# ❌ Data labels
# ❌ Annotations like “Data Source”
# ❌ Chart titles / subtitles
# ❌ Conditional formatting
#
# If it depends on the data, it does not belong in the template.

# ---------------------------
# Register everything globally
# ---------------------------


# Ensure the template is registered for Plotly
# 3. Set this as the default for all future plots in the session
options(
  plotly_template = plot_template)

get_brand <- function() {
  #This function simply gathers the variables you already defined above
  # and puts them into a list for easy access.
  return(list(
    colors = surv_colors,
    fonts = brand_fonts,
    template = plot_template
  ))
}