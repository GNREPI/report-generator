


# ----------------------------------------
# Plotly Accessibility Guard
# ----------------------------------------

lock_accessibility <- function(p,
                               colors,
                               y_range = NULL,
                               dtick = NULL,
                               font_family = "Arial") {

  p %>%
    layout(
      colorway = colors,
      font = list(family = font_family),
      title = list(font = list(color = "#000000")),
      xaxis = list(
        title = list(font = list(color = "#000000")),
        automargin = TRUE
      ),
      yaxis = list(
        title = list(font = list(color = "#000000")),
        range = y_range,
        dtick = dtick,
        automargin = TRUE,
        zeroline = TRUE,
        zerolinewidth = 2,
        zerolinecolor = "black"
      )
    )
}