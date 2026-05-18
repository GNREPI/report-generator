# -----
# District Timeseries Plot
#
#
# Author: Allene Stephens
# Date: 09/29/2025
#----

# 1. Setup and Environment ----
# Loads required packages and custom functions.
# The 'packages.R' script should contain all library calls.
# Load the 'here' package so it can be used later
library(here)
source(here("R","packages.R"))
source("R/functions.R")
source("R/brand_theme.R")



# Define your constants
# Define the specific  and program you are currently processing
select_program <- c("Vaccine Preventable", "Vectorborne", "Enteric")


# Data Import ----
combo <- qread(here::here("output", "combo_deidentified_clean.qs"))


# --- Branding Validation Check ---
current_theme <- getOption("plotly_template")

if (!is.null(current_theme)) {
  message("✅ Branding Active: Using ", current_theme$layout$font$family)
} else {
  warning("⚠️ WARNING: No brand template detected. Plots will use default Plotly styles!")
}



# # Get the specific program name from the vector
# dis_VPD_time <- select_program[1]

district_hep <- combo %>%
     filter(str_detect(disease, "^Hepatitis") |
            str_detect(disease, "^Perinatal"))
# 2. Generate the branded timeline plot using your custom functions
# (Assuming your pipeline function returns a list or an object containing the plot)
hep_plot_object <- district_hep %>%
  district_time() %>%
  district_time_plot()


# 3. Generate automated alt-text string for accessibility compliance
hep_alt_text <- district_alt_text_string(district_hep)

# 4. Save as a compliance-ready list with plot + screen reader text
vpds_district_plot <- list(
  plot = hep_plot_object,   # Pulls the active chart
  sr_text = hep_alt_text    # Pulls the generated WCAG text
)


saveRDS(vpds_district_plot, file = here("hepatitis.rds"))


# # print(district_hep)
#
#
# #   mutate(hep_cases = case_when(str_detect(disease, "^Hepatitis")~Hepatitis),
# #     group_by(hep_cases) %>%
# #   district_time() %>%
# #     district_time_plot())
# #
# # print(district_hep)
#
#
#   # Create a new column that captures the common part of the string.
#     # The regex '^Vaccine Preventable' matches exactly that string at the beginning (^).
#     # We use coalesce in case a value doesn't match the pattern.
#     Common_Group = str_extract(Variable, "^Vaccine Preventable")
#
#
#   # 2. Group by the new standardized column
#   group_by(Common_Group) %>%
#
#   # 3. Perform your calculation (e.g., counting total cases)
#   summarise(
#     Total_Cases = n(),
#     .groups = 'drop'
#   )
#
# # print(df_grouped)
#
# vpds_alt_text <- district_alt_text_string(district_VPD)
#
#
#
# # Save as a list with plot + sr_text
# vpds_district_plot <- list(
#   plot = vpds_district_object$plot,
#   sr_text = vpds_alt_text
# )
#
# #
# #
# #
# # GVPD <- plot_ly(
# # data = Gwinnett_VPD,
# #   x = ~onset_year,
# #   y = ~case_rate_per_100k,
# #   type = "scatter",
# #   mode = 'lines+markers', # This is correct for lines with points
# #   color = ~disease,
# #  # --- ADD THIS FOR CUSTOM HOVER TEXT ---
# #     hovertext = ~paste(
# #       "<b>Disease Type: </b>", disease, "<br>",
# #       "<b>Case Rate: </b>", round(case_rate_per_100k, 2), "<br>"),
# #     hoverinfo = "text" # Tells plotly to use the 'text' aesthetic for hover
# # ) %>%
# # layout(
# #       hoverlabel = list(
# #         bgcolor = "white",       # Background color of the tooltip (e.g., "white", "lightblue", "#E0FFFF")
# #         font = list(
# #           color = "black",     # Font color of the text in the tooltip (e.g., "black", "darkblue")
# #           size = 16,           # Font size (optional)
# #           family = "Arial"    # Font family (optional)
# #           # You cannot directly make it 'bold' here through a 'font' property.
# #           # Boldness within the tooltip text comes from the <b> HTML tags in your hovertext.
# #         ),
# #         bordercolor = "grey",  # Optional: Border color of the tooltip
# #             namelength = -1        # -1 displays the full name for 'color' variable in tooltip (e.g., 'disease')
# #           ),
# #           title = list(
# #             text = paste0("<b>Vaccine-Preventable Case Rates", '<br>', "in ", select_county,
# #                           " County, 2019 - 2024</b>"),
# #             font = list(family = "Helvetica", size = 14, color = "navy"),
# #             x = 0.5, y = 0.9, # Position (0 to 1, x=0.5 is center, y=0.95 is near top)
# #             xanchor = "center", yanchor = "top", # Align the top of the title with y=0.98
# #             pad = list(b = 30)), # Padding *below* the main chart title text itself
# #
# #           xaxis = list(
# #             title = list(
# #               text = "<b>Year</b>", # Bold the X-axis category name (title)
# #               font = list(family = "Calibri", size = 12, color = "black"),
# #               standoff = 20 # Add 20px space between X-axis labels and X-axis title
# #             ),
# #             tickangle = 0, # <--- 1. Rotate X-axis numbers (0 for horizontal numbers)
# #             tickfont = list(size = 12), # <--- 7. Adjust size and bold numbers on the X-axis
# #             zeroline = TRUE, # Hide the zero line
# #             showgrid = TRUE,  # Show grid lines
# #             gridcolor = "lightgrey",
# #             automargin = TRUE,
# #             minor = list(
# #               showgrid = TRUE,
# #               gridcolor = "lightgrey", # Or a slightly lighter shade if desired, e.g., "lightgray"
# #               dtick = 1                 # Defines the step between minor ticks.
# #               # If your major ticks are every 100, dtick=50 places minor ticks in between.
# #               # Adjust 'dtick' value based on your 'range' and desired density.
# #             )
# #           ),
# #           yaxis = list(
# #             title = list(
# #               text = "<b>Case Rates per <br> 100k population</b>",
# #               font = list(family = "Calibri", size = 12, color = "black"),
# #               standoff = 5), # Add 20px space between y-axis labels and y-axis title
# #             tickangle = 0,
# #             ticks ="outside", tickwidth=2, tickcolor='white', ticklen=5,
# #             tickfont = list(size = 16), # <--- 3. Bold Y-axis category names (tick labels)
# #             zeroline = FALSE, # Hide the zero line
# #             showgrid = TRUE,
# #             automargin = TRUE# <--- Ensures Y-axis labels have enough space and stay centered
# #             # For horizontal bars with categorical Y-axis, plotly's default
# #             # behavior is to center labels on the bars. automargin handles overflow.
# #           ),
# #
# #           showlegend = TRUE,
# #           #          --- Bar chart specific layout (if type="bar") ---
# #
# #           bargap = 0.5,
# #           margin = list(
# #             t = 100,
# #             b = 115,
# #             r = 50,
# #             l = 80
# #           ),
# #
# #           # --- ADD FOOTNOTES AS ANNOTATIONS HERE ---
# #           annotations = list(
# #             # Footnote 1: Data Source
# #             list(
# #               text = "<b>Data Source:</b> SENDSS Georgia Department of Public Health, 2019-2024",
# #               xref = "paper", # Position relative to the entire plot area (0 to 1)
# #               yref = "paper", # Position relative to the entire plot area (0 to 1)
# #               x = 0,         # X position (0 = left edge of plot)
# #               y = -0.25,     # Y position (negative values place it below the plot area)
# #               # Adjust 'y' based on your bottom margin and font size
# #               showarrow = FALSE, # Do not show an arrow
# #               xanchor = "left",  # Anchor the text to its left side
# #               yanchor = "bottom",  # Let plotly determine vertical anchor (usually bottom)
# #               font = list(size = 9, color = "gray50") # Smaller, muted font for footnote
# #             )))
# #
# # print(GVPD)
#
# # Call Plotting Function
# Gvpds_object <- generate_county_plots(
#   data = Gwinnett_VPD,
#   county_name = select_county,
#   disease_program = VPD_program,
#   title_text = paste(VPD_program, "Case Rates",  "in", select_county, "County"),
#   county_alt_text = Gvpds_alt_text
# )
#
#
# Gvpds_object
#
#
# #========================================================
# #VBDs
# #========================================================
# # Get the specific program name from the vector
# VBD_program <- select_program[2]
#
# Gwinnett_VBD <- Gwinnett_data %>%
#   filter(disease_program == VBD_program) %>%
#   county_rates(county_name = select_county)
#
# Gvbds_alt_text <- county_alt_text_string(Gwinnett_VBD, VBD_program)
#
# # Call Plotting Function
# Gvbds_object <- generate_county_plots(
#   data = Gwinnett_VBD,
#   county_name = select_county,
#   disease_program = VBD_program,
#   title_text = paste(VBD_program, "Case Rates",  "in", select_county, "County"),
#   county_alt_text = Gvbds_alt_text
# )
#
# Gvbds_object
#
# #========================================================
# #Enterics
# #========================================================
# # Get the specific program name from the vector
# Enteric_program <- select_program[3]
#
# Gwinnett_Enteric <- Gwinnett_data %>%
#   filter(disease_program == VBD_program) %>%
#   county_rates(county_name = select_county)
#
# Genteric_alt_text <- county_alt_text_string(Gwinnett_Enteric, Enteric_program)
#
# # Call Plotting Function
# Gvbds_object <- generate_county_plots(
#   data = Gwinnett_Enteric,
#   county_name = select_county,
#   disease_program = Enteric_program,
#   title_text = paste(Enteric_program, "Case Rates",  "in", select_county, "County"),
#   county_alt_text = Genteric_alt_text
# )
#
#
# # Time Series Plots
#
# plot_ly(
#   data = Gwinnett_Enteric,
#   x = ~onset_year,
#   y = ~case_rate_per_100k,
#   type = "scatter",
#   mode = 'lines+markers', # This is correct for lines with points
#   color = ~disease,
#
#   # --- ADD THIS FOR CUSTOM HOVER TEXT ---
#   hovertext = ~paste(
#     "<b>County: </b>", county, "<br>",
#     "<b>Disease: </b>", disease, "<br>",
#     "<b>Case rate: </b>", round(case_rate_per_100k, 2), "<br>"
#   ),
#   hoverinfo = "text" # Tells plotly to use the 'text' aesthetic for hover
# ) %>%
#
#   layout(
#     hoverlabel = list(
#       bgcolor = "white",       # Background color of the tooltip (e.g., "white", "lightblue", "#E0FFFF")
#       font = list(
#         color = "black",     # Font color of the text in the tooltip (e.g., "black", "darkblue")
#         size = 14,           # Font size (optional)
#         family = "Arial"     # Font family (optional)
#         # You cannot directly make it 'bold' here through a 'font' property.
#         # Boldness within the tooltip text comes from the <b> HTML tags in your hovertext.
#       ),
#       bordercolor = "grey",  # Optional: Border color of the tooltip
#       namelength = -1        # -1 displays the full name for 'color' variable in tooltip (e.g., 'disease')
#     ),
#     title = list(
#       text = paste0("<b>GNR </b>", Enteric_program, "<br> Case Rates, 2019 - 2024</b>"),
#       font = list(family = "Helvetic", size = 24, color = "navy"),
#       x = 0.5, y = 0.9, # Position (0 to 1, x=0.5 is center, y=0.95 is near top)
#       xanchor = "center", yanchor = "top", # Align the top of the title with y=0.98
#       pad = list(b = 30)), # Padding *below* the main chart title text itself
#
#     xaxis = list(
#       title = list(
#         text = "<b>Case Rates</b>", # Bold the X-axis category name (title)
#         font = list(family = "Calibri", size = 18, color = "black"),
#         standoff = 0 # Add 20px space between X-axis labels and X-axis title
#       ),
#       tickangle = 0, # <--- 1. Rotate X-axis numbers (0 for horizontal numbers)
#       tickfont = list(size = 12), # <--- 7. Adjust size and bold numbers on the X-axis
#       zeroline = FALSE, # Hide the zero line
#       showgrid = TRUE,  # Show grid lines
#       #range = c(0, NA), # Adjust this range based on your actual data's max case rate
#       gridcolor = "lightgrey",
#       automargin = TRUE,
#       minor = list(
#         showgrid = TRUE,
#         gridcolor = "lightgrey", # Or a slightly lighter shade if desired, e.g., "lightgray"
#         dtick = 1                 # Defines the step between minor ticks.
#         # If your major ticks are every 100, dtick=50 places minor ticks in between.
#         # Adjust 'dtick' value based on your 'range' and desired density.
#       )
#     ),
#     yaxis = list(
#       title = list(
#         text = "<b>Illness Type</b>",
#         font = list(family = "Calibri", size = 18, color = "black"),
#         standoff = 15), # Add 20px space between y-axis labels and y-axis title
#       tickangle = 0,
#       ticks ="outside", tickwidth=2, tickcolor='white', ticklen=5,
#       tickfont = list(size = 16), # <--- 3. Bold Y-axis category names (tick labels)
#       zeroline = TRUE, # Hide the zero line
#       showgrid = TRUE,
#       automargin = TRUE# <--- Ensures Y-axis labels have enough space and stay centered
#       # For horizontal bars with categorical Y-axis, plotly's default
#       # behavior is to center labels on the bars. automargin handles overflow.
#     ),
#
#     legend = list(
#       title = list(
#         text = "<b>Vaccine-Preventable<br> Illness Type</b>",
#         font = list(size = 12),
#         pad = list(b = 30)
#       ),
#       font = list(size = 12),
#
#       # --- Legend Positioning for Right Side, Centered to Plot ---
#       yref = "paper", # 'y' is relative to the entire paper/figure height (0-1)
#       y = 0.5, # Position vertically at the MIDDLE of the plot area (0 to 1 scale).
#       xref = "paper", # 'x' is relative to the entire paper/figure width (0-1)
#       x = 1, # Start just outside the right edge of the plot area (0 to 1 scale)
#       xanchor = "left", # Anchor the LEFT side of the legend box to the 'x' coordinate
#       yanchor = "middle", # Anchor the MIDDLE of the legend box to the 'y' coordinate
#       orientation = "v", # "v" (vertical, default) or "h" (horizontal)
#       bgcolor = 'rgba(255,255,255,0.7)', # Optional: Make legend background semi-transparent
#       bordercolor = "grey",
#       borderwidth = 1
#     ),
#
#     # --- ADD FOOTNOTES AS ANNOTATIONS HERE ---
#     annotations = list(
#       # Footnote 1: Data Source
#       list(
#         text = "<b>Data Source:</b> SENDSS Georgia Department of Public Health, 2019-2024",
#         xref = "paper", # Position relative to the entire plot area (0 to 1)
#         yref = "paper", # Position relative to the entire plot area (0 to 1)
#         x = 0,         # X position (0 = left edge of plot)
#         y = -0.25,     # Y position (negative values place it below the plot area)
#         # Adjust 'y' based on your bottom margin and font size
#         showarrow = FALSE, # Do not show an arrow
#         xanchor = "left",  # Anchor the text to its left side
#         yanchor = "bottom",  # Let plotly determine vertical anchor (usually bottom)
#         font = list(size = 9, color = "gray50") # Smaller, muted font for footnote
#       ),
#       # Footnote 2: Acknowledgment (Example)
#       list(
#         text = "<b>*Note</b>: Rates are per 100,000 population.",
#         xref = "paper",
#         yref = "paper",
#         x = 0,
#         y = -0.30, # Place this footnote slightly below the first one
#         showarrow = FALSE,
#         xanchor = "left",
#         yanchor = "bottom",
#         font = list(size = 9, color = "gray50")
#       )))
# # Add more list() elements for additional footnotes if
#
#
#
# # # Adjust height as needed
# #
# # plot_ly(
# #   data = district_disease_rates_by_year,
# #   x = ~onset_year,
# #   y = ~district_disease_rate_per_100k,
# #   type = "scatter",
# #   mode = 'lines+markers', # This is correct for lines with points
# #   color = ~disease,
# #   # --- ADD THIS FOR CUSTOM HOVER TEXT ---
# #   hovertext = ~paste(
# #     "<b>Disease Type: </b>", disease, "<br>",
# #     "<b>Case Rate: </b>", round(district_disease_rate_per_100k, 2), "<br>"
# #   ),
# #   hoverinfo = "text" # Tells plotly to use the 'text' aesthetic for hover
# # ) %>%
# #
# #   layout(
# #     hoverlabel = list(
# #       bgcolor = "white",       # Background color of the tooltip (e.g., "white", "lightblue", "#E0FFFF")
# #       font = list(
# #         color = "black",     # Font color of the text in the tooltip (e.g., "black", "darkblue")
# #         size = 16,           # Font size (optional)
# #         family = "Arial"     # Font family (optional)
# #         # You cannot directly make it 'bold' here through a 'font' property.
# #         # Boldness within the tooltip text comes from the <b> HTML tags in your hovertext.
# #       ),
# #       bordercolor = "grey",  # Optional: Border color of the tooltip
# #       namelength = -1        # -1 displays the full name for 'color' variable in tooltip (e.g., 'disease')
# #     ),
# #     title = list(
# #       text = "<b>GNR Vaccine-Preventable Illnesses in the District<br> Case Counts, 2019 - 2024</b>",
# #       font = list(family = "Helvetic", size = 24, color = "navy"),
# #       x = 0.5, y = 0.9, # Position (0 to 1, x=0.5 is center, y=0.95 is near top)
# #       xanchor = "center", yanchor = "top", # Align the top of the title with y=0.98
# #       pad = list(b = 30)), # Padding *below* the main chart title text itself
# #
# #     xaxis = list(
# #       title = list(
# #         text = "<b>Year</b>", # Bold the X-axis category name (title)
# #         font = list(family = "Calibri", size = 18, color = "black"),
# #         standoff = 0 # Add 20px space between X-axis labels and X-axis title
# #       ),
# #       tickangle = 0, # <--- 1. Rotate X-axis numbers (0 for horizontal numbers)
# #       tickfont = list(size = 12), # <--- 7. Adjust size and bold numbers on the X-axis
# #       zeroline = TRUE, # Hide the zero line
# #       showgrid = TRUE,  # Show grid lines
# #       gridcolor = "lightgrey",
# #       automargin = TRUE,
# #       minor = list(
# #         showgrid = TRUE,
# #         gridcolor = "lightgrey", # Or a slightly lighter shade if desired, e.g., "lightgray"
# #         dtick = 1                 # Defines the step between minor ticks.
# #         # If your major ticks are every 100, dtick=50 places minor ticks in between.
# #         # Adjust 'dtick' value based on your 'range' and desired density.
# #       )
# #     ),
# #     yaxis = list(
# #       title = list(
# #         text = "<b>Case Rate</b>",
# #         font = list(family = "Calibri", size = 18, color = "black"),
# #         standoff = 20), # Add 20px space between y-axis labels and y-axis title
# #       tickangle = 0,
# #       ticks ="outside", tickwidth=2, tickcolor='white', ticklen=5,
# #       tickfont = list(size = 16), # <--- 3. Bold Y-axis category names (tick labels)
# #       zeroline = FALSE, # Hide the zero line
# #       showgrid = TRUE,
# #       automargin = TRUE# <--- Ensures Y-axis labels have enough space and stay centered
# #       # For horizontal bars with categorical Y-axis, plotly's default
# #       # behavior is to center labels on the bars. automargin handles overflow.
# #     ),
# #
# #     legend = list(
# #       title = list(
# #         text = "<b>Vaccine-Preventable<br> Illness Type</b>",
# #         font = list(size = 12),
# #         pad = list(b = 30)
# #       ),
# #       font = list(size = 12),
# #
# #       # --- Legend Positioning for Right Side, Centered to Plot ---
# #       yref = "paper", # 'y' is relative to the entire paper/figure height (0-1)
# #       y = 0.5, # Position vertically at the MIDDLE of the plot area (0 to 1 scale).
# #       xref = "paper", # 'x' is relative to the entire paper/figure width (0-1)
# #       x = 1, # Start just outside the right edge of the plot area (0 to 1 scale)
# #       xanchor = "left", # Anchor the LEFT side of the legend box to the 'x' coordinate
# #       yanchor = "middle", # Anchor the MIDDLE of the legend box to the 'y' coordinate
# #       orientation = "v", # "v" (vertical, default) or "h" (horizontal)
# #       bgcolor = 'rgba(255,255,255,0.7)', # Optional: Make legend background semi-transparent
# #       bordercolor = "grey",
# #       borderwidth = 1
# #     ),
# #
# #     # --- ADD FOOTNOTES AS ANNOTATIONS HERE ---
# #     annotations = list(
# #       # Footnote 1: Data Source
# #       list(
# #         text = "<b>Data Source:</b> SENDSS Georgia Department of Public Health, 2019-2024",
# #         xref = "paper", # Position relative to the entire plot area (0 to 1)
# #         yref = "paper", # Position relative to the entire plot area (0 to 1)
# #         x = 0,         # X position (0 = left edge of plot)
# #         y = -0.25,     # Y position (negative values place it below the plot area)
# #         # Adjust 'y' based on your bottom margin and font size
# #         showarrow = FALSE, # Do not show an arrow
# #         xanchor = "left",  # Anchor the text to its left side
# #         yanchor = "bottom",  # Let plotly determine vertical anchor (usually bottom)
# #         font = list(size = 9, color = "gray50") # Smaller, muted font for footnote
# #       )))
# #
# #
# #
