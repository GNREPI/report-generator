# File: generate_county_plots.R----
# Author: Allene Stephens
# Date: 10.20.2025
# This script performs iterations over counties and disease programs to
# generate and save individual Plotly widget files (.rds).
#-----------------------------------------------------------------------------------


# Packages and Custom Functions----
# 1. Setup and Environment ----
# Loads required packages and custom functions.
library(here)

source("R/packages.R")
source(here("R","brand_theme.R"))
source(here::here("R/functions.R"))

# --- Branding Validation Check ---
current_theme <- getOption("plotly_template")

if (!is.null(current_theme)) {
  message("✅ Branding Active: Using ", current_theme$layout$font$family)
} else {
  warning("⚠️ WARNING: No brand template detected. Plots will use default Plotly styles!")
}

# Data Import ----
# Load full dataset and define colors (accessible globally to the functions)
county_data <- qread(here("output", "combo_deidentified_clean.qs"))

# Define your constants
# Define the county name and the disease program names
county_name <- c("Gwinnett", "Newton", "Rockdale")

program_name <- c("Vectorborne", "Enteric", "Vaccine Preventable")

# --- Branding Validation Check ---
current_theme <- getOption("plotly_template")

if (!is.null(current_theme)) {
  message("✅ Branding Active: Using ", current_theme$layout$font$family)
} else {
  warning("⚠️ WARNING: No brand template detected. Plots will use default Plotly styles!")
}

accessible_colors <- surv_colors


# Create a data frame of all 9 combinations
county_program_combinations <- expand_grid(
  county = county_name,
  disease_program = program_name
)



exists("accessible_colors")
stopifnot(exists("accessible_colors"))

county_plot_list <- county_program_combinations %>%
  mutate(data_input = list(county_data)) %>%
  mutate(output = pmap(
    # Argument 1: The list of items to iterate over
    list(
    data_input = data_input,                 # list-column (recycled)
    county_name = county,                    # character vector (one per row)
    program_name = disease_program),        # character vector (one per row)

  # Argument 2: The function (The .f that was "missing")
      # explicit function avoids .x/.y/.z scoping problems
 function(data_input, county_name, program_name) {
    process_data_and_plot(
      data_input = data_input,
      county_name = county_name,
      program_name = program_name)}))



# 3. Extract Plot Objects and Save Files ---------------------------------------
# This pwalk loop extracts the plot object and saves it as an .rds file.
county_plot_list %>%
  # We don't need to extract names from 'output' because we already have
  # 'county' and 'disease_program' columns in this dataframe!
  # DATA FRAME COLUMNS ARE: 'output', 'county', 'disease_program'

  # Now, the pwalk function uses plot_object_list which contains plot and sr_text
  pwalk(function(output, county, disease_program, ...) {

    # Extract the actual plot/text list
    plot_bundle <- output$plot_object

    # Skip if there's no plot to save
    if (is.null(plot_bundle$plot)) {
      message("⏩ Skipping ", county, " ", disease_program, " (No Data)")
      return()
    }

    # 1. Clean the Program Name
    safe_program_name <- gsub(" ", "_", tolower(disease_program))
    safe_program_name <- gsub("-", "_", safe_program_name) # Ensure hyphens are also covered

    # 🌟 2. CLEAN AND STANDARDIZE THE COUNTY NAME 🌟
    safe_county_name <- gsub(" ", "_", tolower(county))
    safe_county_name <- gsub("-", "_", safe_county_name) # Ensure hyphens are also covered

    file_path <- here::here(
      "output",
      # 🌟 3. USE THE CLEANED COUNTY NAME 🌟
      paste0(safe_county_name, "_", safe_program_name, "_plot.rds")
    )
    # Check if the file path is valid before saving (Optional safety)
    if (length(file_path) > 0 && file_path != "") {
      saveRDS(plot_bundle, file_path)
      message("Saved: ", basename(file_path))
    } else {
      warning("Could not generate file path for ", county)
    }
  })




# # # Peek at the VPD plot
#  test_plot <- readRDS(here::here("output", "gwinnett_vaccine_preventable_plot.rds"))
# #
# # # Check the alt text
#  print(test_plot$sr_text)
# #
# # # Check the plot
#  test_plot$plot

