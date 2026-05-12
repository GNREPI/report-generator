# File: Rockdale plots script.R----
# Purpose:
# Author: Allene Stephens
# Date: 01.28.2026
# These scripts are what you run before you open your R Markdown document.
# You typically run them once and save the output.
#-------------------------------------------------------------------------------


# Packages ----------------------------------------------------------------
# Custom Functions----
# 1. Setup and Environment ----
# Loads required packages and custom functions.
# The 'packages.R' script should contain all library calls.
source("R/packages.R")
source("R/brand_theme.R")
source(here("R/functions.R"))
source(here("R", "plot_accessibility.R"))

# ----Define your constants ----
# Define the county name and the disease program names
# Define the specific county and program you are currently processing
select_county <- "Rockdale"
program_list <- c("Vaccine Preventable", "Vectorborne", "Enteric")

# Define the demographics you want to chart
demographic_list <- c("gender_pie", "race_pie", "ethnicity_pie")



# Data Import ----
Rockdale_data <- qread(here("output", "combo_deidentified_clean.qs")) %>%
  filter(county == select_county)



# --- Branding Validation Check ---
current_theme <- getOption("plotly_template")

if (!is.null(current_theme)) {
  message("✅ Branding Active: Using ", current_theme$layout$font$family)
} else {
  warning("⚠️ WARNING: No brand template detected. Plots will use default Plotly styles!")
}

# ------------------------------------------------------------------
# Generate & Save Bar Plots for All Programs
# ------------------------------------------------------------------
source(here("R/functions.R"))

bar_plot_list <- list()

# 1. outter loop: Loop through each program in your list ("Vaccine Preventable", "Vectorborne", "Enteric")
for (prog_name in program_list) {
  message(paste("🔄 Processing:", prog_name, "..."))

  # 2. Filter Data
  # Note: We start with the base Rockdale data
  current_data <- Rockdale_data %>%
    filter(disease_program == prog_name)


  # Ensure we have data before proceeding to prevent errors
  if (nrow(current_data) > 0) {

    # --- SECTION A: MAIN BAR CHART ---
    # Calculate the rates for the program (Data is already filtered to Rockdale)


    # Generate Alt Text
    current_alt_text <- county_alt_text_string(current_data, prog_name)


    # 5. Generate the Plot Object
    percent_object <- percent_chart(
      data = current_data,
      disease_program_filter = prog_name,
      title_text = paste("Percent of ", prog_name, "Cases by Disease Reported", "<br>", " in ", select_county, " County, 2020 - 2024"),
      county_alt_text = current_alt_text
    )
    # 6. Save the Object to your LIST so you can view them later
    bar_plot_list[[prog_name]] <- percent_object
    # 6. Save the Object
    # The 'type' is "bar" for these main rate charts
    save_load_plot(percent_object, select_county, prog_name, type = "bar")

  } else {
    warning(paste("⚠️ No data found for", prog_name, "- skipping plot generation."))
  }
}

message("✅ All program plots processed and saved!")
# This depends on how your specific save_load_plot function is written

# Enteric:
bar_plot_list$Enteric$plot
bar_plot_list[["Vaccine Preventable"]]$plot
bar_plot_list$Vectorborne$plot

# ------------------------------------------------------------------
# Generate & Save Timeseries Plots for All Programs
# ------------------------------------------------------------------
line_chart_list <- list()

# 1. outter loop: Loop through each program in your list ("Vaccine Preventable", "Vectorborne", "Enteric")
for (prog_name in program_list) {
  message(paste("🔄 Processing:", prog_name, "..."))

  # 2. Filter Data
  # Note: We start with the base Rockdale data
  three_yr <- Rockdale_data %>%
    filter(disease_program == prog_name) %>%
    filter(onset_year %in% c(2024, 2023, 2022))


  # Ensure we have data before proceeding to prevent errors
  if (nrow(three_yr) > 0) {

    # --- SECTION A: MAIN BAR CHART ---
    # Calculate the rates for the program (Data is already filtered to Rockdale)
    # This runs once per program
    current_rates3 <- three_yr %>%
      county_rates(county_name = select_county)

    # Generate Alt Text
    current_alt_text <- county_alt_text_string(current_rates3, prog_name)

    current_3rates <- get_top_diseases(current_rates3)

    if (nrow(current_3rates) > 0) {

    # 5. Generate the Plot Object
    time_object <- county_time_plot(
      data = current_3rates,
      title_text = paste("Case Rates for the Top 3 Reported",
                         prog_name, " Diseases", "<br>", " in ", select_county, " County, 2022 - 2024"),
      disease_program = prog_name,
      alt_text = current_alt_text
    )
  } else {
    warning("No 3-year top disease data for ", prog_name, "- skipping trend plot")
  }
    # 6. Save the Object to your LIST so you can view them later
    line_chart_list[[prog_name]] <- time_object

    # 6. Save the Object
    # The 'type' is "bar" for these main rate charts
    save_load_plot(time_object, select_county, prog_name, type = "trend")

  } else {
    warning(paste("⚠️ No data found for", prog_name, "- skipping plot generation."))
  }
}

message("✅ All program plots processed and saved!")
# This depends on how your specific save_load_plot function is written

time_object$plot
line_chart_list$Enteric$plot
line_chart_list[["Vaccine Preventable"]]$plot
line_chart_list$Vectorborne$plot


