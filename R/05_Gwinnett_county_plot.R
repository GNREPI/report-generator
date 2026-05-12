# File: Gwinnett plots script.R----
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
select_county <- "Gwinnett"
program_list <- c("Vaccine Preventable", "Vectorborne", "Enteric")

# Define the demographics you want to chart
demographic_list <- c("gender_pie", "race_pie", "ethnicity_pie")

county <- select_county   # define it so functions can use it


# Data Import ----
Gwinnett_data <- qread(here("output", "combo_deidentified_clean.qs")) %>%
  filter(county == select_county)



# --- Branding Validation Check ---
current_theme <- getOption("plotly_template")

if (!is.null(current_theme)) {
  message("✅ Branding Active: Using ", current_theme$layout$font$family)
} else {
  warning("⚠️ WARNING: No brand template detected. Plots will use default Plotly styles!")
}

# ------------------------------------------------------------------
# Generate & Save Percent Bar Plots for All Programs
# ------------------------------------------------------------------

bar_plot_list <- list()

for (prog_name in program_list) {
  message(paste("🔄 Processing:", prog_name, "..."))

  # 1. Initial Filter
  current_data <- Gwinnett_data %>%
    filter(disease_program == prog_name)

  # 2. GUARD CLAUSE: Skip if no rows exist
  if (nrow(current_data) == 0) {
    warning(paste("⚠️ Skipping", program_list, "- No rows found."))
    next
  }

  # 3. DATA PREP: Calculate case counts and percentages
  # We do this here so we can use these numbers for the Alt Text
  counting_summary <- current_data %>%
    group_by(disease) %>%
    summarise(case_count = n(), .groups = "drop") %>%
    mutate(
      percent = round((case_count / sum(case_count)) * 100, 1),
      label_text = paste0(percent, "%")
    ) %>%
    arrange(desc(case_count))

  # 4. Generate Alt Text
  # Note: Since we aren't doing rates, your alt_text function might need
  # to look at 'case_count' instead of 'case_rate_per_100k'
  percent_alt_text <- percent_alt_text_string(counting_summary, prog_name)

  # 5. Generate the Plot Object
  percent_object <- percent_chart(
    data = current_data,
    id_col = "disease",
    disease_program_filter = prog_name,
    title_text = paste("Percent of", prog_name, "Cases Reported ",
                       "<br> in", select_county, "County, 2020 - 2024"),
    county_alt_text = percent_alt_text
  )

by_year <- Gwinnett_data %>%
  group_by(onset_year, disease_program, disease) %>%
  summarise(total_cases = n(), .groups = "drop") %>%
  pivot_wider(names_from = onset_year, values_from = total_cases) %>%
  arrange(disease_program)
# view(by_year)

# 6. Save the Object
  save_load_plot(percent_object, select_county, prog_name, type = "percent")

# 7. ADD THIS: Store the plot in your list using the program name as the key
bar_plot_list[[prog_name]] <- percent_object
}

message("✅ All program plots processed and saved!")


# bar_plot_list[["Vaccine Preventable"]]
#
# bar_plot_list[[1]]
# bar_plot_list[[2]]
# bar_plot_list[[3]]
# # View(bar_plot_list$Vectorborne$data_view)
#
# # Note: Use the exact string from your program_list
#
#   bar_plot_list$Enteric$plot
#   bar_plot_list$Vectorborne$plot

# ------------------------------------------------------------------
# Generate & Save Timeseries Plots for All Programs
# ------------------------------------------------------------------
line_chart_list <- list()

# 1. outter loop: Loop through each program in your list ("Vaccine Preventable", "Vectorborne", "Enteric")
for (prog_name in program_list) {
  message(paste("--- Checking Program:", prog_name, "---"))

   # 2. Filter Data
  # Note: We start with the base Gwinnett data
  three_yr <- Gwinnett_data %>%
    filter(disease_program == prog_name) %>%
    filter(as.numeric(onset_year) %in% c(2024, 2023, 2022))

  message(paste("Rows found for", prog_name, ":", nrow(three_yr)))

  # Ensure we have data before proceeding to prevent errors
  if (nrow(three_yr) > 0) {

    # --- SECTION A: MAIN BAR CHART ---
    # Calculate the rates for the program (Data is already filtered to Gwinnett)
    # This runs once per program
    current_rates3 <- three_yr %>%
      county_rates(county_name = select_county)

    current_3rates <- get_top_diseases(current_rates3)

    message(paste("Top diseases found:", nrow(current_3rates)))



    # 5. Generate the Plot Object

    if (nrow(current_3rates) > 0) {
      # Generate Alt Text
      current_alt_text <- "Trend chart for top diseases"
        # county_alt_text_string(current_3rates, prog_name)

        time_object <- county_time_plot(
        data = current_3rates,
        title_text = paste("Case Rates for the Top 3 Reported",
                           prog_name, " Diseases", "<br>", " in ", select_county, " County, 2022 - 2024"),
        disease_program = prog_name,
        alt_text = current_alt_text,
        county = select_county
      )

    # 6. Save the Object to your LIST so you can view them later
    line_chart_list[[prog_name]] <- time_object
    message(paste("✅ SUCCESS: Saved plot for", prog_name))

    } else {
      warning("No 3-year top disease data for ", prog_name, "- skipping trend plot")
    }
  } else {
    warning(paste("⚠️ No data found for", prog_name, "- skipping plot generation."))
  }

  save_load_plot(time_object, select_county, prog_name, type = "trend")

  # 7. ADD THIS: Store the plot in your list using the program name as the key
  line_chart_list[[prog_name]] <- time_object
}


    # Combine all your lists into one "Master" object
Gmaster_plot_data <- list(
    line_charts = line_chart_list,
    bar_charts  = bar_plot_list
  )

  # Save it to your project folder
  saveRDS(Gmaster_plot_data, here("output", "plots", "gwinnett_linebar_plots.rds"))

  # Gmaster_plot_data

# time_object$plot
# line_chart_list$Enteric$plot
# line_chart_list[["Vaccine Preventable"]]$plot
# line_chart_list$Vectorborne$plot
# # View(line_chart_list$Vectorborne$data_view)

# Program Name	Correct Syntax
# Enteric	line_chart_list$Enteric$plot
#
# Vectorborne	line_chart_list$Vectorborne$plot
#
# Vaccine Preventable	line_chart_list[["Vaccine Preventable"]]$plot

