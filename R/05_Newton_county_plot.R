# File: Newton plots script.R----
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
counties <- "Newton"
programs <- c("Vaccine Preventable", "Vectorborne", "Enteric")

# Define the demographics you want to chart
demographics <- c("gender", "race", "eth", "case")



# Data Import ----
Newton_data <- qread(here("output", "combo_deidentified_clean.qs")) %>%
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

# Initialize master list
all_dashboard_plots <- list()

for (county in counties) {

  message("Processing county: ", county)
  all_dashboard_plots[[county]] <- list()

  # Load county data
  county_data <- qread(here("output", "combo_deidentified_clean.qs")) %>%
    filter(county == .env$county)

  for (prog in programs) {
    message("  Processing program: ", prog)

    all_dashboard_plots[[county]][[prog]] <- list()

    # --- 1. Bar Chart ---
    prog_data <- county_data %>% filter(disease_program == prog)

    if(nrow(prog_data) > 0){
      alt_text <- county_alt_text_string(prog_data, prog)

      bar_obj <- percent_chart(
        data = prog_data,
        disease_program_filter = prog,
        title_text = paste("Percent of", prog, "Cases in", county, "County"),
        county_alt_text = alt_text
      )

      all_dashboard_plots[[county]][[prog]][["bar"]] <- bar_obj
    } else {
      warning("No bar data for ", county, " / ", prog)
    }

    # --- 2. Trend Chart (Top 3 diseases, last 3 years) ---
    three_yr <- prog_data %>% filter(onset_year %in% c(2022,2023,2024))
    if(nrow(three_yr) > 0){
      current_rates <- county_rates(three_yr, county_name = county)
      top3 <- get_top_diseases(current_rates)
      alt_text <- county_alt_text_string(top3, prog)

      trend_obj <- county_time_plot(
        data = top3,
        title_text = paste("Case Rates for Top 3", prog, "Diseases in", county),
        alt_text = alt_text
      )

      all_dashboard_plots[[county]][[prog]][["trend"]] <- trend_obj
    } else {
      warning("No trend data for ", county, " / ", prog)
    }

    # --- 3. Demographic Pie Charts ---
    for (demo in demographics) {
      pie_obj <- switch(demo,
                        "gender" = percent_chart(prog_data, prog, demo, "Gender"),
                        "race" = percent_chart(prog_data, prog, demo, "Race"),
                        "eth" = percent_chart(prog_data, prog, demo, "Ethnicity"),
                        "case" = percent_chart(prog_data, prog, demo, "Case Definition"))
      all_dashboard_plots[[county]][[prog]][[demo]] <- pie_obj
    }

  } # End program loop
} # End county loop

# Save master file
master_file_path <- here("output", "plots", "all_dashboard_plots.rds")
if(!dir.exists(dirname(master_file_path))) dir.create(dirname(master_file_path), recursive = TRUE)
saveRDS(all_dashboard_plots, master_file_path)
message("✅ All plots saved to master RDS: ", master_file_path)

