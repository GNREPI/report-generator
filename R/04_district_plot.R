# File: 04_district_plot.R----
# Purpose: Using combined data output, this creates the bar chart for the district by program.
# Author: Allene Stephens
# Date: 11.20.2025
# These scripts are what you run before you open your R Markdown document.
# You typically run them once and save the output.
#-------------------------------------------------------------------------------



# Packages ----------------------------------------------------------------
# Custom Functions----
# 1. Setup and Environment ----
# Loads required packages and custom functions.
# The 'packages.R' script should contain all library calls.
source("R/packages.R")
source(here("R/functions.R"))
source("R/brand_theme.R")


# Data Import ----
district_data <- qread(here("output", "combo_deidentified_clean.qs"))

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


# ------------------------------------------------------------------
# VACCINE-PREVENTABLE ILLNESSES (VPDs)
# ------------------------------------------------------------------
# 4. Run your plotting function to generate a plot
vpds_district <- district_data %>%
  filter(disease_program == "Vaccine Preventable",
         onset_year == 2024) %>%
  district_pop()

# current_vpds_district <- vpds_district %>%
#   filter(onset_year == 2024)

# DEFINE alt_text_string HERE (Step 1)
max_vpd_rate <- max(vpds_district$cumulative_rate)
max_vpd_disease <- vpds_district$disease[which.max(vpds_district$cumulative_rate)]
num_vpd_diseases <- nrow(vpds_district)

vpds_alt_text <- district_alt_text_string(vpds_district)


# Call Plotting Function (Step 2)
vpds_district_object <- plot_district_rates(
  data = vpds_district,
  title_text = paste("Vaccine Preventable Case Rates in GNR, 2024"),
  district_alt_text = vpds_alt_text # <--- PASSING THE DEFINED STRING
)


# Save as a list with plot + sr_text
vpds_district_plot <- list(
  plot = vpds_district_object$plot,
  sr_text = vpds_alt_text
)




# ------------------------------------------------------------------
# ENTERIC ILLNESSES
# ------------------------------------------------------------------
enterics_district <- district_data %>%
  filter(disease_program == "Enteric",
         onset_year == 2024) %>%
  district_pop()


max_enteric_rate <- max(enterics_district$cumulative_rate)
max_enteric_disease <- enterics_district$disease[which.max(enterics_district$cumulative_rate)]
num_enteric_diseases <- nrow(enterics_district)


enterics_alt_text <- district_alt_text_string(enterics_district)


# Call Plotting Function (Step 2)
enterics_district_object <- plot_district_rates(
  data = enterics_district,
  title_text = paste("Enteric Case Rates in GNR, 2024"),
  district_alt_text = enterics_alt_text # <--- PASSING THE DEFINED STRING
)

# Save as a list with plot + sr_text
enterics_district_plot <- list(
  plot = enterics_district_object$plot,
  sr_text = enterics_alt_text
)
#


# ------------------------------------------------------------------
# VECTORBORNE ILLNESSES
# ------------------------------------------------------------------
vbds_district <- district_data %>%
  filter(disease_program == "Vectorborne",
         onset_year == 2024) %>%
  district_pop()


max_vbd_rate <- max(vbds_district$cumulative_rate)
max_vbd_disease <- vbds_district$disease[which.max(vbds_district$cumulative_rate)]
num_vbd_diseases <- nrow(vbds_district)


vbds_alt_text <- district_alt_text_string(vbds_district)


# Call Plotting Function (Step 2)
vbds_district_object <- plot_district_rates(
  data = vbds_district,
  title_text = paste("Vectorborne Case Rates in GNR, 2024"),
  district_alt_text = vbds_alt_text # <--- PASSING THE DEFINED STRING

)

# Save as a list with plot + sr_text
vbds_district_plot <- list(
  plot = vbds_district_object$plot,
  sr_text = vbds_alt_text
)




# NEW: Save the generated plot lists to disk
# These files will be loaded by the R Markdown dashboard later.
saveRDS(vpds_district_object, here::here("output", "vpds_district_plot.rds"))
saveRDS(vbds_district_object, here::here("output", "vbds_district_plot.rds"))
saveRDS(enterics_district_object, here::here("output", "enterics_district_plot.rds"))


# # Peek at the VPD plot
# test_plot <- readRDS(here::here("output", "vpds_district_plot.rds"))
#
# # Check the alt text
# print(test_plot$sr_text)
#
# # Check the plot
# test_plot$plot













