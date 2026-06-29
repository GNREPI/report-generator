# -----
# Hepatitis Time Series Plots
#
#
# Author: Allene Stephens
# Date: 12/03/2025
#----

# 1. Setup and Environment ----
# Loads required packages and custom functions.
# The 'packages.R' script should contain all library calls.
# Load the 'here' package so it can be used later
library(here)
source(here("R","packages.R"))
source("R/functions.R")
source("R/brand_theme.R")



# --- Branding Validation Check ---
current_theme <- getOption("plotly_template")

if (!is.null(current_theme)) {
  message("✅ Branding Active: Using ", current_theme$layout$font$family)
} else {
  warning("⚠️ WARNING: No brand template detected. Plots will use default Plotly styles!")
}

accessible_colors <- surv_colors

# Define your constants
# Define the county name and the disease program names
county_name <- c("Gwinnett", "Newton", "Rockdale")
program_name <- c("Vectorborne", "Enteric", "Vaccine Preventable")

# Create a data frame of all 9 combinations
county_program_combinations <- expand_grid(
  county = county_name,
  disease_program = program_name
)
# Load full dataset and define colors (accessible globally to the functions)
combo <- qread(here::here("output", "combo_deidentified_clean.qs"))

exists("accessible_colors")
stopifnot(exists("accessible_colors"))

# ============================================================
#   district
# ============================================================
district_hep_time <- combo %>%
  # The regex '^Vaccine Preventable' matches exactly that string at the beginning (^).
  # We use coalesce in case a value doesn't match the pattern.
  filter(str_detect(disease, "^Hepatitis") |
           str_detect(disease, "^Perinatal")) %>%
  district_time()

# DEFINE alt_text_string HERE (Step 1)
max_hep_rate <- max(district_hep_time$cumulative_rate)
max_hep_disease <- district_hep_time$disease[which.max(district_hep_time$cumulative_rate)]
num_hep_diseases <- nrow(district_hep_time)

dis_hep_alt_text <- district_alt_text_string(district_hep_time)

# Call Plotting Function (Step 2)
district_hep_object <- district_time_plot(
  data = district_hep_time,
  alt_text = dis_hep_alt_text
)


  # district_hep_object$plot



# ============================================================
#   Gwinnett
# ============================================================

county_name <- "Gwinnett"

Gwinnett_hep <- combo %>%
filter(county == county_name) %>%
# The regex '^Vaccine Preventable' matches exactly that string at the beginning (^).
  # We use coalesce in case a value doesn't match the pattern.
  filter(str_detect(disease, "^Hepatitis") |
           str_detect(disease, "^Perinatal")) %>%
  county_time()

# 2. Logic for Alt Text (Screen Reader Description)
if (nrow(Gwinnett_hep) == 0) {
  Gwinnett_hep_alt_text <- paste0("No case data reported for hepatitis cases in this county.")
} else {

# Calculate stats for the string
max_hep_rate <- max(Gwinnett_hep$case_rate_per_100k, na.rm = TRUE)
max_hep_disease <- Gwinnett_hep$disease[which.max(Gwinnett_hep$case_rate_per_100k)]
num_hep_diseases <- length(unique(Gwinnett_hep$disease))
disease_list <- paste(sort(unique(Gwinnett_hep$disease)), collapse = ", ")

# Assemble the string
Gwinnett_hep_alt_text <- paste0(
  "Time series plot showing hepatitis case rates in ", county_name, " County from 2019 to 2024. ",
  "The plot tracks ", num_hep_diseases, " different conditions: ", disease_list, ". ",
  "The highest recorded rate was ", round(max_hep_rate, 2), " cases per 100,000 population for ", max_hep_disease, "."
)
}

# Call Plotting Function
Gwinnett_hep_object <- county_plot_loops(
  data = Gwinnett_hep,
  county_name = "Gwinnett",
  title_text = paste("Hepatitis Case Rates in Gwinnett"),
county_alt_text = Gwinnett_hep_alt_text,
type = "trend"
)

 # Gwinnett_hep_object$plot



# ============================================================
#   Newton
# ============================================================

county_name <- "Newton"

Newton_hep <- combo %>%
  filter(county == county_name) %>%
  # The regex '^Vaccine Preventable' matches exactly that string at the beginning (^).
  # We use coalesce in case a value doesn't match the pattern.
  filter(str_detect(disease, "^Hepatitis") |
           str_detect(disease, "^Perinatal")) %>%
  county_time()

# 2. Logic for Alt Text (Screen Reader Description)
if (nrow(Newton_hep) == 0) {
  Newton_hep_alt_text <- paste0("No case data reported for hepatitis cases in this county.")
} else {

  # Calculate stats for the string
  max_hep_rate <- max(Newton_hep$case_rate_per_100k, na.rm = TRUE)
  max_hep_disease <- Newton_hep$disease[which.max(Newton_hep$case_rate_per_100k)]
  num_hep_diseases <- length(unique(Newton_hep$disease))
  disease_list <- paste(sort(unique(Newton_hep$disease)), collapse = ", ")

  # Assemble the string
  Newton_hep_alt_text <- paste0(
    "Time series plot showing hepatitis case rates in ", county_name, " County from 2019 to 2024. ",
    "The plot tracks ", num_hep_diseases, " different conditions: ", disease_list, ". ",
    "The highest recorded rate was ", round(max_hep_rate, 2), " cases per 100,000 population for ", max_hep_disease, "."
  )
}

# Call Plotting Function (Step 2)
Newton_hep_object <- county_plot_loops(
  data = Newton_hep,
  county_name = "Newton",
  title_text = paste("Hepatitis Case Rates in Newton"),
  county_alt_text = Newton_hep_alt_text,
  type = "trend"
)

# Newton_hep_object$plot



# ============================================================
#   Rockdale
# ============================================================

county_name <- "Rockdale"

Rockdale_hep <- combo %>%
  filter(county == county_name) %>%
  # The regex '^Vaccine Preventable' matches exactly that string at the beginning (^).
  # We use coalesce in case a value doesn't match the pattern.
  filter(str_detect(disease, "^Hepatitis") |
           str_detect(disease, "^Perinatal")) %>%
  county_time()


# 2. Logic for Alt Text (Screen Reader Description)
if (nrow(Rockdale_hep) == 0) {
  Rockdale_hep_alt_text <- paste0("No case data reported for hepatitis cases in this county.")
} else {

  # Calculate stats for the string
  max_hep_rate <- max(Rockdale_hep$case_rate_per_100k, na.rm = TRUE)
  max_hep_disease <- Rockdale_hep$disease[which.max(Rockdale_hep$case_rate_per_100k)]
  num_hep_diseases <- length(unique(Rockdale_hep$disease))
  disease_list <- paste(sort(unique(Rockdale_hep$disease)), collapse = ", ")

  # Assemble the string
  Rockdale_hep_alt_text <- paste0(
    "Time series plot showing hepatitis case rates in ", county_name, " County from 2019 to 2024. ",
    "The plot tracks ", num_hep_diseases, " different conditions: ", disease_list, ". ",
    "The highest recorded rate was ", round(max_hep_rate, 2), " cases per 100,000 population for ", max_hep_disease, "."
  )
}

# Call Plotting Function (Step 2)
Rockdale_hep_object <- county_plot_loops(
  data = Rockdale_hep,
  county_name = "Rockdale",
  title_text = paste("Hepatitis Case Rates in Rockdale"),
  county_alt_text = Rockdale_hep_alt_text,
  type = "trend"
)

# Rockdale_hep_object$plot


# Save individual plot files
saveRDS(district_hep_object, here::here("output", "plots", "district_hep_time.rds"))
saveRDS(Gwinnett_hep_object, here::here("output", "plots", "Gwinnett_hep_time.rds"))
saveRDS(Newton_hep_object,   here::here("output", "plots", "Newton_hep_time.rds"))
saveRDS(Rockdale_hep_object, here::here("output", "plots", "Rockdale_hep_time.rds"))

# Build master list with consistent county-name keys
all_hep_plots <- list(
  district = district_hep_object,
  Gwinnett = Gwinnett_hep_object,
  Newton   = Newton_hep_object,
  Rockdale = Rockdale_hep_object
)

# Save master file
hep_master_file_path <- here("output", "plots", "all_hep_plots.rds")
if (!dir.exists(dirname(hep_master_file_path))) dir.create(dirname(hep_master_file_path), recursive = TRUE)
saveRDS(all_hep_plots, hep_master_file_path)
message("✅ All plots saved to Hep master RDS: ", hep_master_file_path)






