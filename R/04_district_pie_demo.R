# File: 04_district_pie_demo.R----
# Purpose: Using combined data output, this creates the pie charts for the district by admin status,
# race, gender, ethnicity.
# Author: Allene Stephens
# Date:
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

# Data Import ----
district_demo_data <- qread(here("output", "combo_deidentified_clean.qs"))

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


# ------------------------------------------------------------------
# VACCINE-PREVENTABLE ILLNESSES (VPDs)
# ------------------------------------------------------------------
vpd_pie <- "Vaccine Preventable"

# Dataset
district_pie_demo <- district_demo_data %>%
  filter(disease_program == vpd_pie)

# Gvpds_alt_text <- county_alt_text_string(Gwinnett_VPD, VPD_program)


# Gender
vpd_district_gender <- generate_pie_explosion(
  data = district_pie_demo, # Pass the prepared data
  group_col_name = "gender", # <--- Pass the column name you want to plot!
  disease_program = vpd_pie,
  county_name = paste0("the District"))
# Save as a list with plot + sr_text

vpd_district_gender_pie <- list(
  plot = vpd_district_gender,
  sr_text = vpd_district_gender$sr_text
)

vpd_district_gender$plot


# Race
vpd_district_race <- generate_pie_explosion(
  data = district_pie_demo, # Pass the prepared data
  group_col_name = "race", # <--- Pass the column name you want to plot!
  disease_program = vpd_pie,
  county_name = paste0("the District"))

 print(vpd_district_race$plot)


# Ethnicity
vpd_district_ethnicity <- generate_pie_explosion(
  data = district_pie_demo, # Pass the prepared data
  group_col_name = "ethnicity", # <--- Pass the column name you want to plot!
  disease_program = vpd_pie,
  county_name = paste0("the District"))

 print(vpd_district_ethnicity$plot)



# ------------------------------------------------------------------
# ENTERIC ILLNESSES
# ------------------------------------------------------------------
ent_pie <- "Enteric"

# Dataset
district_pie_demo <- district_demo_data %>%
  filter(disease_program == ent_pie)


# Gender
ent_district_gender <- generate_pie_explosion(
  data = district_pie_demo, # Pass the prepared data
  group_col_name = "gender", # <--- Pass the column name you want to plot!
  disease_program = ent_pie,
  county_name = paste0("the District"))


# print(ent_district_gender)


# Race
ent_district_race <- generate_pie_explosion(
  data = district_pie_demo, # Pass the prepared data
  group_col_name = "race", # <--- Pass the column name you want to plot!
  disease_program = ent_pie,
  county_name = paste0(" the District "))

# print(ent_district_race)



# Ethnicity
ent_district_ethnicity <- generate_pie_explosion(
  data = district_pie_demo, # Pass the prepared data
  group_col_name = "ethnicity", # <--- Pass the column name you want to plot!
  disease_program = ent_pie,
  county_name = paste0("the District"))

# print(ent_district_ethnicity)




# ------------------------------------------------------------------
# VECTORBORNE ILLNESSES
# ------------------------------------------------------------------
vbd_pie <- "Vectorborne"

# Dataset
district_pie_demo <- district_demo_data %>%
  filter(disease_program == vbd_pie)



# Gender
vbd_district_gender <- generate_pie_explosion(
  data = district_pie_demo, # Pass the prepared data
  group_col_name = "gender", # <--- Pass the column name you want to plot!
  disease_program = vbd_pie,
  county_name = paste0("the District"))

# print(vbd_district_gender)


# Race
vbd_district_race <- generate_pie_explosion(
  data = district_pie_demo, # Pass the prepared data
  group_col_name = "race", # <--- Pass the column name you want to plot!
  disease_program = vbd_pie,
  county_name = paste0("the District"))
# print(vbd_district_race)



# Ethnicity
vbd_district_ethnicity <- generate_pie_explosion(
  data = district_pie_demo, # Pass the prepared data
  group_col_name = "ethnicity", # <--- Pass the column name you want to plot!
  disease_program = vbd_pie,
  county_name = paste0("the District"))

# print(vbd_district_ethnicity)





# These files will be loaded by the R Markdown dashboard later.
saveRDS(vpd_district_gender, here::here("output", "vpd_district_gender_pie.rds"))
saveRDS(vpd_district_race, here::here("output", "vpd_district_race_pie.rds"))
saveRDS(vpd_district_ethnicity, here::here("output", "vpd_district_ethnicity_pie.rds"))
# saveRDS(vpd_district_case_def, here::here("output", "vpd_district_case_def_pie.rds"))

saveRDS(ent_district_gender, here::here("output", "ent_district_gender_pie.rds"))
saveRDS(ent_district_race, here::here("output", "ent_district_race_pie.rds"))
saveRDS(ent_district_ethnicity, here::here("output", "ent_district_ethnicity_pie.rds"))
# saveRDS(ent_district_case_def, here::here("output", "ent_district_case_def_pie.rds"))

saveRDS(vbd_district_gender, here::here("output", "vbd_district_gender_pie.rds"))
saveRDS(vbd_district_race, here::here("output", "vbd_district_race_pie.rds"))
saveRDS(vbd_district_ethnicity, here::here("output", "vbd_district_ethnicity_pie.rds"))
# saveRDS(vbd_district_case_def, here::here("output", "vbd_district_case_def_pie.rds"))

