# File: Newton Demographics Pie script.R----
# Purpose: Using combined data output, this creates the pie charts for the district by admin status,
# race, gender, ethnicity.
# Author: Allene Stephens
# Date: 11.25.2025
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
source(here::here("R/functions.R"))


# Read the de-identified dataset to a location accessible by your dashboard\
county_pie <- read.csv(here("output", "combo_deidentified_clean.csv")) %>%
  filter(county == "Newton")

target_county = "Newton"

# ------------------------------------------------------------------
# VACCINE-PREVENTABLE ILLNESSES (VPDs)
# ------------------------------------------------------------------
Nvpd_pie <- "Vaccine Preventable"

# Dataset
Gwinnett_pie_demo <- county_pie %>%
  filter(disease_program == Nvpd_pie)


# Gender
Nvpd_gen_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Nvpd_pie, " Illnesses by Gender in ", target_county),
  type            = "gender_pie" # Function handles grouping by 'gender'
)

Nvpd_gen_pie_list$plot

# Nvpd_gen_alt_text <- gender_alt_text(Nvpd_gen_pie_object, Nvpd_pie)



# Race
Nvpd_rac_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo,
  county_name = target_county,
  title_text = paste0("Vaccine Preventable Illnesses by Race in ", target_county),
  county_alt_text = paste0("Pie chart showing the distribution of Vaccine Preventable illnesses by race in ", target_county, "."),
  type = "race_bar"  # This tells the function to look for the 'race' column and make a pie chart
)
Nvpd_rac_pie_list$plot



# Ethnicity
Nvpd_eth_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Nvpd_pie, " Illnesses by Gender in ", target_county),
  type            = "ethnicity_pie") # Function handles grouping by 'gender'

Nvpd_eth_pie_list$plot



# ------------------------------------------------------------------
# ENTERIC ILLNESSES
# ------------------------------------------------------------------
Nent_pie <- "Enteric"

# Dataset
Gwinnett_pie_demo <- county_pie %>%
  filter(disease_program == Nent_pie)


# Gender
Nent_eth_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Nent_pie, " Illnesses by Gender in ", target_county),
  type            = "gender_pie" # Function handles grouping by 'gender'
)

Nent_eth_pie_list$plot

# Nvpd_gen_alt_text <- gender_alt_text(Nvpd_gen_pie_object, Nvpd_pie)



# Race
Nent_rac_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo,
  county_name = target_county,
  title_text = paste0("Vaccine Preventable Illnesses by Race in ", target_county),
  county_alt_text = paste0("Pie chart showing the distribution of Vaccine Preventable illnesses by race in ", target_county, "."),
  type = "race_bar"  # This tells the function to look for the 'race' column and make a pie chart
)
Nent_rac_pie_list$plot



# Ethnicity
Nent_gen_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Nent_pie, " Illnesses by Gender in ", target_county),
  type            = "ethnicity_pie") # Function handles grouping by 'gender'

Nent_gen_pie_list$plot



# ------------------------------------------------------------------
# VECTORBORNE ILLNESSES
# ------------------------------------------------------------------
Nvbd_pie <- "Vectorborne"

# Dataset
Gwinnett_pie_demo <- county_pie %>%
  filter(disease_program == Nvbd_pie)


# Gender
Nvbd_eth_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Nvbd_pie, " Illnesses by Gender in ", target_county),
  type            = "gender_pie" # Function handles grouping by 'gender'
)

Nvbd_eth_pie_list$plot




# Race
Nvbd_rac_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo,
  county_name = target_county,
  title_text = paste0("Vaccine Preventable Illnesses by Race in ", target_county),
  county_alt_text = paste0("Pie chart showing the distribution of Vaccine Preventable illnesses by race in ", target_county, "."),
  type = "race_bar"  # This tells the function to look for the 'race' column and make a pie chart
)
Nvbd_rac_pie_list$plot



# Ethnicity
Nvbd_gen_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Nvbd_pie, " Illnesses by Gender in ", target_county),
  type            = "ethnicity_pie") # Function handles grouping by 'gender'

Nvbd_gen_pie_list$plot



# These files will be loaded by the R Markdown dashboard later.
saveRDS(Nvbd_eth_pie_list, here::here("output", "Nvbd_eth_pie.rds"))
saveRDS(Nvbd_rac_pie_list, here::here("output", "Nvbd_rac_pie.rds"))
saveRDS(Nvbd_gen_pie_list, here::here("output", "Nvbd_gen_pie.rds"))

saveRDS(Nvpd_eth_pie_list, here::here("output", "Nvpd_eth_pie.rds"))
saveRDS(Nvpd_rac_pie_list, here::here("output", "Nvpd_rac_pie.rds"))
saveRDS(Nvpd_gen_pie_list, here::here("output", "Nvpd_gen_pie.rds"))

saveRDS(Nent_eth_pie_list, here::here("output", "Nent_eth_pie.rds"))
saveRDS(Nent_rac_pie_list, here::here("output", "Nent_rac_pie.rds"))
saveRDS(Nent_gen_pie_list, here::here("output", "Nent_gen_pie.rds"))
