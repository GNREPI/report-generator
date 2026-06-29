# File: Rockdale Demographics Pie script.R----
# Purpose:
# Author:
# Date: 12.05.2025
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
filter(county == "Rockdale")


target_county = "Rockdale"

# ------------------------------------------------------------------
# VACCINE-PREVENTABLE ILLNESSES (VPDs)
# ------------------------------------------------------------------
Rvpd_pie <- "Vaccine Preventable"

# Dataset
Gwinnett_pie_demo <- county_pie %>%
  filter(disease_program == Rvpd_pie)


# Gender
Rvpd_gen_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Rvpd_pie, " Illnesses by Gender in ", target_county),
  type            = "gender_pie" # Function handles grouping by 'gender'
)

Rvpd_gen_pie_list$plot

# Rvpd_gen_alt_text <- gender_alt_text(Rvpd_gen_pie_object, Rvpd_pie)

# print(Rvpd_gen_pie)


# Race
Rvpd_rac_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo,
  county_name = target_county,
  title_text = paste0("Vaccine Preventable Illnesses by Race in ", target_county),
  county_alt_text = paste0("Pie chart showing the distribution of Vaccine Preventable illnesses by race in ", target_county, "."),
  type = "race_bar"  # This tells the function to look for the 'race' column and make a pie chart
)
Rvpd_rac_pie_list$plot



# Ethnicity
Rvpd_eth_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Rvpd_pie, " Illnesses by Gender in ", target_county),
  type            = "ethnicity_pie") # Function handles grouping by 'gender'

Rvpd_eth_pie_list$plot



# ------------------------------------------------------------------
# ENTERIC ILLNESSES
# ------------------------------------------------------------------
Rent_pie <- "Enteric"

# Dataset
Gwinnett_pie_demo <- county_pie %>%
  filter(disease_program == Rent_pie)


# Gender
Rent_eth_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Rent_pie, " Illnesses by Gender in ", target_county),
  type            = "gender_pie" # Function handles grouping by 'gender'
)

Rent_eth_pie_list$plot

# Rvpd_gen_alt_text <- gender_alt_text(Rvpd_gen_pie_object, Rvpd_pie)

# print(Rvpd_gen_pie)


# Race
Rent_rac_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo,
  county_name = target_county,
  title_text = paste0("Vaccine Preventable Illnesses by Race in ", target_county),
  county_alt_text = paste0("Pie chart showing the distribution of Vaccine Preventable illnesses by race in ", target_county, "."),
  type = "race_bar"  # This tells the function to look for the 'race' column and make a pie chart
)
Rent_rac_pie_list$plot



# Ethnicity
Rent_gen_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Rent_pie, " Illnesses by Gender in ", target_county),
  type            = "ethnicity_pie") # Function handles grouping by 'gender'

Rent_gen_pie_list$plot



# ------------------------------------------------------------------
# VECTORBORNE ILLNESSES
# ------------------------------------------------------------------
Rvbd_pie <- "Vectorborne"

# Dataset
Gwinnett_pie_demo <- county_pie %>%
  filter(disease_program == Rvbd_pie)


# Gender
Rvbd_eth_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Rvbd_pie, " Illnesses by Gender in ", target_county),
  type            = "gender_pie" # Function handles grouping by 'gender'
)

Rvbd_eth_pie_list$plot




# Race
Rvbd_rac_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo,
  county_name = target_county,
  title_text = paste0("Vaccine Preventable Illnesses by Race in ", target_county),
  county_alt_text = paste0("Pie chart showing the distribution of Vaccine Preventable illnesses by race in ", target_county, "."),
  type = "race_bar"  # This tells the function to look for the 'race' column and make a pie chart
)
Rvbd_rac_pie_list$plot



# Ethnicity
Rvbd_gen_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Rvbd_pie, " Illnesses by Gender in ", target_county),
  type            = "ethnicity_pie") # Function handles grouping by 'gender'

Rvbd_gen_pie_list$plot


# These files will be loaded by the R Markdown dashboard later.
# saveRDS(Rvbd_cas_def_pie_list, here::here("output", "Rvbd_cas_def_pie.rds"))
saveRDS(Rvbd_eth_pie_list, here::here("output", "Rvbd_eth_pie.rds"))
saveRDS(Rvbd_rac_pie_list, here::here("output", "Rvbd_rac_pie.rds"))
saveRDS(Rvbd_gen_pie_list, here::here("output", "Rvbd_gen_pie.rds"))

# saveRDS(Rvpd_cas_def_pie_list, here::here("output", "Rvpd_cas_def_pie.rds"))
saveRDS(Rvpd_eth_pie_list, here::here("output", "Rvpd_eth_pie.rds"))
saveRDS(Rvpd_rac_pie_list, here::here("output", "Rvpd_rac_pie.rds"))
saveRDS(Rvpd_gen_pie_list, here::here("output", "Rvpd_gen_pie.rds"))

# saveRDS(Rent_cas_def_pie_list, here::here("output", "Rent_cas_def_pie.rds"))
saveRDS(Rent_eth_pie_list, here::here("output", "Rent_eth_pie.rds"))
saveRDS(Rent_rac_pie_list, here::here("output", "Rent_rac_pie.rds"))
saveRDS(Rent_gen_pie_list, here::here("output", "Rent_gen_pie.rds"))
