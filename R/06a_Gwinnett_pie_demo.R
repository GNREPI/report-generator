# File: Gwinnett Demographics Pie script.R----
# Purpose:Using combined data output, this creates the pie charts for the district by admin status,
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
source(here("R", "plot_accessibility.R"))
source(here::here("R/functions.R"))

# Master list to hold all plots for this county
all_dashboard_plots <- list()
county_name <- "Gwinnett"

# Initialize the county slot
all_dashboard_plots[[county_name]] <- list()


# Read the de-identified dataset to a location accessible by your dashboard\
county_pie <- qread(here("output", "combo_deidentified_clean.qs")) %>%
filter(county == "Gwinnett")


target_county = "Gwinnett"

# ------------------------------------------------------------------
# VACCINE-PREVENTABLE ILLNESSES (VPDs)
# ------------------------------------------------------------------
Gvpd_pie <- "Vaccine Preventable"

# Dataset
Gwinnett_pie_demo <- county_pie %>%
  filter(disease_program == Gvpd_pie)


# Gender
Gvpd_gen_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
  Gvpd_pie, " Illnesses by Gender in ", target_county),
  type            = "gender_pie" # Function handles grouping by 'gender'
)

Gvpd_gen_pie_list$plot

# Gvpd_gen_alt_text <- gender_alt_text(Gvpd_gen_pie_object, Gvpd_pie)

  # print(Gvpd_gen_pie)


# Race
Gvpd_rac_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo,
  county_name = target_county,
  title_text = paste0("Vaccine Preventable Illnesses by Race in ", target_county),
  county_alt_text = paste0("Pie chart showing the distribution of Vaccine Preventable illnesses by race in ", target_county, "."),
  type = "race_bar"  # This tells the function to look for the 'race' column and make a pie chart
)
Gvpd_rac_pie_list$plot



# Ethnicity
Gvpd_eth_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
title_text      = paste0("Distribution of ",
                         Gvpd_pie, " Illnesses by Gender in ", target_county),
type            = "ethnicity_pie") # Function handles grouping by 'gender'

Gvpd_eth_pie_list$plot



# ------------------------------------------------------------------
# ENTERIC ILLNESSES
# ------------------------------------------------------------------
Gent_pie <- "Enteric"

# Dataset
Gwinnett_pie_demo <- county_pie %>%
  filter(disease_program == Gent_pie)


# Gender
Gent_eth_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Gent_pie, " Illnesses by Gender in ", target_county),
  type            = "gender_pie" # Function handles grouping by 'gender'
)

Gent_eth_pie_list$plot

# Gvpd_gen_alt_text <- gender_alt_text(Gvpd_gen_pie_object, Gvpd_pie)

# print(Gvpd_gen_pie)


# Race
Gent_rac_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo,
  county_name = target_county,
  title_text = paste0("Vaccine Preventable Illnesses by Race in ", target_county),
  county_alt_text = paste0("Pie chart showing the distribution of Vaccine Preventable illnesses by race in ", target_county, "."),
  type = "race_bar"  # This tells the function to look for the 'race' column and make a pie chart
)
Gent_rac_pie_list$plot



# Ethnicity
Gent_gen_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Gent_pie, " Illnesses by Gender in ", target_county),
  type            = "ethnicity_pie") # Function handles grouping by 'gender'

Gent_gen_pie_list$plot



# ------------------------------------------------------------------
# VECTORBORNE ILLNESSES
# ------------------------------------------------------------------
Gvbd_pie <- "Vectorborne"

# Dataset
Gwinnett_pie_demo <- county_pie %>%
  filter(disease_program == Gvbd_pie)


# Gender
Gvbd_eth_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Gvbd_pie, " Illnesses by Gender in ", target_county),
  type            = "gender_pie" # Function handles grouping by 'gender'
)

Gvbd_eth_pie_list$plot

# Gvpd_gen_alt_text <- gender_alt_text(Gvpd_gen_pie_object, Gvpd_pie)

# print(Gvpd_gen_pie)


# Race
Gvbd_rac_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo,
  county_name = target_county,
  title_text = paste0("Vaccine Preventable Illnesses by Race in ", target_county),
  county_alt_text = paste0("Pie chart showing the distribution of Vaccine Preventable illnesses by race in ", target_county, "."),
  type = "race_bar"  # This tells the function to look for the 'race' column and make a pie chart
)
Gvbd_rac_pie_list$plot



# Ethnicity
Gvbd_gen_pie_list <- county_plot_loops(
  data = Gwinnett_pie_demo, # Pass the prepared data
  county_name = target_county,
  title_text      = paste0("Distribution of ",
                           Gvbd_pie, " Illnesses by Gender in ", target_county),
  type            = "ethnicity_pie") # Function handles grouping by 'gender'

Gvbd_gen_pie_list$plot





# These files will be loaded by the R Markdown dashboard later.
# saveRDS(Gvbd_cas_def_pie_list, here::here("output", "Gvbd_cas_def_pie.rds"))
saveRDS(Gvbd_eth_pie_list, here::here("output", "Gvbd_eth_pie.rds"))
saveRDS(Gvbd_rac_pie_list, here::here("output", "Gvbd_rac_pie.rds"))
saveRDS(Gvbd_gen_pie_list, here::here("output", "Gvbd_gen_pie.rds"))

# saveRDS(Gvpd_cas_def_pie_list, here::here("output", "Gvpd_cas_def_pie.rds"))
saveRDS(Gvpd_eth_pie_list, here::here("output", "Gvpd_eth_pie.rds"))
saveRDS(Gvpd_rac_pie_list, here::here("output", "Gvpd_rac_pie.rds"))
saveRDS(Gvpd_gen_pie_list, here::here("output", "Gvpd_gen_pie.rds"))

# saveRDS(Gent_cas_def_pie_list, here::here("output", "Gent_cas_def_pie.rds"))
saveRDS(Gent_eth_pie_list, here::here("output", "Gent_eth_pie.rds"))
saveRDS(Gent_rac_pie_list, here::here("output", "Gent_rac_pie.rds"))
saveRDS(Gent_gen_pie_list, here::here("output", "Gent_gen_pie.rds"))
