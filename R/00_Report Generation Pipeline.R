# -----
# Report Generation Pipeline
# This script orchestrates the entire data analysis, from data ingestion
# and cleaning to plotting and report generation.
#
# Author: Allene Stephens
# Date: 09/29/2025
#----
library(here)


# 1. Setup and Environment ----
# Loads required packages and custom functions.
# The 'packages.R' script should contain all library calls.
# Load the 'here' package so it can be used later
source(here("R", "packages.R"))
source(here("R","brand_theme.R"))
source("R/functions.R")
source(here("R", "plot_accessibility.R"))

# 2. Data Ingestion ----
# Loads raw data from a file into R's memory.
print("Step 1: Loading raw data...")
source("R/01_data_import.R")


# 3. Data Wrangling and Transformation ----
# Cleans and transforms the data for data analysis
# These scripts create cleaned dataframes and save them as .rds and .csv files.
print("Step 2: Cleaning and transforming data...")
source("R/02_enteric_script.R")
source("R/02_vax_script.R")

source("R/02_VBD_script.R")


# 4. Exploratory Data Analysis (EDA) and Visualization ----
# Generates plots and summary tables for EDA.
print("Step 3: Creating exploratory plots...")
source("R/03_combined_data.R")
source("R/04_district_plot.R")
source("R/04_district_pie_demo.R")
source("R/05a_generate_county_plots.R")
source("R/05b_county_plots_readin.R")
source("R/05_Gwinnett_county_plot.R")   # 5-yr bar and 3-yr timeseries
source("R/05_Newton_county_plot.R")     # 5-yr bar and 3-yr timeseries
source("R/05_Rockdale_county_plot.R")   # 5-yr bar and 3-yr timeseries
source("R/06a_Gwinnett_pie_demo.R")
source("R/06b_Newton_pie_demo.R")
source("R/06c_Rockdale_pie_demo.R")
source("R/hep_timeseries.R")
source("R", "07_age_pyramids.R")


# 5. Render a flexdashboard file ----
# Renders the final R Markdown report that combines text, code, and outputs
# from the analysis.
print("Step 4: Generating final report...")
# Assumes report.Rmd is in the reports/folder


 rmarkdown::render(here("testing123.Rmd"),
                   output_file = "final_dashboard.html")


#-----#
print("Pipeline complete!")
#----#

