# File: 01_import_data.R----
# Purpose: Import data, clean column names, check for required columns to prepare for cleaning.
# Author: Allene Stephens
# Date: 09/09/2025
# These scripts are what you run before you open your R Markdown document.
# You typically run them once and save the output.
#-------------------------------------------------------------------------------

# Packages ----------------------------------------------------------------
# Custom Functions----
# 1. Setup and Environment ----
# Loads required packages and custom functions.
# The 'packages.R' script should contain all library calls.
source("R/packages.R")

source(here::here("R/functions.R"))



# Import Data
#--------------------------
# Use here() to ensure the path is always correct.

# Import the raw data and standardize its column names for easier manipulation.
# The `clean_names()` function from `janitor` converts names to a consistent, snake_case format.

# This script should be run in a secure, controlled environment, NOT your public dashboard repo
# Load your raw Excel data
# Checking for essential columns after cleaning -----
VBDraw_PHI_data <- import_and_clean(here("data", "raw", "2024_VBDs_raw.csv"))
entericraw_PHI_data <- import_and_clean(here("data", "raw", "2024_enteric_raw.csv"))
VPDraw_PHI_data <- import_and_clean(here("data", "raw", "VPDs_2019_2024_raw.csv"))
# enteric5_PHI_data <- import_and_clean(here("data", "raw", "2020_2024_enterics.csv"))

# 4. Save Cleaned Data
#----------------------------------------------
qsave(VBDraw_PHI_data, here::here("data", "clean", "clean_phi", "2024_VBDs_clean.qs"))
qsave(entericraw_PHI_data, here::here("data", "clean","clean_phi", "2024_enteric_clean.qs"))
qsave(VPDraw_PHI_data, here::here("data", "clean","clean_phi", "VPDs_2019_2024_clean.qs"))
# qsave(enteric5_PHI_data, here("data", "clean", "clean_phi", "2020_2024_enterics_clean.qs"))
# # Your R Markdown file is now much cleaner and more focused. Now import the
# # Save the de-identified dataset to a location accessible by your dashboard\
# write.csv(entericraw_PHI_data,here::here("data", "clean", "enteric_deidentified_cleaned.csv"), row.names = FALSE)
#
# write.csv(VBDraw_PHI_data,here::here("data", "clean", "enteric_deidentified_cleaned.csv"), row.names = FALSE)
#
#

