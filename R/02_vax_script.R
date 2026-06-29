# File: vax_script.R----
# Purpose:
# Author:
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

source(here::here("R/functions.R"))



# Data Wrangling ----
vax_raw_PHI_data <- wrangle(here("data", "clean", "clean_phi","VPDs_2019_2024_clean.qs"))



# Secure Mapping ----------------------------------------------------------

# --- Step 1: Create the secure mapping table from UNIQUE patient IDs ---
# Generate a unique, non-identifiable pseudo_patient_id for each original patientid.
# This mapping is critical for privacy and must be stored in a highly secure, non-public location.

# Use distinct(patientid) to ensure each unique patient gets ONE pseudo_patient_id
vax_patientid_maps <- patient_id_mapping(vax_raw_PHI_data)


# Saving Secure Mapping ---------------------------------------------------

# It's crucial to separate paths for sensitive raw data and the de-identified public data.
# The secure_base_path defines where sensitive files will be stored or accessed.
secure_mapping_path <- here("Security", "vax_patient_id_mapping.csv")

# Store this mapping to a highly secure location, NOT accessible to your dashboard
write_csv(vax_patientid_maps, secure_mapping_path)
message(paste("Secure mapping saved to:", secure_mapping_path))



# Date Formatting ----
# Define path for problematic rows (e.g., unparsable dates), now with a timestamp.
# Changed to generic "invalid_date_data" as it can now include issues from any of the four date columns.
invalid_date_data_output_path <- here::here("data", "review", "vax_invalid_dates.csv")


raw_parsed_dates <- fix_dates(vax_raw_PHI_data)

# Unpack the list into two separate variables
raw_invalid_dates <- raw_parsed_dates$invalid
raw_valid_dates <- raw_parsed_dates$valid


# Export the problematic rows for manual review
if (nrow(raw_invalid_dates) > 0){
  message("No rows with invalid date formats found.")
} else {
  readr::write_csv(raw_invalid_dates, invalid_date_data_output_path)
  message(paste("Invalid date data saved to:", invalid_date_data_output_path))
  message(
    "IMPORTANT: Please open the data / review / 'vax_invalid_dates.csv'
    file and examine date columns to understand the exact formatting issues.
    You may need to add more specific formats to 'date_formats' or clean the raw data manually."
  )
}

message(paste(
  "Number of rows with invalid date formats exported for investigation:",
  nrow(raw_invalid_dates)
))

message(paste(
  "Number of rows with valid dates proceeding to de-identification:",
  nrow(raw_valid_dates)
))




#START OF YOUR DE-IDENTIFICATION PIPELINE ---
vax_deidentified_cleaned <- joined_deidentified(raw_valid_dates, vax_patientid_maps)



# Save the de-identified dataset to a location accessible by your dashboard
deidentified_data_output_path <- here::here("data", "clean", "vax_deidentified_cleaned.csv")




readr::write_csv(vax_deidentified_cleaned, deidentified_data_output_path)
message(paste("De-identified data saved to:", deidentified_data_output_path))


# 4. Save Cleaned Data
#----------------------------------------------
qsave(vax_deidentified_cleaned, here::here("data", "clean", "vax_deidentified_cleaned.qs"))



