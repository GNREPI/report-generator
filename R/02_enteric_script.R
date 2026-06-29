# File: enteric_script.R----
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

source(here("R/functions.R"))



# <- Wrangling ----
enteric_raw_PHI_data <- wrangle(here("data", "clean", "clean_phi", "2024_enteric_clean.qs"))

# enteric5_raw_PHI_data <- wrangle(here("data", "clean", "clean_phi", "2020_2024_enterics_clean.qs"))

# Secure Mapping ----------------------------------------------------------

# --- Step 1: Create the secure mapping table from UNIQUE patient IDs ---
# Generate a unique, non-identifiable `pseudo_patient_id` for each original `patientid`.
# This mapping is critical for privacy and must be stored in a highly secure, non-public location.

# Use distinct(patientid) to ensure each unique patient gets ONE pseudo_patient_id
enteric_patientid_maps <- patient_id_mapping(enteric_raw_PHI_data)

# enteric5_patientid_maps <- patient_id_mapping(enteric5_raw_PHI_data)

# Saving Secure Mapping ---------------------------------------------------

# It's crucial to separate paths for sensitive raw data and the de-identified public data.
# The `secure_base_path` defines where sensitive files will be stored or accessed.
secure_mapping_path <- here("Security", "enteric_patient_id_mapping.csv")

# secure_mapping_path5 <- here("Security", "enteric5_patient_id_mapping.csv")

# Store this mapping to a highly secure location, NOT accessible to your dashboard
# Save the first mapping file
write_csv(enteric_patientid_maps, secure_mapping_path)

# Save the second mapping file
# write_csv(enteric5_patientid_maps, secure_mapping_path5)

# message(paste("Secure mapping saved to:", secure_mapping_path, " and ", secure_mapping_path5))



# Date Formatting ----
# Define path for problematic rows (e.g., unparsable dates), now with a timestamp.
# Changed to generic "invalid_date_data" as it can now include issues from any of the four date columns.
invalid_date_data_output_path <- here("data", "review", "enteric_invalid_dates.csv")
# invalid_date_data_output_path5 <- here::here("data", "review", "enteric5_invalid_dates.csv")


raw_parsed_dates <- fix_dates(enteric_raw_PHI_data)
# raw_parsed_dates5 <- fix_dates(enteric_raw_PHI_data)

# Unpack the list into two separate variables
raw_invalid_dates <- raw_parsed_dates$invalid
raw_valid_dates <- raw_parsed_dates$valid

# raw_invalid_dates5 <- raw_parsed_dates5$invalid
# raw_valid_dates5 <- raw_parsed_dates5$valid


# Export the problematic rows for manual review
if (nrow(raw_invalid_dates) > 0){
  message("No rows with invalid date formats found.")
} else {
  write_csv(raw_invalid_dates, invalid_date_data_output_path)
  message(paste("Invalid date data saved to:", invalid_date_data_output_path))
  message(
    "IMPORTANT: Please open the data / review / 'enteric_invalid_dates.csv'
    file and examine date columns to understand the exact formatting issues.
    You may need to add more specific formats to 'date_formats' or clean the raw data manually."
  )
}


# # Export the problematic rows for manual review
# if (nrow(raw_invalid_dates5) > 0){
#   message("No rows with invalid date formats found.")
# } else {
#   readr::write_csv(raw_invalid_dates5, invalid_date_data_output_path5)
#   message(paste("Invalid date data saved to:", invalid_date_data_output_path5))
#   message(
#     "IMPORTANT: Please open the data / review / 'enteric_invalid_dates.csv'
#     file and examine date columns to understand the exact formatting issues.
#     You may need to add more specific formats to 'date_formats' or clean the raw data manually."
#   )
# }
#
#
# message(paste(
#   "Number of rows with invalid date formats exported for investigation:",
#   nrow(raw_invalid_dates)
# ))
#
# message(paste(
#   "Number of rows with valid dates proceeding to de-identification:",
#   nrow(raw_valid_dates)
# ))




#START OF YOUR DE-IDENTIFICATION PIPELINE ---
enteric_deidentified_cleaned <- joined_deidentified(raw_valid_dates, enteric_patientid_maps)

# enteric_deidentified_cleaned5 <- joined_deidentified(raw_valid_dates5, enteric5_patientid_maps)


# Save the de-identified dataset to a location accessible by your dashboard
deidentified_data_output_path <- here("data", "clean", "enteric_deidentified_cleaned.csv")

# deidentified_data_output_path5 <- here::here("data", "clean", "enteric_deidentified_cleaned5.csv")



write_csv(enteric_deidentified_cleaned, deidentified_data_output_path)
message(paste("De-identified data saved to:", deidentified_data_output_path))

# readr::write_csv(enteric_deidentified_cleaned5, deidentified_data_output_path5)
# message(paste("De-identified data saved to:", deidentified_data_output_path5))


# 4. Save Cleaned Data
#----------------------------------------------
qsave(enteric_deidentified_cleaned, here("data", "clean", "enteric_deidentified_cleaned.qs"))

# qsave(enteric_deidentified_cleaned5, here::here("data", "clean", "enteric_deidentified_cleaned5.qs"))


# unique(enteric_deidentified_cleaned$disease)
#
# mutate(
#   disease = recode(disease,
#                 "Black" = "Black or African American",
#                 "Other" = "Other Race",
#                 "Not Available" = "Unknown"),
#   race = na_if(race, ""),
#   race = replace_na(race, "Unknown")) %>%







