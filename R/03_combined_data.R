# File: combined_data.R----
# Purpose:
# Author:
# Date:
# These scripts are what you run before you open your R Markdown document.
# You typically run them once and save the output.
#-------------------------------------------------------------------------------

# update.packages(ask = FALSE, checkBuilt = TRUE)

# Packages ----------------------------------------------------------------
# Custom Functions----
# 1. Setup and Environment ----
# Loads required packages and custom functions.
# The 'packages.R' script should contain all library calls.
source("R/packages.R")

source(here("R/functions.R"))



# Get a list of all files in the "data" directory that end with .qs
file_list <- list.files(
  path = here("data", "clean"),
  pattern = "\\.qs$",
  full.names = TRUE # This is important! It gives you the full path.
)

# Print the list to verify
print(file_list)



all_data <- map(file_list, safely(qread))

# Extract successful results and remove failed ones (where result is NULL)
successful_data <- all_data %>%
  map("result") %>%   # Extracts the 'result' element (the data frame) from each inner list
  compact() %>%          # Removes any elements that are NULL (failed reads)
  map(~ mutate(.x, across(any_of("doo"), as.character)))
bind_successful_data <- bind_rows(successful_data)


# Read your lookup table from the CSV file
disease_program_lookup <- read_csv(here("data", "disease_program_lookup.csv")) %>%
  mutate(
    # Create a new, temporary column for case-insensitive joining
    join_disease = tolower(disease),
    # You can still use toTitleCase for display purposes in other columns
    disease = toTitleCase(disease),
    disease_program = toTitleCase(disease_program)
  )

# Assume you have another main data frame named 'main_data'
# Prepare the 'main_data' matching column for joining
main_data_prepared <- bind_successful_data %>%
  mutate(
    # Create a matching temporary column in the main data
    join_disease = tolower(disease) # Replace 'disease_column_name' with the actual column name
  )

# Perform the join on the new 'join_disease' column
joined_data <- left_join(
  main_data_prepared,
  disease_program_lookup,
  by = "join_disease") %>%
  # # Remove ALL PHI and intermediate age columns
  # select(
  #   pseudo_patient_id,
  #   everything())
  # Optional: remove the temporary join column after the merge
  select(-join_disease)


county_populations <- read_csv(here("data", "county_populations.csv")) %>%
  mutate(county = to_title_case(county))


# Perform both joins in a single step using the pipe
final_combo_data <- joined_data %>%
  left_join(county_populations, by = "county") %>%
  select(-disease.x)


final_data <- final_combo_data %>%
 mutate(county = to_title_case(county),
               disease.y = to_title_case(disease.y),
               race = as.character(to_title_case(race)),
               ethnicity = as.character(to_title_case(ethnicity)),
               gender = as.character(to_title_case(gender)),
               hospadmit = to_title_case(hospadmit),
               died = to_title_case(died)) %>%
mutate(
    race = recode(race,
        "Black" = "Black or African American",
        "Other" = "Other Race",
        "Not Available" = "Unknown"),
    race = na_if(race, ""),
    race = replace_na(race, "Unknown")) %>%

  mutate(
    ethnicity = recode(ethnicity,
                       "Hispanic" = "Hispanic or Latino",
                       "Not Available" = "Unknown",
                       "Non Hispanic" = "Not Hispanic or Latino"),
    ethnicity = na_if(ethnicity, ""),
    ethnicity = replace_na(ethnicity, "Unknown"))%>%

  mutate(
    adminstatus = recode(adminstatus,
                         adminstatus = na_if(adminstatus, ""),
                         adminstatus = replace_na(adminstatus, "Unknown"))) %>%

mutate(
  disease.y = case_when(
    disease.y %in% c("Hepatitis b", "Hepatitis b Acute", "Hepatitis b Chronic",
                   "Hepatitis b Probable Chronic") ~ "Hepatitis B",
    disease.y %in% c("Hepatitis c", "Hepatitis c Acute", "Hepatitis c Chronic",
                   "Hepatitis c Probable Acute", "Hepatitis c Probable Chronic") ~ "Hepatitis C",
    disease.y == "Hepatitis a Acute" ~ "Hepatitis A",
    TRUE ~ disease.y)) %>%
  mutate(disease.y = str_to_title(disease.y)) %>%


  mutate(
    disease.y = case_when(
      disease.y == "West Nile Wnv Infection" ~ "West Nile Virus",
      TRUE ~ disease.y)) %>%
  mutate(disease.y = str_to_title(disease.y)) %>%

  rename(disease = disease.y)


# 4. Save Cleaned Data
#----------------------------------------------
# Define the path to your output directory
output_dir <- here::here("output")

# Check if the directory exists and create it if it doesn't
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Now you can safely save your file
qsave(final_data, here::here("output", "combo_deidentified_clean.qs"))


# Save the de-identified dataset to a location accessible by your dashboard\
write.csv(final_data, here::here("output", "combo_deidentified_clean.csv"), row.names = FALSE)



