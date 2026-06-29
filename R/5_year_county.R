# File: 5 Year County Plots.R----
# Purpose:
# Author: Allene Stephens
# Date: 01/26/2026
#-------------------------------------------------------------------------------

# Packages ----------------------------------------------------------------
# Custom Functions----
# 1. Setup and Environment ----
# Loads required packages and custom functions.
# The 'packages.R' script should contain all library calls.
source("R/packages.R")
source("R/brand_theme.R")
source(here("R/functions.R"))



# Import Data
#--------------------------
# Import the raw data and standardize its column names for easier manipulation.
# The `clean_names()` function from `janitor` converts names to a consistent, snake_case format.

# This script should be run in a secure, controlled environment, NOT your public dashboard repo
# Load your raw Excel data
# Checking for essential columns after cleaning -----
VBDraw_PHI_data <- import_and_clean(here("data", "raw", "VBDs_2020_2024_raw.csv"))
entericraw_PHI_data <- import_and_clean(here("data", "raw", "enterics_2020_2024_raw.csv"))
VPDraw_PHI_data <- import_and_clean(here("data", "raw", "VPDs_2020_2024_raw.csv"))
# enteric5_PHI_data <- import_and_clean(here("data", "raw", "2020_2024_enterics.csv"))

# 4. Save Cleaned Data
#----------------------------------------------
qsave(VBDraw_PHI_data, here("data", "clean", "clean_phi", "2020_2024_VBDs_clean.qs"))
qsave(entericraw_PHI_data, here("data", "clean","clean_phi", "2020_2024_enterics_clean.qs"))
qsave(VPDraw_PHI_data, here("data", "clean","clean_phi", "2020_2024_VPDs_clean.qs"))
# qsave(enteric5_PHI_data, here("data", "clean", "clean_phi", "2020_2024_enterics_clean.qs"))
# # Your R Markdown file is now much cleaner and more focused. Now import the
# # Save the de-identified dataset to a location accessible by your dashboard\
# write.csv(entericraw_PHI_data,here::here("data", "clean", "enteric_deidentified_cleaned.csv"), row.names = FALSE)
#
# write.csv(VBDraw_PHI_data,here::here("data", "clean", "enteric_deidentified_cleaned.csv"), row.names = FALSE)
#
#


# <- Wrangling ----
enteric5_raw_PHI_data <- wrangle(here("data", "clean", "clean_phi", "2020_2024_enterics_clean.qs"))
VBD5_raw_PHI_data <- wrangle(here("data", "clean", "clean_phi", "2020_2024_VBDs_clean.qs"))
VPDs5_raw_PHI_data <- wrangle(here("data", "clean", "clean_phi", "2020_2024_VPDs_clean.qs"))


# Secure Mapping ----------------------------------------------------------

# --- Step 1: Create the secure mapping table from UNIQUE patient IDs ---
# Generate a unique, non-identifiable `pseudo_patient_id` for each original `patientid`.
# This mapping is critical for privacy and must be stored in a highly secure, non-public location.

# Use distinct(patientid) to ensure each unique patient gets ONE pseudo_patient_id
enteric5_patientid_maps <- patient_id_mapping(enteric5_raw_PHI_data)
vpd5_patientid_maps <- patient_id_mapping(VPDs5_raw_PHI_data)
vbd5_patientid_maps <- patient_id_mapping(VBD5_raw_PHI_data)


# Saving Secure Mapping ---------------------------------------------------

# It's crucial to separate paths for sensitive raw data and the de-identified public data.
# The `secure_base_path` defines where sensitive files will be stored or accessed.
esecure_mapping_path <- here("Security", "2020_2024_enteric_patient_id_mapping.csv")
psecure_mapping_path <- here("Security", "2020_2024_vpd_patient_id_mapping.csv")
bsecure_mapping_path <- here("Security", "2020_2024_vbd_patient_id_mapping.csv")


# Store this mapping to a highly secure location, NOT accessible to your dashboard
# Save the first mapping file
write_csv(enteric5_patientid_maps, esecure_mapping_path)
write_csv(vpd5_patientid_maps, psecure_mapping_path)
write_csv(vbd5_patientid_maps, bsecure_mapping_path)



# Date Formatting ----
# Define path for problematic rows (e.g., unparsable dates), now with a timestamp.
# Changed to generic "invalid_date_data" as it can now include issues from any of the four date columns.
einvalid_date_data_output_path <- here("data", "review", "enteric5_invalid_dates.csv")
pinvalid_date_data_output_path <- here("data", "review", "vpd5_invalid_dates.csv")
binvalid_date_data_output_path <- here("data", "review", "vbd5_invalid_dates.csv")


eraw_parsed_dates <- fix_dates(enteric5_raw_PHI_data)
praw_parsed_dates <- fix_dates(VPDs5_raw_PHI_data)
braw_parsed_dates <- fix_dates(VBD5_raw_PHI_data)


# Unpack the list into two separate variables
eraw_invalid_dates <- eraw_parsed_dates$invalid
eraw_valid_dates <- eraw_parsed_dates$valid

praw_invalid_dates <- praw_parsed_dates$invalid
praw_valid_dates <- praw_parsed_dates$valid

braw_invalid_dates <- braw_parsed_dates$invalid
braw_valid_dates <- braw_parsed_dates$valid




# Export the problematic rows for manual review
if (nrow(eraw_invalid_dates) > 0){
  message("No rows with invalid date formats found.")
} else {
  write_csv(eraw_invalid_dates, invalid_date_data_output_path)
  message(paste("Invalid date data saved to:", einvalid_date_data_output_path))
  message(
    "IMPORTANT: Please open the data / review / 'enteric_invalid_dates.csv'
    file and examine date columns to understand the exact formatting issues.
    You may need to add more specific formats to 'date_formats' or clean the raw data manually."
  )
}



# Export the problematic rows for manual review
if (nrow(praw_invalid_dates) > 0){
  message("No rows with invalid date formats found.")
} else {
  write_csv(praw_invalid_dates, pinvalid_date_data_output_path)
  message(paste("Invalid date data saved to:", pinvalid_date_data_output_path))
  message(
    "IMPORTANT: Please open the data / review / 'enteric_invalid_dates.csv'
    file and examine date columns to understand the exact formatting issues.
    You may need to add more specific formats to 'date_formats' or clean the raw data manually."
  )
}



# Export the problematic rows for manual review
if (nrow(braw_invalid_dates) > 0){
  message("No rows with invalid date formats found.")
} else {
  readr::write_csv(braw_invalid_dates, binvalid_date_data_output_path)
  message(paste("Invalid date data saved to:", binvalid_date_data_output_path))
  message(
    "IMPORTANT: Please open the data / review / 'enteric_invalid_dates.csv'
    file and examine date columns to understand the exact formatting issues.
    You may need to add more specific formats to 'date_formats' or clean the raw data manually."
  )
}




#START OF YOUR DE-IDENTIFICATION PIPELINE ---
enteric5_deidentified_cleaned <- joined_deidentified(eraw_valid_dates, enteric5_patientid_maps)
vpd5_deidentified_cleaned <- joined_deidentified(praw_valid_dates, vpd5_patientid_maps)
vbd5_deidentified_cleaned <- joined_deidentified(braw_valid_dates, vbd5_patientid_maps)


# Save the de-identified dataset to a location accessible by your dashboard
edeidentified_data_output_path <- here("data", "clean", "enteric5_deidentified_cleaned.csv")
pdeidentified_data_output_path <- here("data", "clean", "vpd5_deidentified_cleaned.csv")
bdeidentified_data_output_path <- here("data", "clean", "vbd5_deidentified_cleaned.csv")


readr::write_csv(enteric5_deidentified_cleaned, edeidentified_data_output_path)
message(paste("De-identified data saved to:", edeidentified_data_output_path))

readr::write_csv(vpd5_deidentified_cleaned, pdeidentified_data_output_path)
message(paste("De-identified data saved to:", pdeidentified_data_output_path))

readr::write_csv(vbd5_deidentified_cleaned, bdeidentified_data_output_path)
message(paste("De-identified data saved to:", bdeidentified_data_output_path))


# 4. Save Cleaned Data
#----------------------------------------------
qsave(enteric5_deidentified_cleaned, here("data", "clean", "enteric5_deidentified_cleaned.qs"))
qsave(vpd5_deidentified_cleaned, here("data", "clean", "vpd5_deidentified_cleaned.qs"))
qsave(vbd5_deidentified_cleaned, here("data", "clean", "vbd5_deidentified_cleaned.qs"))



# Get a list of all files in the "data" directory that end with .qs
file_list <- list.files(
  path = here("data", "clean"),
  pattern = "\\.qs$",
  full.names = TRUE # This is important! It gives you the full path.
)

# Print the list to verify
print(file_list)



all_data <- map(file_list, safely(qs::qread))

# Extract successful results and remove failed ones (where result is NULL)
successful_data <- all_data %>%
  map("result") %>%   # Extracts the 'result' element (the data frame) from each inner list
  compact()          # Removes any elements that are NULL (failed reads)
# Or, using dplyr::bind_rows
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
  dplyr::mutate(county = to_title_case(county),
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
qsave(final_data, here::here("output", "combo5_deidentified_clean.qs"))


# Save the de-identified dataset to a location accessible by your dashboard\
write.csv(final_data, here::here("output", "combo5_deidentified_clean.csv"), row.names = FALSE)


# Creat the Plot ----


program_list <- c("Vaccine Preventable", "Vectorborne", "Enteric")

# Define the demographics you want to chart
demographic_list <- c("gender_pie", "race_pie", "ethnicity_pie", "admin_status")


# # Define your constants
# # Define the county name and the disease program names
county_name <- c("Gwinnett", "Newton", "Rockdale")
#
# program_name <- c("Vectorborne", "Enteric", "Vaccine Preventable")

# --- Branding Validation Check ---
current_theme <- getOption("plotly_template")

if (!is.null(current_theme)) {
  message("✅ Branding Active: Using ", current_theme$layout$font$family)
} else {
  warning("⚠️ WARNING: No brand template detected. Plots will use default Plotly styles!")
}



# Create a data frame of all 9 combinations
county5_program_combinations <- expand_grid(
  county = county_name,
  disease_program = program_list
)


# Data Import ----
final_data <- qread(here("output", "combo5_deidentified_clean.qs")) %>%
  filter(county == select_county)


current_alt_text <- paste("TBD")

# enteric_data <- Gwinnett_data %>%
#   filter(disease_program == "Enteric")

# five_year_bar_data <- final_data %>%
#   filter(
#     county == current_county,
#     disease_program == current_program,
#     onset_year %in% 2020:2024
#   ) %>%
#   group_by(disease) %>%
#   summarise(
#     total_cases = n(),
#     .groups = "drop"
#   )
#

# 5. Generate the Plot Object
Gpercent_chart <- percent_chart(
  data = enteric_data,
  # disease_program = disease_program,
  title_text = paste("Percent Reported by in ", select_county, "County"),
  county_alt_text = current_alt_text
)


# Plot all 3 counties ----

county5_plot_list <- county5_program_combinations %>%
  mutate(data_input = list(county_data)) %>%
  mutate(output = pmap(
    # Argument 1: The list of items to iterate over
    list(
      data_input = data_input,                 # list-column (recycled)
      county_name = county,                    # character vector (one per row)
      program_name = disease_program),        # character vector (one per row)

    # Argument 2: The function (The .f that was "missing")
    # explicit function avoids .x/.y/.z scoping problems
    function(data_input, county_name, program_name) {
      process_data_and_plot(
        data_input = data_input,
        county_name = county_name,
        program_name = program_name)}))



# 1. Setup Lists
county_list <- c("Gwinnett", "Newton", "Rockdale")
program_list <- c("Vaccine Preventable", "Vectorborne", "Enteric")

# 2. Create Grid of Combinations (3 counties * 3 programs = 9 rows)
county5_program_combinations <- expand_grid(
  county = county_list,
  disease_program = program_list
)

source(here("R/functions.R"))


# 3. Load MASTER Data (Do not filter yet)
# We load the full dataset once. We will filter it inside the loop.
all_data5 <- qread(here::here("output", "combo5_deidentified_clean.qs"))

# 4. Iterate using pmap
# This adds a column 'output' which contains the list (plot + text) for each row
plot_results_df <- plot_results_df %>%
  mutate(top3 = map(output, ~ {
    if (is.null(.x$bar_data)) return(NULL)
    get_top_diseases(.x$bar_data)
  }))

county5_program_combinations %>%
  mutate(output = pmap(
    # Arguments to map over
    list(current_county = county, current_program = disease_program),

    # The anonymous function to run for every row
    function(current_county, current_program) {

      five_year_bar_data <- final_data %>%
        filter(
          county == current_county,
          disease_program == current_program,
          onset_year %in% 2020:2024
        ) %>%
        group_by(disease) %>%
        summarise(
          total_cases = n(),
          .groups = "drop"
        )


      # B. Handle Empty Data Safely
      if(nrow(five_year_bar_data) == 0) {
        message(paste("⚠️ No data found for:", current_county, current_program))
        return(list(plot = NULL, sr_text = "No data available"))
      }

      # C. Dynamic Text for accessibility
      dynamic_title <- paste0("Percentage of ", current_program, " Diseases in ", current_county, " County")
      dynamic_alt_text <- paste("Chart showing", current_program, "percentages for", current_county)

      # D. Run Function (Using the robust version)
      bar_plot <-   percent_chart(
        data = five_year_bar_data,
        county_alt_text = dynamic_alt_text,
              title_text = paste(
                "Percent of",
                current_program,
                "Diseases in",
                current_county,
                "County"
              ),
  #         dynamic_title,
  # disease_program_filter = current_program # Added a dedicated filter arg
      )
    }
  ))


# 5. Verification (Check Gwinnett + Enteric)
# Extract the first row (assuming Gwinnett/Vaccine Preventable is first)
# OR filter for specific one:
check_row <- plot_results_df %>%
  filter(county == "Gwinnett", disease_program == "Enteric")

if(nrow(check_row) > 0) {
  print("✅ Plot generated for Gwinnett Enteric:")
  print(check_row$output[[1]]$plot)
} else {
  print("❌ Gwinnett Enteric row not found.")
}


# To extract Gwinnett + Enteric:
target_row <- plot_results_df %>%
  filter(county == "Gwinnett", disease_program == "Enteric")


# ============================================================================
# 3. Extract Plot Objects and Save Files ---------------------------------------
# This pwalk loop extracts the plot object and saves it as an .rds file.
plot_results_df %>%
  # We don't need to extract names from 'output' because we already have
  # 'county' and 'disease_program' columns in this dataframe!
  # DATA FRAME COLUMNS ARE: 'output', 'county', 'disease_program'

  # Now, the pwalk function uses plot_object_list which contains plot and sr_text
  pwalk(function(output, county, disease_program, ...) {

    # Extract the actual plot/text list
    plot_bundle <- output

    # Skip if there's no plot to save
    if (is.null(plot_bundle$plot)) {
      message("⏩ Skipping ", county, " ", disease_program, " (No Data)")
      return()
    }

    # 1. Clean the Program Name
    safe_program_name <- gsub(" ", "_", tolower(disease_program))
    safe_program_name <- gsub("-", "_", safe_program_name) # Ensure hyphens are also covered

    # 🌟 2. CLEAN AND STANDARDIZE THE COUNTY NAME 🌟
    safe_county_name <- gsub(" ", "_", tolower(county))
    safe_county_name <- gsub("-", "_", safe_county_name) # Ensure hyphens are also covered

    file_name <- paste0(safe_county_name, "_", safe_program_name, "_percent_plot.rds")
    file_path <- here::here("output", file_name)

    saveRDS(plot_bundle, file_path)
    message("Saved: ", file_name)
  })

## ============================================================================
# SCRIPT: Generate & Save 3-Year Time Series Plots (Iterative)
# ============================================================================

# ============================================================================
  # SCRIPT: Generate 3-Year Time Series for Top 3 Diseases (from 5yr Bar Plot)
  # ============================================================================

message("🚀 Starting Top 3 Time Series generation...")

# 1. Iterate through the grid of 3 counties x 3 programs
ts_results_df <- county5_program_combinations %>%
  mutate(ts_output = pmap(
    list(current_county = county, current_program = disease_program),

    function(current_county, current_program) {

      message(paste("🔄 Filtering Top 3 for:", current_county, "|", current_program))

      # --- STEP A: IDENTIFY TOP 3 FROM 5-YEAR DATA ---
      # Note: Using 'disease' to match your dataset's column naming convention
      # based on your previous percent_chart function
      top_3_names <- final_data %>%
        filter(county == current_county,
               disease_program == current_program,
               onset_year %in% 2020:2024) %>%
        group_by(disease) %>%
        summarise(total_cases = n(), .groups = "drop") %>%
        slice_max(total_cases, n = 3, with_ties = FALSE) %>%
        pull(disease)

      # Handle cases with no data
      if(length(top_3_names) == 0) {
        message(paste("⚠️ No diseases found for:", current_county, current_program))
        return(NULL)
      }

      # --- STEP B: FILTER 3-YEAR DATA FOR ONLY THOSE DISEASES ---
      subset_data_3yr <- final_data %>%
        filter(county == current_county,
               disease %in% top_3_names,
               onset_year %in% c(2022, 2023, 2024))

      # Calculate Rates for the selected diseases
      # Ensure your county_rates function is also looking for 'disease'
      current_rates_ts <- subset_data_3yr %>%
        county_rates(county_name = current_county)

      # --- STEP C: GENERATE PLOT ---
      current_alt_text <- county_alt_text_string(current_rates_ts, current_program)

      time_object <- county_time_plot(
        data = current_rates_ts,
        county_name = current_county,
        disease_program = current_program,
        title_text = paste("Top 3", current_program, "Trends in", current_county),
        county_alt_text = current_alt_text
      )

      # --- STEP D: SAVE ---
      save_plot_object(
        plot_obj = time_object,
        county = current_county,
        program = current_program,
        type = "trend"
      )

      return(time_object)
    }
  ))

message("✅ All Top 3 time series plots processed and saved!")









# # 1. Setup the iteration grid (Assuming this exists from your 5-year workflow)
# # This grid usually contains: county, disease_program
# # If not already defined:
# # county5_program_combinations <- expand.grid(
# #   county = c("Gwinnett", "Newton", "Rockdale"),
# #   disease_program = c("Vaccine Preventable", "Vectorborne", "Enteric"),
# #   stringsAsFactors = FALSE
# # )
#
# message("🚀 Starting Time Series generation for all County/Program combinations...")
# top3 <- get_top_diseases(plot_results_df)
#
# top_3 <- plot_results_df$output[[i]]$bar_data %>%
#   slice_max(total_cases, n = 3) %>%
#   pull(disease)
#
# # 2. Iterate using pmap
# ts_results_df <- plot_results_df %>%
#   mutate(ts_output = pmap(
#     list(current_county = county, current_program = disease_program),
#
#     function(current_county, current_program) {
#
#       message(paste("🔄 Processing:", current_county, "|", current_program))
#
#       # --- STEP A: IDENTIFY TOP 3 FROM 5-YEAR DATA ---
#       # Note: Using 'disease_name' to match your dataset's column naming convention
#       top_3_names <- final_data %>%
#         filter(county == current_county,
#                disease_program == current_program,
#                onset_year %in% 2020:2024) %>%
#         group_by(disease) %>%
#         summarise(total_cases = n(), .groups = "drop") %>%
#         slice_max(total_cases, n = 3, with_ties = FALSE) %>%
#         pull(disease)
#
#       # B. Handle Empty Data Safely
#       if(nrow(subset_data) == 0) {
#         message(paste("⚠️ No data found for:", current_county, current_program))
#         return(NULL)
#       }
#
#       # --- STEP B: FILTER 3-YEAR DATA FOR ONLY THOSE DISEASES ---
#       subset_data_3yr <- final_data %>%
#         filter(county == current_county,
#                disease %in% top_3_names,
#                onset_year %in% c(2022, 2023, 2024))
#
#
#       # C. Calculate Rates (using your standard county_rates function)
#       current_rates_ts <- subset_data_3yr %>%
#         county_rates(county_name = current_county)
#
#       # D. Generate Alt Text
#       # Uses the helper function to summarize the 3-year trend
#       current_alt_text <- county_alt_text_string(current_rates_ts, current_program)
#
#       # E. Filter for Top Diseases (optional, but keeps the line chart readable)
#       # current_rates_top <- get_top_diseases(current_rates_ts)
#
#       # F. Generate the Plot Object
#       # Note: Ensure county_time_plot is loaded and handles line chart logic
#       time_object <- county_time_plot(
#         data = current_rates_ts,
#         county_name = current_county,
#         disease_program = current_program,
#         title_text = paste(current_program, "Trend in", current_county, "County (2022-2024)"),
#         alt_text = current_alt_text
#       )
#
#       # G. Save the Plot Bundle using your standardized I/O function
#       # We use type = "trend" to match the switch logic in R/io_functions.R
#       # which maps to the suffix "_rate_time.rds"
#       save_plot_object(
#         plot_obj = time_object,
#         county = current_county,
#         program = current_program,
#         type = "trend"
#       )
#
#       # Return the object to the dataframe for immediate inspection if needed
#       return(time_object)
#     }
#   ))
#
# message("✅ All 3-year time series plots processed and saved to /output/.")

# --- Verification ---
# To view a specific plot from the resulting dataframe:
# ts_results_df$ts_output[[1]]$plot


#
# #============================================================
# # --- Method 1: The "Scroll" Method ---
# # This loop prints every plot to your RStudio 'Viewer' pane one by one.
# # Use the back/forward arrows in the Viewer tab to navigate them.
#
# for (i in 1:nrow(plot_results_df)) {
#   # Extract the plot object from the list-column
#   p <- plot_results_df$output[[i]]$plot
#
#   # Check if it's a valid plot before printing
#   if (!is.null(p)) {
#     message("Viewing: ", plot_results_df$county[i], " - ", plot_results_df$disease_program[i])
#     print(p)
#     # Optional: Sys.sleep(1) # Adds a pause if you want to see them slowly
#   }
# }
#
#
# # --- Method 2: The "Grid" Method (Subplots) ---
# # If you want to see all 9 charts in one big interactive window.
#
# # 1. Extract just the plot objects into a simple list
# all_plots <- map(plot_results_df$output, ~ .x$plot)
#
# # 2. Use plotly's subplot function
# # This creates a grid (3 rows, 3 columns)
# subplot(all_plots, nrows = 3, margin = 0.05, shareX = FALSE, titleX = TRUE) %>%
#   layout(title = "Overview: All County Program Percentages")
#
#
# # --- Method 3: The "Filtering" Method ---
# # Best if you just want to see a specific one without knowing the row number.
#
# view_plot <- function(target_county, target_program) {
#   row <- plot_results_df %>%
#     filter(county == target_county, disease_program == target_program)
#
#   if(nrow(row) > 0) {
#     return(row$output[[1]]$plot)
#   } else {
#     message("Plot not found!")
#   }
# }
#
# # Usage:
# view_plot("Newton", "Enteric")
#



county_time_plot <- function(data, title_text, county_name, disease_program, alt_text){

  brand <- get_brand()

  brand_font <- getOption("brand_font")
  if (is.null(brand_font)) brand_font <- "Arial"
  # # Pull brand objects
  # colors <- getOption("brand_colors")

  # 1. Initialize the result as NULL so it always exists
  timeplot_county <- NULL

  # 2. Safety Check for empty data


  if (is.data.frame(data) && nrow(data) > 0){

    timeplot_county <- plot_ly(
      data = data,
      x = ~onset_year,
      y = ~case_rate_per_100k,
      color = ~disease,
      type = "scatter",
      mode = 'lines+markers', # This is correct for lines with points
      line = list(color = unname(brand$colors[1]), width = 3),
      marker = list(color = unname(brand$colors[1]), size = 8),
      # --- ADD THIS FOR CUSTOM HOVER TEXT ---
      hovertext = ~paste(
        "<b>Disease Type: </b>", disease, "<br>",
        "<b>Case Rate: </b>", round(case_rate_per_100k, 2), "<br>"),
      hoverinfo = "text"
    ) %>%
      layout(
        template = brand$template,
        title = list(
          text = title_text,
          # ("<b>Hepatitis Case Rates", '<br>', "in ", select_county,
          # " County, 2019 - 2024</b>"),
          font = list(family = brand$fonts$chart_title),
          x = 0.5, y = 0.9, # Position (0 to 1, x=0.5 is center, y=0.95 is near top)
          xanchor = "center", yanchor = "top", # Align the top of the title with y=0.98
          pad = list(b = 30)), # Padding *below* the main chart title text itself
        xaxis = list(
          title = list(
            text = "<b>Year</b>", # Bold the X-axis category name (title)
            font = list(family = brand_font, color = "black")),
          tickmode = "linear",
          # standoff = 20 # Add 20px space between X-axis labels and X-axis title
          dtick = 1,
          showgrid = TRUE,  # Show grid lines
          gridcolor = "lightgrey",
          automargin = TRUE
        ),
        yaxis = list(
          title = list(
            text = "<b>Case Rates per <br> 100k population</b>",
            font = list(family = brand_font, color = "black"),
            tickmode = "linear",
            standoff = 5), # Add 20px space between y-axis labels and y-axis title
          zeroline = FALSE, # Hide the zero line
          showgrid = TRUE,
          automargin = TRUE),

        showlegend = TRUE,

        margin = list(
          t = 100,
          b = 115,
          r = 50,
          l = 80
        ),

        # --- ADD FOOTNOTES AS ANNOTATIONS HERE ---
        annotations = list(
          # Footnote 1: Data Source
          list(
            text = "<b>Data Source:</b> SENDSS Georgia Department of Public Health, 2019-2024",
            xref = "paper", # Position relative to the entire plot area (0 to 1)
            yref = "paper", # Position relative to the entire plot area (0 to 1)
            x = 0,         # X position (0 = left edge of plot)
            y = -0.25,     # Y position (negative values place it below the plot area)
            # Adjust 'y' based on your bottom margin and font size
            showarrow = FALSE, # Do not show an arrow
            xanchor = "left",  # Anchor the text to its left side
            yanchor = "bottom",  # Let plotly determine vertical anchor (usually bottom)
            font = list(family = brand_font, color = "black")
          )
        )
      ) %>%
      config(responsive = TRUE)

  } else {
    #     If the data frame is empty, print a message instead of a plot
    message(" ", stringr::str_to_title("No data available for district hepatitis cases."))
    plot_result <- plotly_empty() %>%
      layout(title = "No Data Available")
  }
  # 3. Return the standard bundle structure (Plot + Alt Text)
  return(list(plot = timeplot_county, sr_text = alt_text))
}








```{r 04_Newton_VPD_plot, echo=FALSE, warning=FALSE}


# Fetch it
nvpd_data <- load_plot_object("Newton", "Vaccine Preventable", "bar")

# Show it
if(!is.null(nvpd_data)){# We use tagList to wrap multiple HTML elements together
  # We use print() because results='asis' requires explicit output
  tags$div(
    class = "dashboard-card",
    nvpd_data$plot,
    tags$span(
      class = "sr-only",
      nvpd_data$sr_text
    ))
} else {
  # Fallback message if file is missing
  cat("<p>Data not available for this section.</p>")
}

nvpd_data$plot
```

