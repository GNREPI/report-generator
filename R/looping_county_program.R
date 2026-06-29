# File: plots script.R----
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
source("R/brand_theme.R")
source(here("R/functions.R"))



# R/generate_plots.R (or your main script where data is processed)


# 2. Define Constants
county_name <- c("Gwinnett", "Newton", "Rockdale")
disease_program_list <- c("Vectorborne", "Enteric", "Vaccine Preventable")

# 3. Load Data
# Assuming 'disease_program' (singular) is the column name in county_data
county_data <- qs::qread(here("output", "combo_deidentified_clean.qs"))


# --- Branding Validation Check ---
current_theme <- getOption("plotly_template")

if (!is.null(current_theme)) {
  message("✅ Branding Active: Using ", current_theme$layout$font$family)
} else {
  warning("⚠️ WARNING: No brand template detected. Plots will use default Plotly styles!")
}


# 5. Create the Master Combination Table (The input for pmap)
county_program_combinations <- expand.grid(
  county = county_name,
  program = disease_program_list, # Renamed the column to 'program' for clarity
  stringsAsFactors = FALSE
)



# R/generate_plots.R (Cont.)

# 1. GENERATE: Use pmap to apply the function to every row (all 9 combinations)
county_plot_list <- county_program_combinations %>%
  mutate(
    data_input = list(county_data),

    output = purrr::pmap(
      # CRITICAL CHANGE: Pass the new 'data_input' column first
      list(data_input, county, program),
      .f = ~ process_data_and_plot(
        data_input = .x,          # The county_data frame
        county_name = .y,         # The county name
        program_name = .z        # The program name
      )
    )
  )

# 2. SAVE: Extract necessary components and use pwalk to save the RDS files

county_plot_list %>%
  # Safely extract the nested elements required for saving
  mutate(
    plot_object_list = map(output, ~ .x[["plot_object"]]), # contains $plot and $sr_text
    county_name = map_chr(output, ~ .x[["county"]]),
    program_name = map_chr(output, ~ .x[["program"]])
  ) %>%

  # pwalk iterates over rows and executes the side-effect (saving the file)
  purrr::pwalk(function(plot_object_list, county_name, program_name, ...) {

    # 🌟 Standardize names for file path 🌟
    safe_program_name <- gsub(" ", "_", tolower(program_name))
    safe_county_name <- gsub(" ", "_", tolower(county_name))

    file_path <- here::here(
      "output",
      paste0(safe_county_name, "_", safe_program_name, "_plot.rds")
    )

    # Save the list containing the plot and sr_text
    saveRDS(plot_object_list, file_path)
    message("Saved: ", file_path)
  })

# This script is now complete and generates all 9 files!

#
#
# # 1. Access the list in the 'output' column at Row 4
# plot_output_list <- county_plot_list$output[[4]]
#
# # 2. Access the 'plot_object' element
# plot_data <- plot_output_list$plot_object
#
# # 3. Access the actual plot object ($plot) and print it
# print(plot_data$plot)
#
#
# print(county_plot_list$output[[3]]$plot_object$plot)
#
#
#
#
#
#
# # Access Row 5 (Newton / Enteric)
# newton_enteric_output <- county_plot_list$output[[3]]
#
# # Access the plot object (which should contain $plot and $sr_text)
# plot_data <- newton_enteric_output$plot_object
#
# # 🖼️ Preview the plot
# print(plot_data$plot)
#
#
#
#
# names(county_plot_list)
# unique(county_data$disease_program)
#
# # 1. Access the first row's 'output' column
# first_output <- county_plot_list$output[[1]]
#
# # 2. Access the 'plot_object' element within that list
# plot_data <- first_output$plot_object
#
# # 3. Access the actual plot
# actual_plot <- plot_data$plot
#
# # 4. Preview it
# print(actual_plot)
#
#
#
#
# Gwinnett_vpds_plot <- list(
#   plot = gwinnett_vaccine_preventable_plot$plot, # The actual plot object (e.g., plotly, ggplot)
#   sr_text = Gvpds_alt_text
# )
#
#
#
#
# # After the Gwinnett_vpds_plot list is created:
#
# # Access the plot object within the list
# my_plot <- Gwinnett_vpds_plot$plot
#
# # Use the appropriate print/render function for interactive plots
# # plotly objects often require an explicit print() command in a script
# print(my_plot)
