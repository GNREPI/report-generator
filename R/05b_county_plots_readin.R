# -----
# County Plots Read in Script
## Script to read all saved RDS files into one master data frame.
# This ensures the global environment is clean when the report runs.
#
# Author: Allene Stephens
# Date: 10/20/2025
#----

# 1. Setup and Environment ----
# Loads required packages and custom functions.
# The 'packages.R' script should contain all library calls.
# Load the 'here' package so it can be used later
library(here)
source(here("R","packages.R"))
source("R/functions.R")



county_name <- c("Gwinnett", "Newton", "Rockdale")
program_name <- c("Vectorborne", "Enteric", "Vaccine Preventable")



# 2. Build file paths for all plots
plot_paths <- expand.grid(
  county = county_name,
  program = program_name,
  stringsAsFactors = FALSE) %>%
  mutate(
    safe_program = gsub("-", "_", tolower(program)),
    safe_program = gsub(" ", "_", tolower(program)),
    file_name = paste0((tolower(county)), "_", safe_program, "_plot.rds"),
    file_path = here("output", file_name)
  )


# 4. Load all plots into a nested list
plot_object <- plot_paths %>%
  mutate(plot_object = map(file_path, read_plot_safe)) %>%
  split(.$county) %>%     # split by county
  map(~ set_names(.x$plot_object, .x$program))  # name by program

# --- FIX: Define a single, master file path ---
master_plot_list_path <- here("output", "all_dashboard_plots.rds")

# Save the entire nested list of plots to the single master file
saveRDS(plot_object, master_plot_list_path)
message("Saved master plot list to: ", master_plot_list_path)

# saveRDS(plot_object, file_path) # Now saves the correct object
# message("Saved: ", file_path)
#
#
#
# # 5. Now you can reference plots like this:
# print(plot_object[["Gwinnett"]][["Vaccine Preventable"]])
# plot_objects[["Gwinnett"]][["Vectorbornes"]]
# plot_objects[["Newton"]][["Enterics"]]


















