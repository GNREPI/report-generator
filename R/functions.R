
# R/functions.R
# Purpose: To store reusable code that can be called from multiple places.

get_brand <- function() {
  template_obj <- getOption("plotly_template")
  list(
    colors   = surv_colors,
    fonts    = brand_fonts,
    # patterns = brand_patterns,
    template = template_obj,
    brandon  = brand_fonts
  )
}


# ----Helper Function #1: Import & Clean Data----
## File: 01_import_data.R
# Import the raw data and standardize its column names for easier manipulation.
# The `clean_names()` function from `janitor` converts names to a consistent, snake_case format.
# This script should be run in a secure, controlled environment, NOT your public dashboard repo
# Load your raw Excel data

import_and_clean <- function(file_path){
  df <- import(file_path) %>%
  clean_names() %>%  # Standardize column names early
  mutate(patientid = as.character(patientid))
# Checking for essential columns after cleaning
# Check if all columns exist using purrr::every()
# Create a vector of all required column names
required_columns <- c(
  "patientid", "dob", "doo", "adminstatus",
  "county", "lastname", "firstname"
)
# Use purrr::every() to check if all required columns are in the data frame
columns_exist <- purrr::every(required_columns, ~ .x %in% colnames(df))
# If not every column exists, identify the missing ones and stop
if (!columns_exist) {
  missing_cols <- setdiff(required_columns, colnames(df))
  stop(paste0("The following required columns are missing: ",
              paste(missing_cols, collapse = ", "),
              ". Script stopped."))
}
  return(df)
# Return the data frame only if all checks pass
# The code will only proceed past this point if all columns are present
}




# Helper Function #2: Wrangle Data----
# File: enteric_script.R
# File: vax_script.R
# File: VBD_script.R
wrangle <- function(file_path){

  #Import data
cleaned <- qread(file_path) %>%
  janitor::clean_names() %>%
  mutate(patientid = as.character(patientid)) %>%
  # Filter data
  filter(adminstatus %in% c("CONFIRMED", "PROBABLE")) %>%
  # Convert to title case
  mutate(
    county = to_title_case(county),
    adminstatus = to_title_case(adminstatus))%>%

# Return the cleaned data frame
return(cleaned)
}




# Helper Function #3: Create pseudo_patient_id----
# File: enteric_script.R
# File: vax_script.R
# File: VBD_script.R
# Use distinct(patientid) to ensure each unique patient gets ONE pseudo_patient_id

patient_id_mapping <- function(df) {
  # Pipe the raw_PHI_data into the cleaning and mutation steps
  # The result of this pipe chain is assigned to the 'mapping_table' variable
  mapping_table <- df %>%
    # Select the two columns we need to create a unique ID
    select(patientid, county) %>%
  distinct() %>% # Ensure each unique patient gets ONE pseudo_patient_id
  dplyr::mutate(pseudo_patient_id = uuid::UUIDgenerate(n = n())) # Create a *NEW* column for the pseudo ID

  # Return the new mapping table
    return(mapping_table)
}




# Helper Function #4: Date Validity and Formatting----
# File: enteric_script.R
# File: vax_script.R
# File: VBD_script.R
# Define common date formats to try, prioritizing MM/DD/YYYY explicitly for 4-digit years
#Pre-process Dates and Split Data Based on Date Validity
# Attempt to parse 'dob', 'dod', 'doo', 'dooe' using multiple common formats.
fix_dates <- function(df){
date_formats <- c("%m/%d/%Y", # Primary: MM/DD/YYYY (e.g., 01/15/1892)
                  "%m-%d-%Y", # MM-DD-YYYY (e.g., 01-15-1892)
                  "mdy", # lubridate's shorthand for Month Day Year (general)
                  "ymd", "dmy", # Other common orders if mixed formats exist
                  "Y-m-d", "d-m-Y") # More explicit formats for various delimiters)

# Temporarily create a version with parsed dates and original date strings
raw_parsed_dates <- df %>%
  mutate(
    dob_parsed = lubridate::parse_date_time(dob, orders = date_formats, quiet = TRUE),
    doo_parsed = parse_date_time(doo, orders = date_formats, quiet = TRUE)
  ) %>%
  mutate(onset_year = year(doo_parsed))

# Separate rows with ANY unparsable critical date for investigation
# Now checking all four key date columns as per your function's comments.
raw_invalid_dates <- raw_parsed_dates %>%
  filter(is.na(dob_parsed) | is.na(doo_parsed))

# Filter for rows with all valid dates to proceed with de-identification
raw_valid_dates <- raw_parsed_dates %>%
  filter(!is.na(dob_parsed), !is.na(doo_parsed))

# Return both data frames as a list
return(list(
  valid = raw_valid_dates,
  invalid = raw_invalid_dates
))
}





# Helper Function #4b: CALCULATE AGE GROUPS----
# Calculate age at onset from DOB and date of onset, then assign to age group categories
calculate_age_group <- function(df) {
  df %>%
    mutate(
      # Parse dates if they're still characters
      dob_date = mdy(dob),
      doo_date = mdy(doo),
      # Calculate age at onset in years
      age_at_onset = as.numeric(difftime(doo_date, dob_date, units = "days")) / 365.25,
      # Create age group categories
      age_group = case_when(
        is.na(age_at_onset) ~ "Unknown",
        age_at_onset < 5 ~ "0-4 years",
        age_at_onset < 12 ~ "5-11 years",
        age_at_onset < 20 ~ "12-19 years",
        age_at_onset < 30 ~ "20-29 years",
        age_at_onset < 40 ~ "30-39 years",
        age_at_onset < 50 ~ "40-49 years",
        age_at_onset < 60 ~ "50-59 years",
        age_at_onset < 70 ~ "60-69 years",
        age_at_onset < 80 ~ "70-79 years",
        age_at_onset >= 80 ~ "80+ years",
        TRUE ~ "Unknown"
      ),
      # Convert to factor with proper ordering
      age_group = factor(age_group, 
                        levels = c("0-4 years", "5-11 years", "12-19 years", 
                                  "20-29 years", "30-39 years", "40-49 years", 
                                  "50-59 years", "60-69 years", "70-79 years", "80+ years", "Unknown"),
                        ordered = TRUE)
    ) %>%
    # Keep the age_group column, remove intermediate calculations
    select(-dob_date, -doo_date, -age_at_onset)
}


# Helper Function #5: DE-IDENTIFICATION PIPELINE----
# File: enteric_script.R
# File: vax_script.R
# File: VBD_script.R
# --- START OF YOUR MAIN DE-IDENTIFICATION PIPELINE ---
joined_deidentified <- function(df_clean, mapping_table){

# Apply the join and select operations to the *valid* data frame
  df_deid_clean <- df_clean %>% # <--- THIS IS THE KEY CHANGE

  # Calculate age groups BEFORE removing DOB
  calculate_age_group() %>%

  # Join with the mapping to get the pseudo ID
  left_join(mapping_table, by = c("patientid", "county")) %>%

  # --- Step 3: Remove true duplicate *records* from the de-identified data (if desired) ---
  distinct() %>%    # Removes rows where ALL values across ALL columns are identical

  # Remove ALL PHI and intermediate age columns
  select(
    pseudo_patient_id,
    everything(),
    -dob,
    -patientid,
    -lastname,
    -firstname,
    -dob_parsed,
    -doo_parsed)
# Example: remove other PHI columns if they exist in your raw_data_with_phi
# ... (go through the 18 HIPAA identifiers and remove or generalize)

return(df_deid_clean)
}




# Helper Function #6: Join Population Data to Disease Counts----

  # Join Population Data to Disease Counts ---
    # Use a left_join to bring the population into the counts data.
    # This ensures all disease counts are kept, and population is added where county matches.
    data_with_population <- function(df, county_pop){
      df_pop <- df %>%
        # Clean both data frames' county columns first
        mutate(county = trimws(tolower(county))) %>%
        left_join(
          county_pop %>% mutate(county = trimws(tolower(county))),
          by = "county")

          return(df_pop)
          }




# Helper Function #7: District Case Rates ----

# District Case Rates
  district_pop <- function(df){
    district_rates <- df %>%
        group_by(disease, onset_year, disease_program) %>%
        summarise(
          total_pop = sum(population),
          total_cases = n(),
          .groups = 'drop') %>%
        mutate(cumulative_rate = round((total_cases / total_pop) * 100000, digits = 2)) %>%
        mutate(disease = as.factor(disease)) %>%

        select(disease, onset_year, cumulative_rate, disease_program)
    return(district_rates)
    }




# Helper Function #8: District alt text metrics for plots ----
  # File: 04_district_plot.R
  # Defining alt_text_string for plots
  # Step 1: Key metrics

  district_alt_text_string <- function(data){
  max_disease_rate <- round(max(data$cumulative_rate, na.rm = TRUE), 1)  # round to 1 decimal
  if (is.infinite(max_disease_rate) || max_disease_rate < 0) {
    return("No valid case rate data found for this disease program.")
  }

  valid_data <- data[!is.na(data$cumulative_rate),]
  max_disease_disease <- valid_data$disease[which.max(valid_data$cumulative_rate)]

  if (length(max_disease_disease) == 0) {
    max_disease_disease <- "No valid max case rate data found for this disease program."
  } else {
    max_disease_disease <- max_disease_disease[1]
  }
  # max_disease_disease <- data$disease[which.max(data$cumulative_rate)]

  num_disease_diseases <- length(unique(data$disease))

  # Step 2: Handle pluralization
  illness_word <- ifelse(num_disease_diseases == 1, "Illness", "Illnesses")

  # Step 3: Optional: list all disease names (comma separated)
  disease_list <- paste(sort(unique(data$disease)), collapse = ", ")

  # Step 4: Construct dynamic alt text
  district_alt_text <- paste0(
    "District-wide case rates for ", num_disease_diseases, " for this",
    tolower(illness_word), ". ",    "The maximum rate is ", max_disease_rate,
    " per 100,000 population, associated with ", max_disease_disease, ". ",
    "Diseases included: ", disease_list, "."
  )

  # Example output:
  # "District-wide case rates for 6 Vectorborne Illnesses. The maximum rate is 48.3 per 100,000 population, associated with West Nile Virus. Diseases included: West Nile Virus, Dengue, Zika, Chikungunya, Yellow Fever, Malaria."
 return(district_alt_text)
  }




# Helper Function #9: Safely read plot objects ----
  read_plot_safe <- function(path) {
    if (file.exists(path)) {
      readRDS(path)
    } else {
      warning(paste("File not found:", path))
      return(NULL)
    }
  }




# Helper Function #10: District Plot ----

plot_district_rates <- function(data, title_text, district_alt_text){
  brand <- get_brand()
  n_rows <- nrow(data)
   base_colors <- unname(brand$colors)
  plot_colors <- rep(base_colors, length.out = n_rows)

  # --- Step 0: Ensure diseases have a color ---
  # Strip the names so Plotly just cycles through the hex codes in order

  title_font <- brand$brandon$chart_title
  axis_font <- brand$template$axis
  footnote_font <- brand$fonts$footnote
  hover_font <- brand$fonts$hover

  # Ensure data is sorted by rate so the colors apply consistently to top values
  data <- data %>% arrange(desc(cumulative_rate)) %>%
  mutate(rate_label = sprintf("%.2f", cumulative_rate))

  # 2. CONTRAST LOGIC (For the inside labels)
  # This ensures the disease name is readable against the bar  color
get_contrast <- function(hex) {
  rgb <- col2rgb(hex)
  luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
  if (luminance > 0.5) "#333333" else "#FFFFFF"
  }
  text_colors <- sapply(plot_colors, get_contrast)
  title_font <- brand$brandon$chart_title
  axis_font <- brand$template$axis
  footnote_font <- brand$fonts$footnote
  hover_font <- brand$fonts$hover
  data <- data %>%
    arrange(desc(cumulative_rate)) %>%
    mutate(t_color = text_colors,
           # Formatted string for the outside labels
           rate_label = sprintf("%.2f", cumulative_rate))

  # --- Step 1: Dynamic margins ---
  max_label_chars <- max(nchar(as.character(data$disease)))
  left_margin <- 80 + max_label_chars * 7  # dynamically expand left margin
  max_x_chars <- max(nchar(sprintf("%.2f", data$cumulative_rate)))
  bottom_margin <- 50 + max_x_chars * 8    # dynamic bottom margin for x-axis

  # --- Step 2: Label font size ---
  label_font_size <- 12

  # --- Step 3: Create Plotly Horizontal Bar Chart ---
  df <- plot_ly(
    data = data,
    x = ~cumulative_rate,
    y = ~reorder(disease, cumulative_rate),
    type = "bar",
    orientation = 'h',
    marker = list(
    color = plot_colors,
  line = list(color = "white", width = 1) # Outline helps distinguish patterns
    ),
  text = ~paste0("<b>", disease, "</b><br>",
                 "Rate: ", rate_label),
  hoverinfo = "text",
  textposition = "none"
  ) %>%
  # 4. PERCENTAGE (VALUE) OUTSIDE THE END OF THE BAR
  # We use add_annotations to place the rate labels precisely
  add_annotations(
    x = ~cumulative_rate,
    y = ~reorder(disease, cumulative_rate),
    text = ~paste0(" ", rate_label),
    showarrow = FALSE,
    xanchor = 'left', # Anchors text to start at the bar's end
    xshift = 10,      # Small 10px gap from the bar
    font = list(family = hover_font, size = 12, color = "#333333")
  ) %>%
    layout(
      font = list(family = hover_font),
      title = list(
        text = title_text,
        font = list(family = title_font),
          bordercolor = "grey",  # Optional: Border color of the tooltip
         namelength = -1,       # -1 displays the full name for 'color' variable in tooltip (e.g., 'disease')
        x = 0.5, y = 1.1, # Position (0 to 1, x=0.5 is center, y=0.95 is near top)
        xanchor = "center", yanchor = "bottom", # Align the top of the title with y=0.98
        pad = list(b = 10),
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = hover_font, size = 13)
        )), # Padding *below* the main chart title text itself
      xaxis = list(
        title = list(
          text = "Case Rate per 100,000", # Bold the X-axis category name (title)
           font = list(family = axis_font),
          standoff = 20, # Add 20px space between X-axis labels and X-axis title
          automargin = TRUE
        ),
        showticklabels = TRUE,
        tickangle = 0, # <--- 1. Rotate X-axis numbers (0 for horizontal numbers)
        tickfont = list(size = 10, family = axis_font),
        zeroline = TRUE, # Hide the zero line
        showgrid = TRUE,  # Show grid lines
        gridcolor = "lightgrey",
        autosize = TRUE
               ),
      yaxis = list(
        title = list(
          text = "<b>Illness Type</b>",
          font = list(family = axis_font),
          standoff = 6), # Add 20px space between y-axis labels and y-axis title
        tickangle = 0,
        ticks="outside", tickwidth=2, tickcolor='white', ticklen=5,
        tickfont = list(size = 12, family = axis_font),
        zeroline = TRUE, # Hide the zero line
        showgrid = FALSE,
        automargin = TRUE # <--- Ensures Y-axis labels have enough space and stay centered
             ),
      showlegend = FALSE,
    margin = list(t=80, r=80, b=bottom_margin, l = 20),
      # --- Bar chart specific layout (if type="bar") ---
      bargap = 0.5)  %>%  # Gap between bars (0 to 1)

      # --- ADD FOOTNOTES AS ANNOTATIONS HERE ---
      add_annotations(
        # Footnote 1: Data Source
          text = "<b>Data Source:</b> SENDSS Georgia Department of Public Health, 2024",
          xref = "paper", # Position relative to the entire plot area (0 to 1)
          yref = "paper", # Position relative to the entire plot area (0 to 1)
          x = 0,         # X position (0 = left edge of plot)
          y = -0.30,     # Y position (negative values place it below the plot area)
          # Adjust 'y' based on your bottom margin and font size
          showarrow = FALSE, # Do not show an arrow
          xanchor = "left",  # Anchor the text to its left side
          yanchor = "bottom",  # Let plotly determine vertical anchor (usually bottom)
          font = list(family = footnote_font))  %>%

   config(displayModeBar = TRUE,
          modeBarButtonsToAdd = list("v1hovermode", "hovercompare"))

 return(
 # Return both plot and alt text
 list(plot = df, sr_text = district_alt_text))
 }




# Helper Function #11: County Parent ----
# File: generate_county_plots.R

 process_data_and_plot <- function(data_input, county_name, program_name) {

   # 1. ADD THE REAL-TIME MESSAGE HERE
   # Use 'toupper' to make it stand out in the console
   message("🚀 Processing: ", toupper(county_name), " - ", program_name, "...")

     # 1A. Filter the raw data and store it
     # Use the COLUMN name 'disease_program'
     county_and_pgm_filter <- data_input %>%
       filter(county == !!county_name,
              disease_program == !!program_name)

     # 1B. CHECK FOR EMPTY DATA FRAME on the raw filter result
     # If there are NO raw cases for this county/program combination, return NULL
     # 3. SHORT-CIRCUIT: If there are NO raw cases, exit early
     if (nrow(county_and_pgm_filter) == 0) {
       message("ℹ️  Skipping: No data found for ", county_name, " ", program_name)
       # Return a structured list with a NULL plot so the dashboard knows to skip it
        return(list(county = county_name, program = program_name,
                   plot_object = list(plot = NULL, sr_text = "No cases reported for this period.")))
     }

  # --- Data is present, proceed to rates and plotting ---

     # 1. Filter and Calculate Rates
     county_and_pgm_rates <- county_and_pgm_filter %>%
             county_rates(county_name = county_name) # Assuming county_rates expects county_name


     # 2. Generate Alt Text
     county_alt_text <- county_alt_text_string(county_and_pgm_rates, program_name)

     # 3. Generate Plot Object
     GNRplot_sr_list <- generate_county_plots(
       data = county_and_pgm_rates,
       title_text = paste("Case Rates from ", program_name, " in ", county_name, " County"),
       county_alt_text = county_alt_text,
       county_name = county_name,
       disease_program = program_name
     )

     # 4. Return a consistently structured list
     return(list(
       county = county_name,
       program = program_name,
       plot_object = GNRplot_sr_list # This contains $plot and $sr_text
     ))
   }




 # Helper Function #12: County Rates ----
 # File: Gwinnett plots script.R----

  # --- Calculate Disease Counts per Disease and County ---

county_rates <- function(df, county_name){
    filtered_county_rates <- df %>%
      filter(county == county_name)

    # 2. Summarize the counts, then calculate the rate (assuming 'population' is already
    #    the correct denominator for the county/year group)
    county_rates <-  filtered_county_rates %>%
    group_by(disease_program, disease, population, onset_year) %>%
      summarise(disease_count = n(),
                .groups = 'drop') %>%

      # 3. Combine factor conversion and rate calculation into one efficient mutate call
      mutate(disease = as.factor(disease),
        case_rate_per_100k = round((disease_count / population) * 100000, digits = 2))
    return(county_rates)
}




# Helper Function #13: County alt text metrics for plots ----
# File: Gwinnett plots script.R

  # Defining alt_text_string for plots
  # Step 1: Key metrics
county_alt_text_string <- function(data, disease_program){
  # 🌟 CRITICAL FIX: Check if the data frame is empty first
  if (nrow(data) == 0) {
    return(paste0("No case data reported for ", disease_program, " in this county."))
  }
  # 🌟 Step 1.5: CRITICAL FIX for the "-Inf" Warning
  # Check if all rates are NA. If so, stop here.
  if (all(is.na(data$case_rate_per_100k))) {
    return(paste0("Case data reported for ", disease_program, ", but rate calculations are unavailable."))
  }
  # 1. Use a variable to store the max index safely
  max_idx <- which.max(data$case_rate_per_100k)

  # 2. Check if we actually found a maximum
  if (length(max_idx) > 0) {
    # This only runs if there's at least one non-NA rate
    max_val          <- data$case_rate_per_100k[max_idx]
    max_disease_rate <- round(max_val, 1)
    max_disease      <- as.character(data$disease[max_idx])
  } else {
    # Fallback if everything is NA
    max_disease_rate <- "N/A"
    max_disease      <- "unknown diseases"
  }

  num_diseases <- length(unique(data$disease))

    # Step 2: Handle pluralization
    illness_word <- ifelse(num_diseases == 1, "Illness", "Illnesses")

    # Step 3: Optional: list all disease names (comma separated)
    disease_list <- paste(sort(unique(data$disease)), collapse = ", ")

    # Step 4: Construct dynamic alt text
    county_alt_text <- paste0(
      "Case rates for ", num_diseases, "for this", disease_program,
      illness_word, ". ", "The maximum rate is ", max_disease_rate, " per 100,000
      population, associated with ", max_disease, ". ", "Diseases included: ",
      disease_list, "."
    )

    # Example output:
    # "Case rates for 6 Vectorborne Illnesses. The maximum rate is 48.3 per 100,000 population, associated with West Nile Virus. Diseases included: West Nile Virus, Dengue, Zika, Chikungunya, Yellow Fever, Malaria."
    return(county_alt_text)
  }




# Helper Function #14: Define safe read function ----

  read_plot_safe <- function(path) {
    if (file.exists(path)) {
      readRDS(path)
    } else {
      warning(paste("File not found:", path))
      return(NULL)
    }
  }




 # Helper Function #15: read the plot object ----

  read_plot_data <- function(path) {
    tryCatch(readRDS(path), error = function(e) return(NULL))
  }




# Helper Function #16: County Plots Loop ----
# Bar Charts
generate_county_plots <- function(data, title_text, county_alt_text, county_name, disease_program, type){

  brand <- get_brand()

  # 1. Basic Setup and branding
  n_rows <- nrow(data)
  base_colors <- unname(brand$colors)
  base_patterns <- if (is.null(brand$patterns)) "" else brand$patterns
  plot_colors <- rep(base_colors, length.out = n_rows)
  plot_patterns <- rep(base_patterns, each = max(1, length(base_colors)), length.out = n_rows)

  title_font    <- brand$fonts$chart_title
  axis_font     <- brand$fonts$axis
  footnote_font <- brand$fonts$footnote
  hover_font    <- brand$fonts$hover

  # --- DYNAMIC settings CALCULATION ---
  # Calculate height inside the function
  calc_height <- max(5, 4 + (n_rows * 0.5))

  # --- CORRECTED DATA PREP ---
  # Only arrange if the column exists (usually for bar charts)
  if ("case_rate_per_100k" %in% names(data)) {
    data <- data %>% arrange(desc(case_rate_per_100k))
  }

  # Logic for dynamic labels and margins
  label_font_size <- 14
  dynamic_bargap <- max(0.1, min(0.4, 1 / (n_rows + 3)))

  # Base font size (you can tweak this)
  base_font_size <- 20

  # Adjust font size: more digits → smaller font
  label_font_size <- 14
  # max(14, base_font_size - (max_digits -1) * 1.5)


  # --- Calculate dynamic margins based on label lengths ---
  max_label_chars <- max(nchar(as.character(data$disease)))

  max_x_chars <- max(nchar(sprintf("%.2f", data$case_rate_per_100k)))


  # --- Step 3: Conditional Plot Creation ---
      #   --- conditional check to prevent errors ---
    if (n_rows > 0){

   #     Create the plotly chart for the current county ---
    county_plot_result  <- plot_ly(
        data = data,
        x = ~ case_rate_per_100k,
        y = ~reorder(disease, case_rate_per_100k),
        type = "bar",
        orientation = 'h',
        marker = list(
          color = unname(plot_colors),
          pattern = list(shape = plot_patterns, solidity = 0.5),
          line = list(color = "white", width = 1) # Outline helps distinguish patterns
        ),
      # Use texttemplate for full control over the text and its color
      texttemplate = ~paste0('<b style="color:black; font-size:', label_font_size, 'px;">',
                             sprintf("%.2f", case_rate_per_100k), '</b>'),
      textposition = 'outside', # <--- NEW: Position the text label outside the bar
      hovertext = ~paste(
        "<b>County:</b>", county_name, "<br>",
        "<b>Disease:</b>", disease, "<br>",
        "<b>Case Rate (per 100k):</b>", sprintf("%.2f", case_rate_per_100k),"<br>"
      ),
      hoverinfo = "text" # Tells plotly to use the 'text' aesthetic for hover
      ) %>%
  layout(
    template = brand$template, # Force the global template
    title = list(
           text = title_text,
           font = list(family = title_font),
           bordercolor = "grey",  # Optional: Border color of the tooltip
           namelength = -1,       # -1 displays the full name for 'color' variable in tooltip (e.g., 'disease')
           x = 0.5, y = 1.2, # Position (0 to 1, x=0.5 is center, y=0.95 is near top)
           xanchor = "center", yanchor = "bottom", # Align the top of the title with y=0.98
           pad = list(b = 30) # Padding *below* the main chart title text itself
     ),
      xaxis = list(
           title = list(
             text = "<b>Case Rates</b><br><b>(per 100k population)</b>", # Bold the X-axis category name (title)
             font = list(family = axis_font)
             ),
             standoff = 5, # Add 20px space between X-axis labels and X-axis title
            showticklabels = TRUE,
            tickangle = 0,
            tickfont = list(size = 10),
            zeroline = TRUE,
            showgrid = TRUE,
            range = c(0, quantile(data$case_rate_per_100k, 0.95, na.rm=TRUE) * 1.2),
              # c(0, max(county_rates$case_rate_per_100k) * 1.2),
            gridcolor = "lightgrey",
            automargin = TRUE,
            minor = list(
              showgrid = TRUE,
              gridcolor = "lightgrey",
              dtick = 25)
            ),
      yaxis = list(
            title = list(
              text = "<b>Illness Type</b>", font = list(family = axis_font),  standoff = 10),
            tickangle = 0,
            ticks = "outside",
            tickwidth = 2,
            tickcolor = 'white',
            ticklen = 5,
            tickfont = list(size = 12),
            zeroline = TRUE,
            showgrid = FALSE,
            autosize = TRUE
      ),
      showlegend = FALSE,
      bargap = dynamic_bargap,
      margin = list(
            t = 80,
            b = 175,
            r = 100,
            l = 200
          ),
      annotations = list(
            list(
              text = "<b>Data Source:</b> SENDSS Georgia Department of Public Health, 2024",
              xref = "paper",
              yref = "paper",
              x = 0,
              y = -.50,
              showarrow = FALSE,
              xanchor = "left",
              yanchor = "bottom",
              font = list(family = footnote_font)
            )
            )
    ) %>%
    config(responsive=TRUE)     # dynamic plot
    } else {
      #     If the data frame is empty, print a message instead of a plot
      # 1. Provide a single, helpful console message (Optional)
      message("ℹ️ No data for ", county_name, " (", disease_program, ")")

              # 2. Create a clean empty plot that tells the user WHICH data is missing
       plot_result <- plotly_empty() %>%
         layout(title = list(
         text = paste0("No ", disease_program, " data available for ", county_name, " County."),
         font = list(family = brand$fonts$body, size = 14, color = "grey"),
         x = 0.5, y = 0.5 # Centered in the middle of the box
       ))
}
   # Returns the plot object (or NULL) enclosed in a consistently named list.
  return(list(
    plot = county_plot_result,
    sr_text = county_alt_text,
    plot_height = calc_height
    ))
  }




# Helper Function #17: Gender Pie Chart ----
# Gender
# % of gender
gender_per <- function(data){
  gender_summary <- data %>%
    group_by(gender) %>%
    summarise(
      no_gender = n()
    ) %>%
    ungroup() %>%
    mutate(
      per_total = round(no_gender/sum(no_gender)*100, 1),
      demo = gender,
      label = paste0("<b>", gender, ": ", per_total, "%</b>")) %>%
    # Filter out any rows where the percentage is zero
    filter(per_total > 0)
  return(gender_summary)
}




# Helper Function #19: Master Pie Chart Generator ----
# File: 04_district_pie_demo.R
# This function handles data preparation, alt text generation, and plotting
# for any specified demographic column (gender, race, ethnicity, etc.).

generate_pie_chart <- function(
    data,
    group_col_name,
    disease_program,
    county_name,
    title = NULL) {

  brand <- get_brand()

  # Define local variables
  n_rows <- nrow(data)
  base_colors <- unname(brand$colors)
  body_font <- brand$fonts$body
  footnote_font <- brand$fonts$footnote
  hover_font <- brand$fonts$hover
  title_font <- brand$fonts$chart_title

  # --- Step 0: Contrast Function (The Luminance Logic) ---
  get_contrast <- function(hex) {
    rgb <- col2rgb(hex)
    luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
    if (luminance > 0.5) "#333333" else "#FFFFFF"
  }

  # CRITICAL CHECK: Ensure the grouping column exists
  if (!(group_col_name %in% names(data))) {
    stop(paste0("Error: The column '", group_col_name, "' does not exist."))
  }

  # --- 1. DATA PREPARATION ---
  group_sym <- sym(group_col_name)

  data_summary <- data %>%
    group_by(!!group_sym) %>%
    summarise(n_cases = n(), .groups = "drop") %>%
    mutate(
      per_total = round(n_cases/sum(n_cases)*100, 1),
      demo = !!group_sym,
      label = paste0("<b>", demo, "<br>", per_total, "%</b>")
    ) %>%
    filter(per_total > 0) %>%
    arrange(desc(per_total))

  # --- 1.5 COLOR, CONTRAST, & POSITION MAPPING ---
  data_summary <- data_summary %>%
    mutate(
      slice_color = base_colors[1:n()],
      t_color = sapply(slice_color, get_contrast),
      label_position = ifelse(per_total >= 15, "inside", "outside")
    )

  # --- 2. ALT TEXT GENERATION ---
  if (nrow(data_summary) == 0) {
    alt_text <- paste0("No case data reported for ", disease_program, " in ", county_name, ".")
    return(list(plot = plotly_empty(), sr_text = alt_text))
  }

  max_val <- max(data_summary$per_total, na.rm = TRUE)
  max_group <- data_summary$demo[which.max(data_summary$per_total)]

  alt_text <- paste0(
    "Pie chart showing ", disease_program, " cases by ", group_col_name, ". ",
    max_group, " represents the largest portion at ", max_val, "%."
  )

  # --- 3. TITLE LOGIC ---
  if (is.null(title) || title == "") {
    title_text <- paste0(
      "<b>Distribution of ", str_to_title(disease_program), " Illnesses <br> by ", str_to_title(group_col_name), " in ", county_name, "</b>"
    )
  } else {
    title_text <- paste0("<b>", title, "</b>")
  }

  # --- 4. PLOT GENERATION ---
  pie_plot <- plot_ly() %>%
    add_trace(data = data_summary,
              labels = ~label,
              values = ~per_total,
              type = 'pie',
              text = ~label,
              # THE "EXPLOSION" EFFECT
              pull = rep(0.02, nrow(data_summary)),
              # DYNAMIC POSITIONING
              textposition = ~label_position,
              textinfo = 'text',
              # ACCESSIBLE FONTS
              insidetextfont = list(family = body_font, color = ~t_color),
              outsidetextfont = list(family = body_font, color = '#333333'),
              insidetextorientation = 'radial',
              hoverlabel = list(font = list(family = hover_font)),
              marker = list(
                colors = ~slice_color,
                line = list(color = '#FFFFFF', width = 2) # Delineation border
              )
    ) %>%
    layout(
      title = list(
        text = title_text,
        font = list(family = title_font),
        x = 0.5, xanchor = 'center', y = 0.95
      ),
      legend = list(font = list(family = body_font)),
      showlegend = FALSE,
      margin = list(l=70, r=70, b=70, t=70, pad=3),
      annotations = list(
        list(
          text = "<b>Data Source:</b> SENDSS Georgia Department of Public Health, 2024",
          xref = "paper", yref = "paper",
          x = 0, y = -.15, showarrow = FALSE, xanchor = "left",
          font = list(family = footnote_font)
        )
      )
    ) %>%
    config(responsive = TRUE)

  return(list(plot = pie_plot, sr_text = alt_text))
}






# Helper Function #19b: Master Pie Chart Explosion ----
# File: 04_district_pie_demo.R
# This function handles data preparation, alt text generation, and plotting
# for any specified demographic column (gender, race, ethnicity, etc.).

generate_pie_explosion <- function(
    data,
    group_col_name,
    disease_program,
    county_name,
    title = NULL) {

  brand <- get_brand()

  # Define local variables from the brand list
  n_rows <- nrow(data)
  base_colors <- unname(brand$colors)

  # --- Step 0: Ensure diseases have a color ---
  # Strip the names so Plotly just cycles through the hex codes in order
  get_contrast <- function(hex) {
    rgb <- col2rgb(hex)
    luminance <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
    if (luminance > 0.5) "#333333" else "#FFFFFF"
  }
  # # colors <- unname(brand$colors)
  body_font <- brand$fonts$body
  footnote_font <- brand$fonts$footnote
  hover_font <- brand$fonts$hover
  title_font <- brand$fonts$chart_title

  # CRITICAL CHECK 1: Ensure the grouping column exists
  if (!(group_col_name %in% names(data))) {
    stop(paste0("Error: The column '", group_col_name, "' does not exist."))
  }

  # --- 1. DATA PREPARATION ---
  group_sym <- sym(group_col_name)

  data_summary <- data %>%
    group_by(!!group_sym) %>%
    summarise(
      n_cases = n(),
      .groups = "drop") %>%
    mutate(
      per_total = round(n_cases/sum(n_cases)*100, 1),
      demo = !!group_sym,
      label = paste0("<b>", demo, "<br>", per_total, "%</b>")) %>%
    filter(per_total > 0) %>%
    arrange(desc(per_total))

  # --- 1.5 COLOR & CONTRAST MAPPING (Move it here!) ---
  # Now that we know exactly how many slices we have, we assign colors.
  data_summary <- data_summary %>%
    mutate(
      slice_color = base_colors[1:n()],
      t_color = sapply(slice_color, get_contrast),
  label_position = ifelse(per_total >= 15, "inside", "outside")
  )
  # --- 2. ALT TEXT GENERATION ---
  # CRITICAL FIX: Check if the data frame is empty first
  if (nrow(data_summary) == 0) {
    alt_text <- paste0("No case data reported for ", disease_program, " in ", county_name, ".")
    # Return a placeholder plot/list if no data exists
    return(list(plot = plotly_empty(), sr_text = alt_text))
  }

  max_val <- max(data_summary$per_total, na.rm = TRUE)
  max_group <- data_summary$demo[which.max(data_summary$per_total)]
  # num_groups <- length(unique(data_summary$demo))
  # group_list <- paste(sort(unique(data_summary$demo)), collapse = ", ")

  alt_text <- paste0(
    "Pie chart showing ", disease_program, " cases by ", group_col_name, ". ",
    max_group, " represents the largest portion at ", max_val, "%."
  )

  # --- 3. TITLE LOGIC ---
  # If a title is NOT provided (NULL or empty string), generate the default title.
  # Otherwise, use the provided custom title.

  # Generate the title with a subheading
  aggregate_total <- sum(data_summary$n_cases)

  if (is.null(title) || title == "") {
    title_text <- paste0(
      "<b>Distribution of ", str_to_title(disease_program), " Illnesses <br> by ", str_to_title(group_col_name), " in ", county_name, "</b>",
      "<span style='font-size: 16px; color: #666;'></span>"
    )
  } else {
    title_text <- paste0("<b>", title, "</b><br><span style='font-size: 16px; color: #666;'>, </span>")
  }

  # --- 3. PLOT GENERATION ---
  pie_plot <- plot_ly() %>%
    add_trace(data = data_summary,
              labels = ~label,
              values = ~per_total,
              type = 'pie',
              text= ~label,
              # EXPLOSION & DELINEATION
              # 'pull' creates the gap. 0.05 is a 5% offset from center.
              pull = rep(0.02, nrow(data_summary)),

              # DATA LABELS & WCAG CONTRAST
              textinfo = 'text',
              textposition = ~label_position,
              insidetextfont = list(family = body_font, color = ~t_color), # Use the mapped column
              outsidetextfont = list(family = body_font, color = '#333333'),     # Dark text if the label moves outside
              insidetextorientation = 'radial',
              # 3. HOVER LABEL FONT
              hoverlabel = list(font = list(family = hover_font)),
              name = 'label',
              # SLICE BORDERS (Delineation)
              marker = list(
                colors = ~slice_color,
                line = list(color = '#FFFFFF', width = 2) # White border between slices
              )
    ) %>%
    layout(# 4. CHART TITLE FONT
      title = list(
        text = title_text,
        font = list(family = title_font),
        x = 0.5,
        xanchor = 'center',
        y = 0.95
      ),
      # 5. LEGEND FONT
      legend = list(font = list(family = body_font)),
      showlegend = FALSE,
      margin = list(l=200, r=100, b=75, t=75, pad=2),

      annotations = list(
        list(
          text = "<b>Data Source:</b> SENDSS Georgia Department of Public Health, 2024",
          xref = "paper",
          yref = "paper",
          x = 0,
          y = -.15,
          showarrow = FALSE,
          xanchor = "left",
          font = list(family = footnote_font)))) %>%

    config(responsive = TRUE)

  return(list(plot = pie_plot, sr_text = alt_text))
}









# # Helper Function #25: District Time Series Plot ----
# # District Time Series Plot

district_time_plot <- function(data, alt_text = NULL){

  # 1. Initialize the result as NULL so it always exists
  plot_result <- NULL

  # Pull brand objects
  colors <- getOption("brand_colors")
  if (is.null(colors)) colors <- RColorBrewer::brewer.pal(8, "Dark2")

  brand_font <- getOption("brand_font")
  if (is.null(brand_font)) brand_font <- "Arial"

  if (is.data.frame(data) && nrow(data) > 0){
    district_data_plot <- plot_ly(
        data = data,
        x = ~onset_year,
        y = ~cumulative_rate,
        type = "scatter",
        mode = 'lines+markers', # This is correct for lines with points
        color = ~disease,
        colors = colors,

        # --- ADD THIS FOR CUSTOM HOVER TEXT ---
        hovertext = ~paste(
          "<b>Disease Type: </b>", disease, "<br>",
          "<b>Case Rate: </b>", round(cumulative_rate, 2), "<br>"),
        hoverinfo = "text"
   ) %>%
      layout(
        hoverlabel = list(
          bgcolor = "white",       # Background color of the tooltip (e.g., "white", "lightblue", "#E0FFFF")
          font = list(family = brand_font, color = "black"),
          bordercolor = "grey",  # Optional: Border color of the tooltip
          namelength = -1        # -1 displays the full name for 'color' variable in tooltip (e.g., 'disease')
        ),
        title = list(
          text = paste0("<b>Hepatitis Case Rates", '<br>', "in ", " 2019 - 2024</b>"),
          font = list(family = brand_font, size = 16, color = "black"),
          x = 0.5, y = 1.2, # Position (0 to 1, x=0.5 is center, y=0.95 is near top)
          xanchor = "center", yanchor = "top", # Align the top of the title with y=0.98
          pad = list(b = 30)), # Padding *below* the main chart title text itself

        xaxis = list(
          title = list(
            text = "<b>Year</b>", # Bold the X-axis category name (title)
            font = list(family = brand_font, size = 14, color = "black"),
            standoff = 5 # Add 20px space between X-axis labels and X-axis title
          ),
          tickangle = 0, # <--- 1. Rotate X-axis numbers (0 for horizontal numbers)
          tickfont = list(size = 10), # <--- 7. Adjust size and bold numbers on the X-axis
          zeroline = TRUE, # Hide the zero line
          showgrid = TRUE,  # Show grid lines
          gridcolor = "lightgrey",
          automargin = TRUE,
          minor = list(
            showgrid = TRUE,
            gridcolor = "lightgrey", # Or a slightly lighter shade if desired, e.g., "lightgray"
            dtick = 5                 # Defines the step between minor ticks.
            # If your major ticks are every 100, dtick=50 places minor ticks in between.
            # Adjust 'dtick' value based on your 'range' and desired density.
          )
        ),
        yaxis = list(
          title = list(
            text = "<b>Case Rates per <br> 100k population</b>",
            font = list(family = brand_font, color = "black"),
            standoff = 10), # Add 20px space between y-axis labels and y-axis title
          tickangle = 0,
          ticks ="outside", tickwidth=2, tickcolor='white', ticklen=5,
          tickfont = list(size = 14), # <--- 3. Bold Y-axis category names (tick labels)
          zeroline = FALSE, # Hide the zero line
          showgrid = TRUE,
          automargin = TRUE# <--- Ensures Y-axis labels have enough space and stay centered
          # For horizontal bars with categorical Y-axis, plotly's default
          # behavior is to center labels on the bars. automargin handles overflow.
        ),
        showlegend = TRUE,
        #          --- Bar chart specific layout (if type="bar") ---
        margin = list(
          t = 100,
          b = 125,
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
            font = list(family = brand_font, size = 10, color = "black")
          )
        )
      ) %>%
      config(responsive = TRUE)

      plot_result <- district_data_plot

  } else {
    #     If the data frame is empty, print a message instead of a plot
    message(" ", stringr::str_to_title("No data available for district hepatitis cases."))
    plot_result <- plotly_empty() %>%
      layout(title = "No Data Available")
  }

  # Returns the plot object (or NULL) enclosed in a consistently named list.
  return(list(plot = plot_result, sr_text = alt_text))

}




# #Helper Function #26: County Time Series----

county_time <- function(df){
  filtered_county_time <- df %>%
    group_by(disease_program, disease, population, onset_year) %>%
    summarise(disease_count = n(),
              .groups = 'drop') %>%

    # 3. Combine factor conversion and rate calculation into one efficient mutate call
    mutate(disease = as.factor(disease),
           case_rate_per_100k = round((disease_count / population) * 100000, digits = 2))
  return(filtered_county_time)
}




# Helper Function #27: District Time Series----

# District Case Rates
# 1. Group the data by Year (onset_year) and Disease
district_time <- function(df){
  district_rates <- df %>%
    group_by(disease, onset_year) %>%
    # 2. Count the cases (n_cases) and calculate the population denominator
    #    (assuming 'population' is a column in 'combo' representing the district population)
    summarise(
      total_pop = sum(population),
      total_cases = n(),
      .groups = 'drop') %>%
    mutate(cumulative_rate = round((total_cases / total_pop) * 100000, digits = 2)) %>%
    mutate(disease = as.factor(disease)) %>%

    select(disease, onset_year, cumulative_rate)
  return(district_rates)
}




# Helper Function #28: County Time Series Plot----
# File: Gwinnett plots script.R
# File: Newton plots script.R
# File: Rockdale plots script.R
county_time_plot <- function(data, title_text, disease_program, alt_text, county) {

  brand <- get_brand()
  brand_font <- getOption("brand_font")
  if (is.null(brand_font)) brand_font <- "Arial"

  # --- 1. DATA VALIDATION ---
  if (!is.data.frame(data) || nrow(data) == 0) {
    return(list(
      plot = plotly_empty() %>% layout(title = list(text = "No data available")),
      sr_text = "No data available",
      plot_height = 5
    ))
  }

  # ---- Dynamic Y-axis scaling (WCAG-safe) ----
  max_rate <- max(data$case_rate_per_100k, na.rm = TRUE)

  # Fallback if all values are NA or zero
  if (!is.finite(max_rate) || max_rate <= 0) {
    y_max <- 1
  } else {
    # Add headroom (15%)
    padded_max <- max_rate * 1.15

    # Round to a "nice" number
    magnitude <- 10 ^ floor(log10(padded_max))
    y_max <- ceiling(padded_max / magnitude) * magnitude
  }

  dtick <- if (y_max <= 5) 1
  else if (y_max <= 20) 2
  else if (y_max <= 50) 5
  else if (y_max <= 100) 10
  else 25

  # Ensure we have at least 3 distinct colors from your brand palette
  # (If your brand palette is short, you might need to manually add hex codes here)
   my_colors <- unname(brand$colors[1:3])


  # 1. Initialize the result as NULL so it always exists
  timeplot_county <- NULL

  # 2. Safety Check for empty data
  title_font    <- brand$fonts$chart_title
  axis_font     <- brand$fonts$axis
  footnote_font <- brand$fonts$footnote

  # --- NEW: Step A: Create the label data ---
  #   This finds the "end" of each line
  label_data <- data %>%
    group_by(disease) %>%
    filter(onset_year == max(onset_year)) %>%
    ungroup()

  # --- NEW: Step B: Create the list of annotations ---
  # This builds a label for each disease found in label_data
  line_labels <- lapply(1:nrow(label_data), function(i) {
    list(x = label_data$onset_year[i],
         y = label_data$case_rate_per_100k[i],
         text = paste0("<b>", label_data$disease[i], "</b>"),
         xanchor = "left",
         yanchor = "middle",
         showarrow = FALSE,
         xshift = 12,
         font = list(size = 16, color = my_colors[i],
                     family = brand$fonts$axis)
    )
  })

      # --- NEW: Step C: Define the footnote ---
   footnote_obj <- list(
    text = "<b>Data Source:</b> SENDSS Georgia Department of Public Health, 2025",
    xref = "paper", # Position relative to the entire plot area (0 to 1)
    yref = "paper", # Position relative to the entire plot area (0 to 1)
    x = -.10,         # X position (0 = left edge of plot)
    y = -0.45,     # Y position (negative values place it below the plot area)
    # Adjust 'y' based on your bottom margin and font size
    showarrow = FALSE, # Do not show an arrow
    xanchor = "left",  # Anchor the text to its left side
    yanchor = "bottom",  # Let plotly determine vertical anchor (usually bottom)
    font = list(family = brand$fonts$footnote, color = "black")
  )

   all_annotations <- append(line_labels, list(footnote_obj))

     # --- Step D: Build the Plot ---
  timeplot_county <- plot_ly(
    data = data,
  x = ~onset_year,
  y = ~case_rate_per_100k,

  # --- MAPPING ATTRIBUTES ---
 color = ~disease, # 1. Different Colors
 colors = my_colors,     #    (Uses your specific brand colors)
 linetype = ~disease,
 # linetypes = my_lines,
 symbol = ~disease,
 # --- FIX 1: Enable Text Mode ---
 type = "scatter",
 mode = "lines+markers+text",
 # --- FIX 2: Define the Label Text & Position ---
 text = ~round(case_rate_per_100k, 1), # The number to show
 textposition = "top center",          # Places label above the dot
 # --- STYLES ---
 line = list(width = 6),
 marker = list(size = 14),

 # Optional: Font style for the data labels
 textfont = list(family = brand$fonts$axis, size = 12, color = "black"),

# --- ADD THIS FOR CUSTOM HOVER TEXT ---
  hovertext = ~paste(
    "<b>Illness Type: </b>", disease, "<br>",
    "<b>Year: </b>", onset_year, "<br>",
    "<b>Case Rate per 100,000: </b>", round(case_rate_per_100k, 1)),
  hoverinfo = "text"
  ) %>%
  layout(
    hoverlabel = list(font = list(family=brand$fonts$axis, size=12)
    ),
    title = list(
      text = paste0("<b>", title_text, "</b>"),
      font = list(family = brand$fonts$chart_title, color = "black"),
      x = 0.5, y = 0.96, # Position (0 to 1, x=0.5 is center, y=0.95 is near top)
      xanchor = "center", yanchor = "top", # Align the top of the title with y=0.98
      pad = list(b = 15)), # Padding *below* the main chart title text itself
    xaxis = list(tickfont = list(size=14),
      # title = list(
      #   text = "<b>Year</b>", # Bold the X-axis category name (title)
      #   font = list(family = brand$fonts$axis, color = "#4A4A4A")),
        tickmode = "linear",
         # standoff = 50, # Add 20px space between X-axis labels and X-axis title
      # FIX: Hardcode this to 1 so every year (2022, 2023, 2024) shows up
      dtick = 1,
      showgrid = TRUE,  # Show grid lines
      gridcolor = "lightgrey",
      automargin = TRUE
   ),
    yaxis = list(tickfont = list(size=14),
      title = list(
        text = "<b>case rates per <br> 100k population</b>",
        font = list(family = brand$fonts$axis, color = "#4A4A4A")),
      range = c(0, y_max),
      standoff = 45, # Add 20px space between y-axis labels and y-axis title
      zeroline = TRUE,
      zerolinewidth = 2,
      zerolinecolor = "black",
      showgrid = FALSE,
      automargin = TRUE,
      minor = FALSE,
   gridcolor = "rgba(0,0,0,0.1)"
   ),
     showlegend = FALSE,
    margin = list(
      t = 150,
      b = 155,
      r = 125,
      l = 125,
      pad = 5
    ),
    # --- ADD FOOTNOTES AS ANNOTATIONS HERE ---
    annotations = all_annotations)

timeplot_county <- lock_accessibility(
  p           = timeplot_county,
  colors      = my_colors,
  y_range     = c(0, y_max),
  dtick       = dtick,
  font_family = brand_font
)
  # 3. Return the standard bundle structure (Plot + Alt Text)
  return(list(plot = timeplot_county %>% config(responsive = TRUE),
              sr_text = alt_text,
              plot_height = 6, # Line charts usually work well at a fixed height
              data_view = data)
  )
}




# Helper Function #29: Saving and Loading Plot objects - The Wrapper function ----
# saves the plot to its folder and also loads it for the rmd file.
save_load_plot <- function(plot_obj, county, program, type = "bar") {

  # Clean names
  clean_county <- gsub(" ", "_", tolower(county))
  clean_program <- gsub(" ", "_", tolower(program))
  clean_program <- gsub("-", "_", tolower(clean_program))

  # File suffix based on plot type
  suffix <- switch(type,
                   "bar" = "_bar.rds",
                   "percent" = "_percent_plot.rds",
                   "trend"   = "_trend.rds",
                   "gender"  = "_gen_pie.rds",
                   "race"    = "_rac_pie.rds",
                   "eth"     = "_eth_pie.rds",
                   "case"    = "_cas_def_pie.rds",
                   stop("Unknown plot type: ", type)
  )

  # Ensure folder exists
  master_folder <- here::here("output", "plots")
  if (!dir.exists(master_folder)) dir.create(master_folder, recursive = TRUE)


  file_name <- paste0(clean_county, "_", clean_program, suffix)
  file_path <- here::here("output", "plots", file_name)


  # Save the plot individually
  saveRDS(plot_obj, file_path)
  message("💾 Saved (", type, "): ", file_name)


# -------------------------------
# Also append to a master list for all_dashboard_plots.rds
# -------------------------------

  # Append to master list
  master_file <- file.path(master_folder, "all_dashboard_plots.rds")
  if (file.exists(master_file)) {
    master_list <- readRDS(master_file)
  } else {
    master_list <- list()
  }

  if (is.null(master_list[[county]])) master_list[[county]] <- list()
  if (is.null(master_list[[county]][[program]])) master_list[[county]][[program]] <- list()

  master_list[[county]][[program]][[type]] <- plot_obj

  saveRDS(master_list, master_file)
  message("✅ Updated master RDS: all_dashboard_plots.rds")
}






# Helper Function #30: Plot Loops ----
# Files: 06a-06C_pie_demo for the counties, race is a bar chart

  county_plot_loops <- function(data, county_name = "", title_text = "", county_alt_text = "", type = "bar") {

    # --- 1. SETUP & BRANDING ---
    brand       <- get_brand()
    base_colors <- unname(brand$colors)
    title_font  <- brand$fonts$chart_title
    body_font   <- brand$fonts$body
    foot_font   <- brand$fonts$footnote

    # Contrast helper for WCAG (luminance)
    get_contrast <- function(hex) {
      rgb <- col2rgb(hex)
      lum <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
      if (lum > 0.5) "#333333" else "#FFFFFF"
    }

    if (!is.data.frame(data) || nrow(data) == 0) {
      return(list(plot = plotly_empty() %>% layout(title = "No data available"),
                  sr_text = "No data available", plot_height = 500))
    }

    # --- 2. AUTOMATIC TYPE SELECTION ---
    if (grepl("race", type)) {
      type <- "race_bar"
    }

    # --- 3. PLOT LOGIC ---

    # BRANCH A: DEMOGRAPHIC BAR CHART (Best for Race)
    if (grepl("_bar", type)) {
      demo_col <- gsub("_bar", "", type)
      bar_data <- data %>%
        count(!!sym(demo_col)) %>%
        rename(cat = 1, n = 2) %>%
        mutate(pct = round(n/sum(n)*100, 1)) %>%
        arrange(desc(n))

      final_plot <- plot_ly(bar_data, x = ~pct, y = ~reorder(cat, pct), type = "bar", orientation = 'h',
                            marker = list(color = base_colors[1:nrow(bar_data)], line = list(color = "white", width = 1)),
                            text = ~paste0("<b>", pct, "%</b>"),
                            textposition = 'outside',
                            hovertext = ~paste0("<b>", cat, "</b><br>", pct, "%"),
                            hoverinfo = "text",
                            hoverlabel = list(font = list(family = body_font))) %>%
        layout(xaxis = list(title = "<b>Percentage (%)</b>", automargin = TRUE),
               yaxis = list(title = "", automargin = TRUE),
               margin = list(l=150, r=100, b=100, t=100))

      # BRANCH B: ENHANCED PIE CHART (Best for Gender/Ethnicity)
    } else if (grepl("pie", type)) {
      demo_col <- gsub("_pie", "", type)
      pie_data <- data %>% count(!!sym(demo_col)) %>% rename(cat = 1, n = 2) %>%
        mutate(pct = round(n/sum(n)*100, 1),
               color = base_colors[1:n()],
               txt_color = sapply(color, get_contrast),
               pos = ifelse(pct >= 15, "inside", "outside"))

      final_plot <- plot_ly(pie_data, labels = ~cat, values = ~n, type = 'pie',
                            text = ~paste0("<b>", cat, "</b><br>", pct, "%"),
                            textposition = ~pos,
                            textinfo = 'text',
                            hovertext = ~paste0("<b>", cat, "</b><br>", pct, "%"),
                            hoverinfo = "text",
                            pull = rep(0.02, nrow(pie_data)),
                            insidetextfont = list(family = body_font, color = ~txt_color),
                            outsidetextfont = list(family = body_font, color = '#333333'),
                            marker = list(colors = ~color, line = list(color = '#FFFFFF', width = 2)),
                            hoverlabel = list(font = list(family = body_font))) %>%
        layout(xaxis = list(title = "<b>Percentage (%)</b>", automargin = TRUE),
               yaxis = list(title = "", automargin = TRUE),
          showlegend = FALSE,
               margin = list(l=80, r=80, b=100, t=100))

      # BRANCH C: OVERALL DISEASE BAR CHART
    } else {
      final_plot <- plot_ly(data, x = ~case_rate_per_100k, y = ~reorder(disease, case_rate_per_100k),
                            type = "bar", orientation = 'h',
                            marker = list(color = base_colors[1], line = list(color = "white", width = 1)),
                            texttemplate = '<b>%{x:.2f}</b>',
                            textposition = 'outside',
                            hovertext = ~paste0("<b>", disease, "</b><br>Rate: ", sprintf("%.2f", case_rate_per_100k)),
                            hoverinfo = "text",
                            hoverlabel = list(font = list(family = body_font))) %>%
        layout(xaxis = list(title = "<b>Percentage (%)</b>", automargin = TRUE),
               yaxis = list(title = "", automargin = TRUE),
               margin = list(l=200, r=100, b=100, t=100))
    }

    # --- 4. SHARED LAYOUT & OUTPUT ---
    final_plot <- final_plot %>%
      layout(
        title = list(text = paste0("<b>", title_text, "</b>"), font = list(family = title_font), x = 0.5),
        annotations = list(
          list(text = "<b>Data Source:</b> SENDSS Georgia DPH, 2024",
               xref = "paper", yref = "paper", x = 0, y = -0.15,
               showarrow = FALSE, font = list(family = foot_font, size = 10))
        )
      )

    return(list(
      plot = final_plot %>% config(responsive = TRUE),
      sr_text = county_alt_text,
      plot_height = if(grepl("bar", type)) 650 else 550
    ))
  }





# Helper Function #31: County Parent ----
# wrapper function for an automated loop.
# filters, calculates rates, and generates plots
process5_data_and_plot <- function(data_input, county_name, program_name) {

  # 1. ADD THE REAL-TIME MESSAGE HERE
  # Use 'toupper' to make it stand out in the console
  message("🚀 Processing: 5 ", toupper(county_name), " - ", program_name, "...")

  # 1A. Filter the raw data and store it
  # Use the COLUMN name 'disease_program'
  county_and_pgm_filter5 <- data_input %>%
    filter(county == !!county_name,
           disease_program == !!program_name)

  # 1B. CHECK FOR EMPTY DATA FRAME on the raw filter result
  # If there are NO raw cases for this county/program combination, return NULL
  # 3. SHORT-CIRCUIT: If there are NO raw cases, exit early
  if (nrow(county_and_pgm_filter5) == 0) {
    message("ℹ️  Skipping: No data found for ", county_name, " ", program_name)
    # Return a structured list with a NULL plot so the dashboard knows to skip it
    return(list(county = county_name, program = program_name,
                plot_object = list(plot = NULL, sr_text = "No cases reported for this period.")))
  }

  # --- Data is present, proceed to rates and plotting ---

  # 1. Filter and Calculate Rates
  county_and_pgm_rates5 <- county_and_pgm_filter5 %>%
    county_rates(county_name = county_name) # Assuming county_rates expects county_name


  # 2. Generate Alt Text
  county_alt_text5 <- county_alt_text_string(county_and_pgm_rates, program_name)

  # 3. Generate Plot Object
  GNRplot5_sr_list <- generate_county_plots(
    data = county_and_pgm_rates,
    title_text = paste("Case Rates Over 5 years for ", program_name, " in ", county_name, " County"),
    county_alt_text = county_alt_text,
    county_name = county_name,
    disease_program = program_name
  )

  # 4. Return a consistently structured list
  return(list(
    county = county_name,
    program = program_name,
    plot_object = GNRplot5_sr_list # This contains $plot and $sr_text
  ))
}





# Helper Function #32: Percent Chart ----
# creates a bar chart using percentages
percent_chart <- function(data, title_text, county_alt_text, disease_program_filter = NULL, id_col = "disease") {

  brand <- get_brand()

  brand_font <- getOption("brand_font")

  # --- 2. ID Column Detection ---
  # If the user didn't provide an id_col, try to find one automatically
  if (is.null(id_col)) {
    id_col <- intersect(names(data), c("pseudo_patient_id", "patient_id", "case_id", "id", "disease"))[1]
  }

  if (is.na(id_col)) {
    warning("❌ Error: No ID column found. Check names: ", paste(names(data), collapse=", "))
    return(list(plot = NULL, sr_text = "Data structure error"))
  }

  # --- 3. Filter by Program (If requested) ---
  data_to_use <- data
  if (!is.null(disease_program_filter)) {
    data_to_use <- data %>%
      filter(disease_program == !!disease_program_filter)
  }

  if (nrow(data_to_use) == 0) {
    return(list(plot = NULL, sr_text = paste("No cases found for", disease_program_filter)))
  }

  # 3. DATA PREP: Calculate case counts and percentages
  # We do this here so we can use these numbers for the Alt Text
  counting <- data %>%
    filter(!is.na(.data[[id_col]])) %>%
    group_by(disease = .data[[id_col]]) %>%
    summarise(case_count = n(), .groups = "drop") %>%
    mutate(percent = round((case_count / sum(case_count)) * 100, 1))%>%
    arrange(percent) # Ascending order works best for Plotly horizontal bars


  # --- 2. Styling ---
  n_rows <- nrow(data)

  base_colors <- unname(brand$colors)
  # base_patterns <- brand$patterns
  # plot_patterns <- rep(base_patterns, each = length(base_colors), length.out = nrow(data))

  title_font    <- brand$fonts$chart_title
  axis_font     <- brand$fonts$axis
  footnote_font <- brand$fonts$footnote

  data <- data %>%
    filter(!is.na(disease))

  # --- 3. Plot Creation ---
    county_perct <- plot_ly(
    data = counting,
    x = ~percent,
    y = ~reorder(disease, percent),
    type = "bar",
    orientation = 'h',
    marker = list(
      color = unname(brand$colors[1]), size = 10),
      # line = list(color = unname(brand$colors[1]), width = 3
    # ),
    text = ~paste0(percent, "%"),
    textposition = 'outside',
    cliponaxis = FALSE,

     # --- ADD THIS FOR CUSTOM HOVER TEXT ---
    # FIX: Customizing the Hover Label Background and Style
      hoverlabel = list(
        bgcolor = "#333333",       # Dark background for the tooltip
        bordercolor = "#008080",   # Border matching the bar color
        font = list(
          family = brand$fonts$axis,
          size = 12,
          color = "white"          # White text for contrast
        )),
    hoverinfo = "text",
    hovertext = ~paste0(disease, ": " , percent, "%")
  ) %>%
    layout(
      template = brand$template,
      title = list(
        text = paste0("<b>", title_text, "</b>"),
        font = list(family = brand$fonts$chart_title, color = "black"),
        x = 0.5, y = 0.96, # Position (0 to 1, x=0.5 is center, y=0.95 is near top)
        xanchor = "center", yanchor = "top", # Align the top of the title with y=0.98
        pad = list(b = 50)
        ),
      xaxis = list(
        title = list(
          # text = "<b>Percentage of Disease Reported</b>", # Bold the X-axis category name (title)
                     font = list(family = brand_font, color = "#4A4A4A"),
                standoff = 35),  # Increases space between title and the disease names
                zeroline = TRUE,
        zerolinewidth = 2,
        # zerolinecolor = "#444",
        tickmode = "linear",
            showgrid = TRUE,
        automargin = TRUE,
        zeroline = TRUE,
        dtick = "10",
        gridcolor = "rgba(0,0,0,0.1)",
        # Minor grid lines (dtick determines frequency of major; minor is calculated based on ticks)
       minor = list(
       showgrid = FALSE,
       gridcolor = "lightgrey",
       gridwidth = 10),
        ticksuffix = "%",# Adds % to axis labels
         range = c(0,105)
 ),
      yaxis = list(
        title = FALSE,
        # (text = "<b>Disease</b>", Font = list(family = brand$fonts$axis, color = "black"),
        # standoff = 25),  # Increases space between title and the disease names
        # automargin = TRUE,

           # FIX: Customizing the font for the Y-axis tick labels (Disease Names)
                     tickfont = list(
                       family = brand$fonts$axis,
                       size = 14,
                       color = "#333333")
                     ),
      margin = list(l = 150, r = 50, t = 80, b = 150, pad = 10),

            # --- ADD FOOTNOTES AS ANNOTATIONS HERE ---
      annotations = list(
        # Footnote 1: Data Source
        list(
          text = "<b>Data Source:</b> SENDSS Georgia Department of Public Health, 2024",
          xref = "paper", # Position relative to the entire plot area (0 to 1)
          yref = "paper", # Position relative to the entire plot area (0 to 1)
          x = -0.45, # X position (0 = left edge of plot)
          y = -.25, # Y position (negative values place it below the plot area)
          # Adjust 'y' based on your bottom margin and font size
          showarrow = FALSE, # Do not show an arrow
          xanchor = "left", # Anchor the text to its left side
          yanchor = "top", # Let plotly determine vertical anchor (usually bottom)
          font = list(family = brand$fonts$footnote, color = "black")
        )
      )
    ) %>%
      config(responsive = TRUE)

  # Return list
  return(list(
    plot = county_perct,
    sr_text = county_alt_text,
    data_view = counting
  ))
}




# Helper Function #33: Top 3 Diseases ----
get_top_diseases <- function(data, n = 3) {
  # ---- 1. Required columns ----
  required_cols <- c("disease", "case_rate_per_100k")

  missing <- setdiff(required_cols, names(data))
  if (length(missing) > 0) {
    stop(
      "get_top_diseases(): Missing required columns: ",
      paste(missing, collapse = ", ")
    )
  }
  # ---- 2. Clean & validate ----
  data_clean <- data %>%
    filter(
      !is.na(disease),
      !is.na(case_rate_per_100k)
    )

  if (nrow(data_clean) == 0) {
    return(data_clean)
  }
  # ---- 3. Rank diseases by peak rate ----
  top_diseases <- data_clean %>%
    group_by(disease) %>%
    summarise(
      peak_rate = max(case_rate_per_100k, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(peak_rate), disease) %>%  # deterministic tie-break
    slice_head(n = n) %>%
    pull(disease)
  # ---- 4. Filter original data ----
  data_clean %>%
    filter(disease %in% top_diseases)
}



# Helper Function # : Render plots
render_dashboard_plot <- function(x) {
  if (is.null(x)) {
    cat("<p>Data not available for this section.</p>")
  } else {
    htmltools::div(
      class = "dashboard-card",
      x$plot,
      htmltools::span(class = "sr-only", x$sr_text)
    )
  }
}



# Helper: Read plot from master RDS
read_dashboard_plot <- function(county, program, type = "bar",
                                master_file = "output/all_dashboard_plots.rds") {

  # 1. Load the master RDS if it hasn't been loaded yet
  if (!exists("all_dashboard_plots", envir = .GlobalEnv)) {
    assign("all_dashboard_plots", readRDS(here::here(master_file)), envir = .GlobalEnv)
  }

  # 2. Clean inputs
  county <- county[1]
  program <- program[1]
  type <- type[1]

  # 3. Attempt to extract the plot object
  plot_bundle <- tryCatch({
    all_dashboard_plots[[county]][[program]][[type]]
  }, error = function(e) {
    warning("Plot not found for ", county, " / ", program, " / ", type)
    return(NULL)
  })

  # 4. Return
  return(plot_bundle)
}





# Helper Function #: PercentS alt text metrics for plots ----
# File: Gwinnett plots script.R

# Defining alt_text_string for plots
# Step 1: Key metrics
percent_alt_text_string <- function(data, disease_program){
  # 🌟 CRITICAL FIX: Check if the data frame is empty first
  if (nrow(data) == 0) {
    return(paste0("No case data reported for ", disease_program, " in this county."))
  }
  # 🌟 Step 1.5: CRITICAL FIX for the "-Inf" Warning
  # Check if all rates are NA. If so, stop here.
  if (all(is.na(data$percent))) {
    return(paste0("Case data reported for ", disease_program, ", but rate calculations are unavailable."))
  }
  # 1. Use a variable to store the max index safely
  max_idx <- which.max(data$percent)

  # 2. Check if we actually found a maximum
  if (length(max_idx) > 0) {
    # This only runs if there's at least one non-NA rate
    max_val          <- data$percent[max_idx]
    max_disease_percent <- round(max_val, 1)
    max_disease      <- as.character(data$disease[max_idx])
  } else {
    # Fallback if everything is NA
    max_disease_rate <- "N/A"
    max_disease      <- "unknown diseases"
  }

  # 4. Count unique diseases
  num_diseases <- length(unique(data$disease))

  # Step 2: Handle pluralization
  illness_word <- ifelse(num_diseases == 1, "Illness", "Illnesses")

  # Step 3: Optional: list all disease names (comma separated)
  disease_list <- paste(sort(unique(data$disease)), collapse = ", ")

  # Step 4: Construct dynamic alt text
  percent_alt_text <- paste0(
    "Percentage breakdown for ", num_diseases, " ", disease_program, " ", illness_word, ". ",
    "The most frequently reported disease is ", max_disease, ", accounting for ",
    max_disease_percent, "% of cases within this program. ",
    "Diseases included in this chart: ", disease_list, "."
  )

  # Example output:
  # "Case rates for 6 Vectorborne Illnesses. The maximum rate is 48.3 per 100,000 population, associated with West Nile Virus. Diseases included: West Nile Virus, Dengue, Zika, Chikungunya, Yellow Fever, Malaria."
  return(percent_alt_text)
}


# Helper Function #32: Age Pyramid by Gender and Age Group ----
generate_age_pyramid <- function(data, title_text = "Age and Gender Distribution", disease_program = "Disease Program") {
  
  # Prepare data: count by age group and gender
  pyramid_data <- data %>%
    filter(gender %in% c("MALE", "FEMALE")) %>%  # Exclude UNKNOWN for cleaner pyramid
    count(age_group, gender, name = "count") %>%
    mutate(
      # Make counts negative for males (left side of pyramid)
      count = case_when(
        gender == "MALE" ~ -count,
        TRUE ~ count
      ),
      gender = factor(gender, levels = c("MALE", "FEMALE"), labels = c("Male", "Female"))
    ) %>%
    arrange(age_group)
  
  # Create the age pyramid using ggplot2
  age_pyramid_plot <- ggplot(pyramid_data, aes(x = count, y = age_group, fill = gender)) +
    geom_col(position = "identity", width = 0.7) +
    # Set custom colors using GNR brand palette
    scale_fill_manual(
      values = c("Male" = "#08786B", "Female" = "#f27866"),  # GNR Teal and Coral
      name = "Gender"
    ) +
    # Format x-axis to show absolute values
    scale_x_continuous(
      labels = function(x) abs(x),
      expand = expansion(mult = 0.05)
    ) +
    labs(
      title = title_text,
      subtitle = disease_program,
      x = "Number of Cases",
      y = "Age Group"
    ) +
    theme_minimal(base_size = 12, base_family = "Roboto") +
    theme(
      plot.title = element_text(
        face = "bold", 
        size = 16, 
        family = "Brandon Grotesque Black",
        color = "#000000",
        margin = margin(b = 5)
      ),
      plot.subtitle = element_text(
        size = 12, 
        color = "#666666",
        margin = margin(b = 10)
      ),
      axis.title = element_text(face = "bold", size = 11, color = "#000000"),
      axis.text = element_text(size = 10, color = "#2B2B2B"),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      panel.grid.major.x = element_line(color = "#e1e8ed", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA)
    )
  
  # Convert to plotly for consistency with dashboard
  pyramid_plotly <- ggplotly(
    age_pyramid_plot, 
    tooltip = c("x", "y", "fill"),
    layout = list(hovermode = "closest")
  ) %>%
    layout(
      template = plot_template,
      margin = list(l = 70, r = 70, t = 70, b = 70),
      font = list(family = "Roboto, Arial, Sans-Serif", color = "#2B2B2B"),
      title = list(
        font = list(family = "Brandon Grotesque Black, Arial, Sans-Serif", size = 18, color = "#000000")
      ),
      xaxis = list(
        title = list(font = list(family = "Roboto Medium, Arial, Sans-Serif", size = 13)),
        tickfont = list(family = "Roboto, Arial, Sans-Serif", size = 11)
      ),
      yaxis = list(
        title = list(font = list(family = "Roboto Medium, Arial, Sans-Serif", size = 13)),
        tickfont = list(family = "Roboto, Arial, Sans-Serif", size = 11)
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = 1.15,
        xanchor = "center",
        yanchor = "top",
        font = list(family = "Roboto, Arial, Sans-Serif", size = 12)
      )
    ) %>%
    config(responsive = TRUE, displayModeBar = FALSE)
  
  # Generate alt text for accessibility
  total_cases <- sum(abs(pyramid_data$count))
  male_cases <- sum(pyramid_data$count[pyramid_data$gender == "Male"])
  female_cases <- sum(pyramid_data$count[pyramid_data$gender == "Female"])
  
  # Find peak age groups
  male_peak <- pyramid_data$age_group[pyramid_data$gender == "Male" & pyramid_data$count == min(pyramid_data$count[pyramid_data$gender == "Male"])][1]
  female_peak <- pyramid_data$age_group[pyramid_data$gender == "Female" & pyramid_data$count == max(pyramid_data$count[pyramid_data$gender == "Female"])][1]
  
  alt_text <- sprintf(
    "Age pyramid showing the distribution of %d cases by age group and gender. Males comprise %d cases (%.1f%%) with highest concentration in the %s age group. Females comprise %d cases (%.1f%%) with highest concentration in the %s age group.",
    total_cases,
    abs(male_cases),
    (abs(male_cases) / total_cases) * 100,
    male_peak,
    female_cases,
    (female_cases / total_cases) * 100,
    female_peak
  )
  
  return(list(
    plot = pyramid_plotly,
    data = pyramid_data,
    sr_text = alt_text
  ))
}


# Helper Function #33: Horizontal Bar Chart for Disease Distribution by Demographics ----
generate_horizontal_bar_chart <- function(
    data,
    group_col_name,
    disease_program,
    county_name,
    title = NULL) {
  
  brand <- get_brand()
  
  # Define local variables
  base_color <- brand$colors["GNRgreen"]  # Use GNR green as primary color
  body_font <- brand$fonts$body
  axis_font <- brand$fonts$axis
  title_font <- brand$fonts$chart_title
  
  # CRITICAL CHECK: Ensure the grouping column exists
  if (!(group_col_name %in% names(data))) {
    stop(paste0("Error: The column '", group_col_name, "' does not exist."))
  }
  
  # --- 1. DATA PREPARATION ---
  group_sym <- sym(group_col_name)
  
  data_summary <- data %>%
    group_by(!!group_sym) %>%
    summarise(n_cases = n(), .groups = "drop") %>%
    mutate(
      per_total = round(n_cases / sum(n_cases) * 100, 1),
      demo = str_to_title(as.character(!!group_sym)),
      label = paste0(demo, " (", per_total, "%)")
    ) %>%
    filter(per_total > 0) %>%
    # ASCENDING ORDER: sort from lowest to highest
    arrange(per_total)
  
  # --- 2. ALT TEXT GENERATION ---
  if (nrow(data_summary) == 0) {
    alt_text <- paste0("No case data reported for ", disease_program, " in ", county_name, ".")
    return(list(plot = plotly_empty(), sr_text = alt_text, data = NULL))
  }
  
  # Generate descriptive alt text
  max_group <- data_summary$demo[nrow(data_summary)]  # Last row is highest (ascending order)
  max_val <- data_summary$per_total[nrow(data_summary)]
  min_group <- data_summary$demo[1]  # First row is lowest
  min_val <- data_summary$per_total[1]
  
  alt_text <- sprintf(
    "Horizontal bar chart showing %s cases by %s in ascending order. %s has the highest percentage at %.1f%%, while %s has the lowest at %.1f%%. Total categories: %d.",
    str_to_title(disease_program),
    str_to_lower(group_col_name),
    max_group,
    max_val,
    min_group,
    min_val,
    nrow(data_summary)
  )
  
  # --- 3. TITLE LOGIC ---
  if (is.null(title) || title == "") {
    title_text <- paste0(
      "Distribution of ", str_to_title(disease_program), " Illnesses by ", 
      str_to_title(group_col_name)
    )
  } else {
    title_text <- title
  }
  
  # --- 4. CREATE GGPLOT HORIZONTAL BAR CHART ---
  bar_plot <- ggplot(data_summary, aes(x = per_total, y = reorder(demo, per_total), fill = demo)) +
    geom_col(width = 0.7, fill = base_color) +
    geom_text(
      aes(label = paste0(per_total, "%")),
      hjust = -0.1,
      family = "Roboto",
      size = 4,
      color = "#000000"
    ) +
    scale_x_continuous(
      limits = c(0, max(data_summary$per_total) * 1.15),
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = title_text,
      x = "Percentage of Cases",
      y = str_to_title(group_col_name)
    ) +
    theme_minimal(base_size = 12, base_family = "Roboto") +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 16,
        family = "Brandon Grotesque Black",
        color = "#000000",
        margin = margin(b = 10)
      ),
      axis.title = element_text(
        face = "bold",
        size = 11,
        color = "#000000",
        family = "Roboto Medium"
      ),
      axis.text = element_text(
        size = 10,
        color = "#2B2B2B",
        family = "Roboto"
      ),
      axis.title.y = element_text(margin = margin(r = 10)),
      axis.title.x = element_text(margin = margin(t = 10)),
      panel.grid.major.x = element_line(color = "#e1e8ed", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      legend.position = "none"
    )
  
  # --- 5. CONVERT TO PLOTLY FOR DASHBOARD ---
  bar_plotly <- ggplotly(
    bar_plot,
    tooltip = c("x", "y"),
    layout = list(hovermode = "closest")
  ) %>%
    layout(
      template = plot_template,
      margin = list(l = 70, r = 70, t = 70, b = 70),
      font = list(family = "Roboto, Arial, Sans-Serif", color = "#2B2B2B"),
      title = list(
        font = list(family = "Brandon Grotesque Black, Arial, Sans-Serif", size = 18, color = "#000000")
      ),
      xaxis = list(
        title = list(font = list(family = "Roboto Medium, Arial, Sans-Serif", size = 13)),
        tickfont = list(family = "Roboto, Arial, Sans-Serif", size = 11)
      ),
      yaxis = list(
        title = list(font = list(family = "Roboto Medium, Arial, Sans-Serif", size = 13)),
        tickfont = list(family = "Roboto, Arial, Sans-Serif", size = 11)
      )
    ) %>%
    config(responsive = TRUE, displayModeBar = FALSE)
  
  return(list(
    plot = bar_plotly,
    data = data_summary,
    sr_text = alt_text
  ))
}
