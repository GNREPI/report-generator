


specified demographic column (gender, race, ethnicity, etc.).

generate_pie_chart <- function(
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

  # # colors <- unname(brand$colors)
  body_font <- brand$fonts$body
  footnote_font <- brand$fonts$footnote
  hover_font <- brand$fonts$hover
  title_font <- brand$fonts$chart_title


  # CRITICAL CHECK 1: Ensure the grouping column exists
  if (!(group_col_name %in% names(data))) {
    stop(paste0("Error: The column '", group_col_name, "' does not exist."))
  }

  # --- 1. DATA PREPARATION (Using glue/sym for dynamic column selection) ---
  group_sym <- sym(group_col_name) # Convert string to symbol for tidy evaluation

  data_summary <- data %>%
    group_by(!!group_sym) %>% # Group by the dynamic column (e.g., 'gender')
    summarise(
      n_cases = n(),
      .groups = "drop") %>%
    # ungroup() %>%
    # as_tibble() %>%
    mutate(
      per_total = round(n_cases/sum(n_cases)*100, 1),
      demo = !!group_sym, # Create the standard 'demo' column for Alt Text
      label = paste0("<b>", demo, ": ", "<br>", per_total, "%</b>")) %>%
    # Filter out any rows where the percentage is zero
    filter(per_total > 0) %>%
    arrange(desc(per_total))


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
              textposition = 'auto',
              insidetextfont = list(family = body_font),
              outsidetextfont = list(family = body_font),
              insidetextorientation = 'radial',
              # 3. HOVER LABEL FONT
              hoverlabel = list(font = list(family = hover_font)),
              name = 'label',
              marker = list(colors = base_colors[1:nrow(data_summary)])
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
      showlegend = TRUE,
      margin = list(l=50, r=250, b=75, t=75, pad=3),

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





Helper Function #10: District Plot ----

plot_district_rates <- function(data, title_text, district_alt_text){

  brand <- get_brand()

  n_rows <- nrow(data)
  base_colors <- unname(brand$colors)
  plot_colors <- rep(base_colors, length.out = n_rows)

  # n_diseases <- nrow(data)

  # --- Step 0: Ensure diseases have a color ---
  # Strip the names so Plotly just cycles through the hex codes in order

  # # colors <- unname(brand$colors)
  title_font <- brand$brandon$chart_title
  axis_font <- brand$template$axis
  footnote_font <- brand$fonts$footnote
  hover_font <- brand$fonts$hover


  # Ensure data is sorted by rate so the colors apply consistently to top values
  data <- data %>% arrange(desc(cumulative_rate))
  #
  #   # --- Step 0: Ensure diseases have a color ---
  #   # Match the disease names to the color vector
  #   disease_levels <- unique(data$disease)
  #
  #   # Add default color if any disease is missing
  #   missing_colors <- setdiff(disease_levels, names(colors))
  #   if(length(missing_colors) > 0){
  #     message("⚠️ Some diseases missing colors: assigning default grey")
  #     colors <- c(colors, setNames(rep("#CCCCCC", length(missing_colors)), missing_colors))
  #   }
  #
  #   # Ensure factor levels match the color names
  #   data$disease <- factor(data$disease, levels = names(colors))

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


  # # Adjust font size: more digits → smaller font
  # label_font_size <- 14
  # # max(14, base_font_size - (max_digits -1) * 1.5)
  # --- Step 3: Create Plotly Horizontal Bar Chart ---
  district_plot <- plot_ly(
    data = data,
    x = ~cumulative_rate,
    y = ~reorder(disease, cumulative_rate),
    type = "bar",
    orientation = 'h',
    marker = list(
      color = plot_colors,
      line = list(color = "white", width = 1) # Outline helps distinguish patterns
    ),
    # 3. LABEL NAMES INSIDE THE END OF THE BAR
    text = ~disease,
    # textposition = 'inside',
    # insidetextanchor = 'end',
    # textfont = list(color = ~t_color, family = hover_font),
    #
    # hovertext = ~paste(
    #   "<b>Disease</b>", disease, "<br>",
    #   "<b>Case Rate</b>", rate_label
    # ),
    hoverinfo = "text" # Tells plotly to use the 'text' aesthetic for hover
  ) %>%
    # color = ~disease,
    # colors = colors,
    # marker = list(opacity = 0.8),
    # texttemplate = ~paste0('<b style="color:black; font-size:', label_font_size, 'px;">',
    #                        sprintf("%.2f", cumulative_rate), '</b>'),
    # textposition = 'outside',


    # hovertext = ~paste(
    #   "<b>Disease:</b>", disease, "<br>",
    #   "<b>Case Rate (per 100k):</b>", sprintf("%.2f", cumulative_rate), "<br>"
    # ),

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
        pad = list(b = 10)), # Padding *below* the main chart title text itself
      xaxis = list(
        title = list(
          text = "Case Rate per 100,000", # Bold the X-axis category name (title)
          font = list(family = axis_font),
          standoff = 10, # Add 20px space between X-axis labels and X-axis title
          automargin = TRUE
        ),
        showticklabels = TRUE,
        tickangle = 0, # <--- 1. Rotate X-axis numbers (0 for horizontal numbers)
        tickfont = list(size = 10, family = axis_font),
        zeroline = TRUE, # Hide the zero line
        showgrid = TRUE,  # Show grid lines
        gridcolor = "lightgrey",
        autosize = TRUE
        # minor = list(
        #   showgrid = TRUE,
        #   gridcolor = "lightgrey", # Or a slightly lighter shade if desired, e.g., "lightgray"
        #   dtick = 25)                 # Defines the step between minor ticks.
        #   # If your major ticks are every 100, dtick=50 places minor ticks in between.
        #   # Adjust 'dtick' value based on your 'range' and desired density.
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
        # For horizontal bars with categorical Y-axis, plotly's default
        # behavior is to center labels on the bars. automargin handles overflow.
      ),
      showlegend = FALSE,
      margin = list(t=80, r=80, b=bottom_margin, l = 20),
      # --- Bar chart specific layout (if type="bar") ---
      bargap = 0.5,# Gap between bars (0 to 1)

      # --- ADD FOOTNOTES AS ANNOTATIONS HERE ---
      annotations = list(
        # Footnote 1: Data Source
        list(
          text = "<b>Data Source:</b> SENDSS Georgia Department of Public Health, 2024",
          xref = "paper", # Position relative to the entire plot area (0 to 1)
          yref = "paper", # Position relative to the entire plot area (0 to 1)
          x = 0,         # X position (0 = left edge of plot)
          y = -.60,     # Y position (negative values place it below the plot area)
          # Adjust 'y' based on your bottom margin and font size
          showarrow = FALSE, # Do not show an arrow
          xanchor = "left",  # Anchor the text to its left side
          yanchor = "bottom",  # Let plotly determine vertical anchor (usually bottom)
          font = list(family = footnote_font)))
      # autosize = TRUE
    ) %>%
    config(responsive=TRUE)

  return(
    # Return both plot and alt text
    list(plot = district_plot, sr_text = district_alt_text))
}



# # Helper Function ----
# generate_alt_text <- function(data, county, program) {
#   # Calculate simple stats for the narrative
#   total_cases <- sum(data$n_cases, na.rm = TRUE)
#   max_row <- data[which.max(data$n_cases), ]
#
#   text <- paste0(
#     "Surveillance chart for ", program, " in ", county, " County. ",
#     "There are a total of ", total_cases, " reported cases. ",
#     "The highest frequency occurred in ", max_row$Year, " with ", max_row$n_cases, " cases. ",
#     "The data shows a ", if(data$n_cases[nrow(data)] > data$n_cases[1]) "rising" else "declining", " trend."
#   )
#   return(text)
# }
#



# # Helper Function #17b: Gender Pie Chart ----
#
#   # # Gender Plot
# gender_pie <- function(data, alt_text, title_text, accessible_colors){
#     gender_plot <- plot_ly(data = data,
#                          labels = ~label,
#                          values = ~per_total,
#                          type = 'pie',
#                          textposition = 'auto',
#                          textinfo = 'label',
#                          insidetextfont = list(color = "black", size = 12),
#                          marker = list(colors = accessible_colors[1:nrow(gender_summary)]),
#                          hoverinfo = 'text')%>%
#     layout(title = paste0("<b>", title_text, "</b>"),
#            margin = list(l=50, r=250, b=50, t=75, pad=3),
#            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#
#   # FIX: Return a list containing both the plot and the alt text (screen reader text)
#   return(list(plot = gender_plot, sr_text = alt_text))
#   }
#
#
#
# # Helper Function #18: Gender Alt Text ----
#
# # Gender alt text
# gender_alt_text <- function(data, disease_program){
#   # 🌟 CRITICAL FIX: Check if the data frame is empty first
#   if (nrow(data) == 0) {
#     return(paste0("No case data reported for ", disease_program, " in this county."))
#   }
#   # the group with the highest number of cases
#   max_gender <- max(data$per_total, na.rm = TRUE)
#   max_gender_group <- data$demo[which.max(data$per_total)]
#
#   # number of unique groups included
#   num_gender_grps <- length(unique(data$demo))
#
#   # --- Step 2: List all age groups (comma separated) ---
#   gender_list <- paste(sort(unique(data$demo)), collapse = ", ")
#
#   # --- Step 3: Construct dynamic alt text ---
#   gender_alt_text <- paste0(
#     "This visualization displays case counts across ", num_gender_grps,
#     " distinct groups. ",
#     "The highest case volume is ", max_gender, " cases, which is associated with the ",
#     max_gender_group, " age band. ",
#     "The age groups represented are: ", gender_list, ".")
#   return(gender_alt_text)
# }
#
#
#
# # Helper Function #19: Race Pie Chart ----
# # Race
# # % of race
# race_per <- function(data){
#   data_summary <- data %>%
#     group_by(race) %>%
#     summarise(
#       no_race = n()
#     ) %>%
#     ungroup() %>%
#     mutate(
#       per_total = round(no_race/sum(no_race)*100, 1),
#       demo = race,
#       label = paste0("<b>", race, ": ", per_total, "%</b>")) %>%
#     # Filter out any rows where the percentage is zero
#     filter(per_total > 0)
# return(race_per)
# }
#
#
# # Helper Function #19b: Race Pie Chart ----
#
#   # Race Plot
# race_pie <- function(data){
#    race_plot <- plot_ly(data = data_summary,
#                          labels = ~label,
#                          values = ~per_total,
#                          type = 'pie',
#                          textposition = 'auto',
#                          textinfo = 'label',
#                          insidetextfont = list(color = "black", size = 12),
#                          hoverinfo = 'text')%>%
#     layout(title = paste0("<b>", "Percentage of Cases by Race", "</b>"),
#            margin = list(l=200, r=200, b=50, t=75, pad=3),
#            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#
#   return(race_plot)
# }
#
#
#
# # Helper Function #20: Race Alt Text ----
#
# # Race alt text
# race_alt_text <- function(data){
#   # 🌟 CRITICAL FIX: Check if the data frame is empty first
#   if (nrow(data) == 0) {
#     return(paste0("No case data reported for ", disease_program, " in this county."))
#   }
#   # the group with the highest number of cases
#   max_race <- max(data$per_total, na.rm = TRUE)
#   max_race_group <- data$demo[which.max(data$per_total)]
#
#   # number of unique groups included
#   num_race_grps <- length(unique(data$demo))
#
#   # --- Step 2: List all age groups (comma separated) ---
#   race_list <- paste(sort(unique(data$demo)), collapse = ", ")
#
#   # --- Step 3: Construct dynamic alt text ---
#   race_alt_text <- paste0(
#     "This visualization displays case counts across ", num_race_grps,
#     " distinct groups. ",
#     "The highest case volume is ", max_race, " cases, which is associated with the ",
#     max_race_group, " age band. ",
#     "The age groups represented are: ", race_list, ".")
#   return(race_alt_text)
# }
#
#
#
# # Helper Function #21: Ethnicity Pie Chart ----
# # Ethnicity
# # % of ethnicity
# ethnicity_per <- function(data){
#   data_summary <- data %>%
#     group_by(ethnicity) %>%
#     summarise(
#       no_ethnicity = n()
#     ) %>%
#     ungroup() %>%
#     mutate(
#       per_total = round(no_ethnicity/sum(no_ethnicity)*100, 1),
#       label = paste0("<b>", ethnicity, ": ", per_total, "%</b>")) %>%
#
#   # Filter out any rows where the percentage is zero
#   filter(per_total > 0)
#
#   # Ethnicity Plot
#   ethnicity_plot <- plot_ly(data = data_summary,
#                          labels = ~label,
#                          values = ~per_total,
#                          type = 'pie',
#                          textposition = 'auto',
#                          textinfo = 'label',
#                          insidetextfont = list(color = "black", size = 12),
#                          hoverinfo = 'text')%>%
#     layout(title = paste0("<b>", "Percentage of Cases by Ethnicity", "</b>"),
#            margin = list(l=200, r=200, b=50, t=75, pad=3),
#            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#
#   return(ethnicity_plot)
# }
#
#
#
# # Helper Function #22: Ethnicity Alt Text ----
#
# # Ethnicity alt text
# ethnicity_alt_text <- function(data){
#   # 🌟 CRITICAL FIX: Check if the data frame is empty first
#   if (nrow(data) == 0) {
#     return(paste0("No case data reported for ", disease_program, " in this county."))
#   }
#   # the group with the highest number of cases
#   max_ethnicity <- max(data$per_total, na.rm = TRUE)
#   max_ethnicity_group <- data$demo[which.max(data$per_total)]
#
#   # number of unique groups included
#   num_ethnicity_grps <- length(unique(data$demo))
#
#   # --- Step 2: List all age groups (comma separated) ---
#   ethnicity_list <- paste(sort(unique(data$demo)), collapse = ", ")
#
#   # --- Step 3: Construct dynamic alt text ---
#   ethnicity_alt_text <- paste0(
#     "This visualization displays case counts across ", num_ethnicity_grps,
#     " distinct groups. ",
#     "The highest case volume is ", max_ethnicity, " cases, which is associated with the ",
#     max_ethnicity_group, " age band. ",
#     "The age groups represented are: ", ethnicity_list, ".")
#   return(ethnicity_alt_text)
# }
#
#
#
#
# # Helper Function #23: Case Definition Pie Chart ----
# # Case Definition
# # % of case_def
# case_def_per <- function(data){
#   data_summary <- data %>%
#     group_by(adminstatus) %>%
#     summarise(
#       no_case_def = n()
#     ) %>%
#     ungroup() %>%
#     mutate(
#       per_total = round(no_case_def/sum(no_case_def)*100, 1),
#       label = paste0("<b>", adminstatus, ": ", per_total, "%</b>"))
#
#   # Case Definition Plot
#   case_def_plot <- plot_ly(data = data_summary,
#                          labels = ~label,
#                          values = ~per_total,
#                          type = 'pie',
#                          textposition = 'auto',
#                          textinfo = 'label',
#                          insidetextfont = list(color = "black", size = 12),
#                          hoverinfo = 'text')%>%
#     layout(title = paste0("<b>", "Percentage of Cases by Case Definition", "</b>"),
#            margin = list(l=50, r=250, b=50, t=75, pad=3),
#            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
#            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#
#   return(case_def_plot)
# }
#
#
#
# # Helper Function #24: Case Definition Alt Text ----
#
# # Case Definition alt text
# case_def_alt_text <- function(data){
#   # 🌟 CRITICAL FIX: Check if the data frame is empty first
#   if (nrow(data) == 0) {
#     return(paste0("No case data reported for ", disease_program, " in this county."))
#   }
#   # the group with the highest number of cases
#   max_case_def <- max(data$per_total, na.rm = TRUE)
#   max_case_def_group <- data$demo[which.max(data$per_total)]
#
#   # number of unique groups included
#   num_case_def_grps <- length(unique(data$demo))
#
#   # --- Step 2: List all age groups (comma separated) ---
#   case_def_list <- paste(sort(unique(data$demo)), collapse = ", ")
#
#   # --- Step 3: Construct dynamic alt text ---
#   case_def_alt_text <- paste0(
#     "This visualization displays case counts across ", num_case_def_grps,
#     " distinct groups. ",
#     "The highest case volume is ", max_case_def, " cases, which is associated with the ",
#     max_case_def_group, " age band. ",
#     "The age groups represented are: ", case_def_list, ".")
#   return(case_def_alt_text)
# }
#
#

