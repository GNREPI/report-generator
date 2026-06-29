library(shiny)
library(plotly)
library(htmltools)

# --- 1. Define the R/Shiny UI with ARIA Attributes ---
ui <- fluidPage(
  titlePanel("Infectious Disease Case Rates (Accessible Demo)"),

  # 1A. Define the necessary CSS for sr-only (Screen Reader Only)
  tags$head(
    tags$style(
      HTML('
                .sr-only {
                    position: absolute;
                    width: 1px;
                    height: 1px;
                    padding: 0;
                    margin: -1px;
                    overflow: hidden;
                    clip: rect(0, 0, 0, 0);
                    white-space: nowrap;
                    border-width: 0;
                }
                /* Optional: Add a clear visual focus indicator for keyboard users */
                .chart-container:focus {
                    outline: 4px solid #3b82f6;
                    outline-offset: 4px;
                }
            ')
    )
  ),

  # 1B. The Invisible, Detailed Description (The Alt Text)
  # This must be placed immediately before the element it describes in the DOM order.
  tags$span(
    id = "chart-description",
    class = "sr-only",
    # Fix: I corrected the minor typo found in the previous step (19 illnesses).
    "District-wide case rates for 19 illnesses. The maximum rate is 0.1 per 100,000 population, associated with Hepatitis c. Diseases included: Haemophilus Influenzae Invasive, Hepatitis a Acute, Hepatitis b, Hepatitis b Acute, Hepatitis b Chronic, Hepatitis b Probable Chronic, Hepatitis c, Hepatitis c Acute, Hepatitis c Chronic, Hepatitis c Probable Acute, Hepatitis c Probable Chronic, Measles Rubeola, Mumps, Perinatal Hepatitis b, Perinatal Hepatitis c, Pertussis, Rsv Fatal Cases, Rubella Including Congenital, Varicella Chicken Pox."
  ),

  # 1C. The Plot Container with ARIA Links
  # We wrap the plotlyOutput in a custom div to add the focus/ARIA attributes.
  tags$div(
    class = "chart-container", # Use the class for the custom focus style
    # CRITICAL: Make the chart focusable
    tabindex = "0",
    # CRITICAL: Define the role and the short accessible name
    role = "img",
    'aria-label' = "Bar chart showing case rates for 19 infectious diseases by district.",
    # CRITICAL: Link the short label to the long, detailed description
    'aria-describedby' = "chart-description",

    # Insert the actual Plotly output inside this accessible container
    plotlyOutput("case_rates_plot")
  )
)

# --- 2. Define the R/Shiny Server Logic ---
server <- function(input, output, session) {
  output$case_rates_plot <- renderPlotly({
    # Generate a simple Plotly chart for demonstration
    data <- data.frame(
      Disease = c("Hepatitis c", "Pertussis", "Measles", "Mumps", "Hepatitis b Acute"),
      Rate = c(0.1, 0.05, 0.001, 0.02, 0.01)
    )

    plot_ly(data, x = ~Disease, y = ~Rate, type = 'bar',
            marker = list(color = 'blue')) %>%
      layout(title = "District-wide Case Rates",
             xaxis = list(title = "Disease"),
             yaxis = list(title = "Rate per 100,000"))
  })
}

# --- 3. Run the App ---
shinyApp(ui = ui, server = server)