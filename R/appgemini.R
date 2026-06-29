library(shiny)
library(plotly)
library(htmltools)
library(dplyr)
ui <- fluidPage(
  titlePanel("Infectious Disease Case Rates (Accessible Demo)"),
  tags$head(
  tags$style(
    HTML(
      '.sr-only {
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
      .chart-container:focus {
      outline: 4px solid #3b82f6;outline-offset: 4px;
      }'))),
  # FIX: Changed to uiOutput so the text updates automatically with the data
  uiOutput("chart_description_accessible"),
  # Accessible wrapper container
  tags$div(class = "chart-container",
           tabindex = "0",role = "img",
           aria-label = "Bar chart showing case rates for infectious diseases by district.",
           aria-describedby = "chart-description",
           # Wrap the plotly output in an aria-hidden span so screen readers
           # # do not try to parse Plotly's internal SVG vector paths directly.
                 tags$span(                                                                                                                                                                                                                                                                                                                                                                                                      `aria-hidden` = "true",
                 plotlyOutput("case_rates_plot")
                           )
  ))
server <- function(input, output, session) {
  # A reactive dataset ensures both the visual plot and the text explanation pull from the exact same source
  chart_data <- reactive({data.frame(
    Disease = c("Hepatitis c", "Pertussis", "Measles", "Mumps", "Hepatitis b Acute"),
    Rate = c(0.1, 0.05, 0.001, 0.02, 0.01),
    stringsAsFactors = FALSE)})

  # Dynamically generate the screen-reader description text based on the active data
  output$chart_description_accessible <- renderUI({df <- chart_data()
  total_illnesses <- nrow(df)
  max_row <- df[which.max(df$Rate), ]
  all_diseases <- paste(df$Disease, collapse = ", ")
 })
  output$case_rates_plot <- renderPlotly({
    df <- chart_data()
    plot_ly(df, x = ~Disease, y = ~Rate, type = 'bar',
           marker = list(color = 'blue')) %>%
    layout(
      title = "District-wide Case Rates",
      xaxis = list(title = "Disease"),
      yaxis = list(title = "Rate per 100,000")
    ) %>%
    # FIX: Suppress the interactive modebar to prevent keyboard users
    # from getting stuck in an endless tab loop across toolbar options
    config(displayModeBar = FALSE)
  })}shinyApp(ui = ui, server = server)