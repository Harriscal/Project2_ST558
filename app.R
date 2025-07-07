library(shiny)
library(shinyjs)
library(httr)
library(jsonlite)
library(tibble)
library(dplyr)
library(stringr)
library(DT)

#load helper function
source("helpers.R")

ui <- navbarPage("Weather Insights App",
                 
                 # --- About Tab ---
                 tabPanel("About",
                          fluidPage(
                            useShinyjs(),
                            titlePanel("About This App"),
                            fluidRow(
                              column(8,
                                     h4("Purpose"),
                                     p("This app allows users to query historical daily weather data from the ",
                                       a("Open-Meteo API", href = "https://open-meteo.com/en/docs", target = "_blank"),
                                       " and explore trends in temperature, precipitation, cloud cover, and UV index."),
                                     h4("Source Data"),
                                     p("Weather data is obtained from the free Open-Meteo API, which provides 
            global forecasts and historical weather variables."),
                                     h4("App Structure"),
                                     tags$ul(
                                       tags$li(strong("About:"), " Overview of the app and data."),
                                       tags$li(strong("Data Download:"), " Customize API queries and download weather data."),
                                       tags$li(strong("Data Exploration:"), " View graphical and numerical summaries.")
                                     )
                              ),
                              column(4,
                                     img(src = "open-meteo-logo.png", width = "100%", alt = "Open-Meteo Logo")
                              )
                            )
                          ),
                          
                 ),
                 tabPanel("Data Download",
                          fluidPage(
                            titlePanel("Download Weather Data"),
                            fluidRow(
                              # Column for user inputs (left)
                              column(
                                width = 4,
                                {
                                    min_available_date <- Sys.Date() - 74  # 74 days of historical data
                                    max_available_date <- Sys.Date() + 15  # API allows 15 days into the future
                                    
                                    tagList(
                                      textInput("location", "Enter City, State (US):", placeholder = "e.g., Raleigh, NC"),
                                      
                                      dateInput("start_date", "Start Date:",
                                                value = Sys.Date() - 7,
                                                min = min_available_date,
                                                max = max_available_date,
                                                width = "100%"),
                                      
                                      dateInput("end_date", "End Date:",
                                                value = Sys.Date(),
                                                min = min_available_date,
                                                max = max_available_date,
                                                width = "100%"),
                                      
                                      actionButton("get_data", "Get Weather Data"),
                                      
                                      helpText(paste0(
                                        "Note: Data is available from ",
                                        format(min_available_date, "%B %d, %Y"), " to ",
                                        format(max_available_date, "%B %d, %Y"), "."
                                      ))
                                    )
                                }
                              ),
                              
                              # Column for output (right)
                              column(
                                width = 8,
                                uiOutput("warning_text"),
                                br(),
                                uiOutput("loading_spinner"),
                                DTOutput("weather_table")
                              )
                            )
                          )
                 )
)

server <- function(input, output, session) {
  # Helper to check if inputs are valid
  is_valid_input <- reactive({
    req(input$location, input$start_date, input$end_date)
    days_apart <- as.numeric(difftime(input$end_date, input$start_date, units = "days"))
    max_future <- as.numeric(difftime(input$end_date, Sys.Date(), units = "days"))
    # Valid if: correct format, ≤ 15 days into future
    grepl("^.+,\\s?[A-Z]{2}$", input$location) &&
      days_apart >= 0 && max_future <= 15
  })
  
  # Enable/disable button based on validation
  observe({
    toggleState("get_data", condition = is_valid_input())
  })
  
  # Reactive value to store data
  weather_data <- reactiveVal(NULL)
  
  observeEvent(input$get_data, {
    output$warning_text <- renderUI({})
    output$loading_spinner <- renderUI({
      tags$div("Loading data...", style = "color: blue; font-weight: bold;")
    })
    
    tryCatch({
      data <- get_weather_data(
        location = input$location,
        start_date = as.character(input$start_date),
        end_date = as.character(input$end_date)
      )
      weather_data(data)
    }, error = function(e) {
      weather_data(NULL)
      output$warning_text <- renderUI({
        tags$p(style = "color: red;", paste("Error:", e$message))
      })
    })
    
    output$loading_spinner <- renderUI({})
  })
  
  output$weather_table <- renderDT({
    req(weather_data())
    datatable(
      weather_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
}

shinyApp(ui = ui, server = server)

