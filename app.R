library(shiny)
library(shinydashboard)
library(DT)
library(lubridate)
library(shinyjs)
source("helpers.R")

valid_start <- Sys.Date() - 74
valid_end <- Sys.Date() + 15

all_vars <- c(
  "Date", 
  "Max Temp (°F)", 
  "Min Temp (°F)", 
  "Mean Temp (°F)", 
  "Rainfall (in)", 
  "UV Index", 
  "Cloud Cover (%)", 
  "Hot Day?"
)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Weather Insights App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Data Download", tabName = "download", icon = icon("cloud-download-alt")),
      menuItem("Data Exploration", tabName = "exploration", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  width = 12,
                  title = "About This App",
                  status = "primary",
                  solidHeader = TRUE,
                  column(8,
                         h4("Purpose"),
                         p("This app allows users to query historical daily weather data from the ",
                           a("Open-Meteo API", href = "https://open-meteo.com/en/docs", target = "_blank"),
                           ", and explore trends in temperature, precipitation, cloud cover, and UV index."),
                         h4("Source Data"),
                         p("Weather data is obtained from the free Open-Meteo API, which provides global forecasts and historical weather variables."),
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
              )
      ),
      
      # Data Download Tab
      tabItem(tabName = "download",
              fluidRow(
                box(
                  title = "Input",
                  width = 4,
                  status = "primary",
                  textOutput("instructions"),
                  textInput("location", "Enter City, ST (e.g., Raleigh, NC):", value = ""),
                  dateInput("start_date", "Start Date:", value = Sys.Date() - 7, min = valid_start, max = valid_end),
                  dateInput("end_date", "End Date:", value = Sys.Date(), min = valid_start, max = valid_end),
                  helpText("Valid range:", format(valid_start), "to", format(valid_end)),
                  checkboxGroupInput("vars_to_show", "Select Variables to Display:", choices = all_vars, selected = all_vars),
                  actionButton("get_data", "Get Weather Data"),
                  br(), br(),
                  downloadButton("download_csv", "Download CSV", class = "btn-primary", disabled = TRUE)
                ),
                box(
                  title = "Preview",
                  width = 8,
                  status = "info",
                  DTOutput("weather_table")
                )
              )
      ),
      
      # Data Exploration Tab
      tabItem(tabName = "exploration",
              h2("Data Exploration (Coming Soon)")
      )
    )
  )
)

server <- function(input, output, session) {
  output$instructions <- renderText("Enter a location as 'City, ST' (e.g., Raleigh, NC), set date range, then click 'Get Weather Data'.")
  
  weather_data <- reactiveVal()
  
  observe({
    shinyjs::disable("download_csv")
  })
  
  observeEvent(input$get_data, {
    req(input$location, input$start_date, input$end_date)
    
    tryCatch({
      df <- get_weather_data(input$location, input$start_date, input$end_date)
      weather_data(df)
      shinyjs::enable("download_csv")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$weather_table <- renderDT({
    req(weather_data(), input$vars_to_show)
    df <- weather_data()[, input$vars_to_show, drop = FALSE]
    datatable(df, options = list(pageLength = 10))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("weather_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- weather_data()[, input$vars_to_show, drop = FALSE]
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
