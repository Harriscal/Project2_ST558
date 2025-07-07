# app.R

library(shiny)

ui <- navbarPage("Weather Insights App",
                 
                 # --- About Tab ---
                 tabPanel("About",
                          fluidPage(
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
                          )
                 )
                 
                 # Additional tabs to be added later
)

server <- function(input, output, session) {
  # Placeholder for server logic
}

shinyApp(ui = ui, server = server)

