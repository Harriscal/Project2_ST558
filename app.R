# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(lubridate)
library(shinyjs)
library(plotly)
library(bslib)
library(GGally)

# Load helper function to fetch weather data via API
source("helpers.R")

# Set valid date range for querying data
valid_start <- Sys.Date() - 74
valid_end <- Sys.Date() + 15

# Define all possible weather variables for display
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

# === UI ===
ui <- dashboardPage(
  skin = "blue",  # Set theme color
  dashboardHeader(title = "Weather Insights App"),
  
  # Sidebar with navigation
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Data Download", tabName = "download", icon = icon("cloud-download-alt")),
      menuItem("Data Exploration", tabName = "exploration", icon = icon("chart-line"))
    )
  ),
  
  # Body containing tab content
  dashboardBody(
    useShinyjs(),  # Enable shinyjs for interactive control (like disabling buttons)
    
    # Custom CSS to fix calendar UI overlap issue
    tags$head(
      tags$style(HTML("
        .datepicker { z-index: 9999 !important; }
        .form-group { overflow: visible !important; }
      "))
    ),
    
    # Define individual tabs
    tabItems(
      
      # === About Page ===
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
                         p("Weather data is obtained from the free Open-Meteo API."),
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
      
      # === Data Download Page ===
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
                  downloadButton("download_csv", "Download CSV", class = "btn-primary")
                ),
                box(
                  title = "Preview",
                  width = 8,
                  status = "info",
                  DTOutput("weather_table"),
                  uiOutput("no_data_message_download")  # Red message shown if no data
                )
              )
      ),
      
      # === Data Exploration Page ===
      tabItem(tabName = "exploration",
              h2("Data Exploration"),
              uiOutput("no_data_message_explore"),  # Conditional warning message
              conditionalPanel(
                condition = "output.dataAvailable == true",  # Only show when data is present
                
                # === Summary Tables Tab ===
                tabsetPanel(
                  tabPanel("Summary Tables",
                           fluidRow(
                             box(title = "Hot Day by Cloud Cover", width = 12, tableOutput("contingency1"), uiOutput("cloud_legend")),
                             box(title = "Hot Day by UV Level", width = 12, tableOutput("contingency2"), uiOutput("uv_legend")),
                             box(title = "Rain by Cloud Cover", width = 12, tableOutput("contingency3"), uiOutput("rain_legend"))
                           ),
                           fluidRow(
                             box(title = "Mean Temp by Cloud Cover", width = 12, tableOutput("numeric_summary2")),
                             box(title = "UV Index by Cloud Cover", width = 12, tableOutput("numeric_summary1")),
                             box(title = "UV Index by Hot Day", width = 12, tableOutput("numeric_summary3"))
                           ),
                           fluidRow(
                             box(title = "Rainfall by Cloud Cover", width = 12, tableOutput("rain_summary_by_cloud"))
                           )
                  ),
                  
                  # === Visualizations Tab ===
                  tabPanel("Visualizations",
                           tabsetPanel(
                             tabPanel("Heatmap", plotlyOutput("correlation_heatmap")),
                             tabPanel("Box Plot", plotlyOutput("uv_boxplot")),
                             tabPanel("Bubble Plot", plotlyOutput("bubble_plot")),
                             tabPanel("Line Plot", plotlyOutput("mean_temp_time"))
                           )
                  )
                )
              )
      )
    )
  )
)

# === Server ===
server <- function(input, output, session) {
  output$instructions <- renderText(
    "Enter a location in the format 'City, ST' (e.g., Raleigh, NC), choose a date range, then click 'Get Weather Data'."
  )
  
  weather_data <- reactiveVal(NULL)
  
  output$dataAvailable <- reactive({
    !is.null(weather_data())
  })
  outputOptions(output, "dataAvailable", suspendWhenHidden = FALSE)
  
  observe({
    toggleState("download_csv", condition = !is.null(weather_data()))
  })
  
  observeEvent(input$get_data, {
    req(input$location, input$start_date, input$end_date)
    
    if (input$start_date < valid_start || input$end_date > valid_end) {
      showNotification("Date range must be within 74 days before and 15 days after today.", type = "error")
      return(NULL)
    }
    
    tryCatch({
      df <- get_weather_data(input$location, input$start_date, input$end_date)
      weather_data(df)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$weather_table <- renderDT({
    req(weather_data(), input$vars_to_show)
    datatable(weather_data()[, input$vars_to_show, drop = FALSE],
              options = list(pageLength = 10))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("weather_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(weather_data()[, input$vars_to_show, drop = FALSE], file, row.names = FALSE)
    }
  )
  
  output$no_data_message_download <- renderUI({
    if (is.null(weather_data())) {
      tags$p("Please enter a location and click 'Get Weather Data' to preview and download data.", style = "color: red;")
    }
  })
  
  output$no_data_message_explore <- renderUI({
    if (is.null(weather_data())) {
      tags$p("Please enter a location and click 'Get Weather Data' to explore summaries and plots.", style = "color: red;")
    }
  })
  
  # ===== CONTINGENCY TABLES =====
  output$contingency1 <- renderTable({
    req(weather_data())
    df <- weather_data() |>
      mutate(CloudGroup = cut(`Cloud Cover (%)`, breaks = c(-Inf, 33, 66, Inf), labels = c("Low", "Medium", "High")))
    
    tab <- as.data.frame(table(`Cloud Cover Category` = df$CloudGroup, `Hot Day?` = df$`Hot Day?`))
    colnames(tab) <- c("Cloud Cover Category", "Hot Day?", "Count")
    tab
  })
  
  output$contingency2 <- renderTable({
    req(weather_data())
    df <- weather_data() |>
      mutate(UVGroup = cut(`UV Index`, breaks = c(-Inf, 3, 6, Inf), labels = c("Low", "Moderate", "High")))
    
    tab <- as.data.frame(table(`UV Index Level` = df$UVGroup, `Hot Day?` = df$`Hot Day?`))
    colnames(tab) <- c("UV Index Level", "Hot Day?", "Count")
    tab
  })
  
  output$contingency3 <- renderTable({
    req(weather_data())
    df <- weather_data() |>
      mutate(CloudGroup = cut(`Cloud Cover (%)`, breaks = c(-Inf, 33, 66, Inf), labels = c("Low", "Medium", "High")),
             RainBinary = ifelse(`Rainfall (in)` > 0, "Rain", "No Rain"))
    
    tab <- as.data.frame(table(`Cloud Cover Category` = df$CloudGroup, `Rain?` = df$RainBinary))
    colnames(tab) <- c("Cloud Cover Category", "Rain?", "Count")
    tab
  })
  
  # ===== MESSAGES FOR TABLES =====
  # Explanation for Cloud Cover Category
  output$cloud_legend <- renderUI({
    HTML("<em>Cloud Cover Category:</em> Low = 0–33%, Medium = 34–66%, High = 67–100%")
  })
  
  # Explanation for UV Index Category
  output$uv_legend <- renderUI({
    HTML("<em>UV Index Level:</em> Low = 0–3, Moderate = 4–6, High = 7+")
  })
  
  # Explanation for Rain (binary)
  output$rain_legend <- renderUI({
    HTML("<em>Rain?:</em> Yes = Rainfall > 0 in, No = 0 in")
  })
  
  
  # ===== NUMERICAL SUMMARIES =====
  output$numeric_summary1 <- renderTable({
    req(weather_data())
    weather_data() |>
      mutate(CloudGroup = cut(`Cloud Cover (%)`, breaks = c(-Inf, 33, 66, Inf), labels = c("Low", "Medium", "High"))) |>
      group_by(CloudGroup) |>
      summarise(Mean_UV = round(mean(`UV Index`, na.rm = TRUE), 2),
                SD_UV = round(sd(`UV Index`, na.rm = TRUE), 2),
                .groups = "drop") |>
      filter(!is.na(SD_UV))
  })
  
  output$numeric_summary2 <- renderTable({
    req(weather_data())
    weather_data() |>
      mutate(CloudGroup = cut(`Cloud Cover (%)`, breaks = c(-Inf, 33, 66, Inf), labels = c("Low", "Medium", "High"))) |>
      group_by(CloudGroup) |>
      summarise(Mean_Temp = round(mean(`Mean Temp (°F)`, na.rm = TRUE), 2),
                SD_Temp = round(sd(`Mean Temp (°F)`, na.rm = TRUE), 2),
                .groups = "drop") |>
      filter(!is.na(SD_Temp))
  })
  
  output$numeric_summary3 <- renderTable({
    req(weather_data())
    weather_data() |>
      group_by(`Hot Day?`) |>
      summarise(Mean_UV = round(mean(`UV Index`, na.rm = TRUE), 2),
                SD_UV = round(sd(`UV Index`, na.rm = TRUE), 2),
                .groups = "drop") |>
      filter(!is.na(SD_UV))
  })
  
  output$rain_summary_by_cloud <- renderTable({
    req(weather_data())
    weather_data() |>
      mutate(CloudGroup = cut(`Cloud Cover (%)`, breaks = c(-Inf, 33, 66, Inf), labels = c("Low", "Medium", "High"))) |>
      group_by(CloudGroup) |>
      summarise(Mean_Rain = round(mean(`Rainfall (in)`, na.rm = TRUE), 2),
                SD_Rain = round(sd(`Rainfall (in)`, na.rm = TRUE), 2),
                .groups = "drop") |>
      filter(!is.na(SD_Rain))
  })
  
  # ===== PLOTS =====
  output$correlation_heatmap <- renderPlotly({
    req(weather_data())
    df <- weather_data() |> select(where(is.numeric))
    df <- df[, sapply(df, function(x) sd(x, na.rm = TRUE) > 0)]
    if (ncol(df) < 2) return(plotly_empty(type = "scatter") |> layout(title = "Not enough numeric variables"))
    plot_ly(z = round(cor(df, use = "complete.obs"), 2),
            x = colnames(df), y = rownames(df),
            type = "heatmap", colors = "RdBu", reversescale = TRUE) |>
      layout(title = "Correlation Heatmap")
  })
  
  output$uv_boxplot <- renderPlotly({
    req(weather_data())
    df <- weather_data() |>
      mutate(CloudGroup = cut(`Cloud Cover (%)`, breaks = c(-Inf, 33, 66, Inf), labels = c("Low", "Medium", "High")))
    p <- ggplot(df, aes(x = CloudGroup, y = `UV Index`, fill = CloudGroup)) +
      geom_boxplot() +
      labs(title = "UV Index by Cloud Cover Category", x = "Cloud Cover Category", y = "UV Index") +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$bubble_plot <- renderPlotly({
    req(weather_data())
    df <- weather_data()
    plot_ly(
      data = df,
      x = ~`UV Index`,
      y = ~`Mean Temp (°F)`,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 10, opacity = 0.7, color = ~`Cloud Cover (%)`, colorscale = 'Viridis',
                    colorbar = list(title = "Cloud Cover (%)")),
      text = ~paste("Date:", Date, "<br>UV Index:", `UV Index`,
                    "<br>Temp:", `Mean Temp (°F)`, "<br>Cloud:", `Cloud Cover (%)`),
      hoverinfo = 'text'
    ) |> layout(title = "UV vs Temp (Color = Cloud Cover)",
                xaxis = list(title = "UV Index"),
                yaxis = list(title = "Mean Temp (°F)"))
  })
  
  output$mean_temp_time <- renderPlotly({
    req(weather_data())
    df <- weather_data()
    df$Date <- as.Date(df$Date)
    p <- ggplot(df, aes(x = Date, y = `Mean Temp (°F)`, color = `Hot Day?`)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Mean Temp Over Time by Hot Day", x = "Date", y = "Mean Temp (°F)") +
      theme_minimal() +
      scale_color_manual(values = c("Yes" = "tomato", "No" = "steelblue"))
    ggplotly(p) |> layout(xaxis = list(type = "date", tickformat = "%b %d", tickangle = -45))
  })
}

shinyApp(ui, server)

