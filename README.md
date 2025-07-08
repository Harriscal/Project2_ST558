# Project2_ST558

## Description
The Weather Insights App is a Shiny dashboard that allows users to:
- Query and download daily weather data using the Open-Meteo API (https://open-meteo.com/en/docs)
- Visualize temperature, precipitation, UV index, and cloud cover patterns
- Explore contingency tables and summary statistics
- Create interactive plots to uncover trends in weather conditions

This tool is designed to help users gain insights into weather variability over a custom date range for any U.S. city.

## Required R Packages
To run the app, you'll need the following R packages:

- shiny

- shinydashboard

- DT

- lubridate

- shinyjs

- plotly

- bslib

- GGally

- dplyr

- ggplot2

Keep in mind, there is a helpers.r file that use as a source in the main app.r with these R packages:

- httr

- jsonlite

- tibble

- dplyr

- stringr


## Install All Packages
You can install all the required packages with one line of code:

install.packages(c("shiny", "shinydashboard", "DT", "lubridate", "shinyjs",
                   "plotly", "bslib", "GGally", "dplyr", "ggplot2", "httr", "tibble", "stringr"))
                   
## Run the App from GitHub

To run this app directly from GitHub using shiny::runGitHub(), use the following code:
shiny::runGitHub("Project2_ST558", "Harriscal")

## Notes
- Data availability is based on the Open-Meteo API, which supports up to 74 days in the past and 15 days into the future.
- No API key is required.
