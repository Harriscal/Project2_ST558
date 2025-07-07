library(httr)
library(jsonlite)
library(tibble)
library(dplyr)
library(stringr)



# Helper function to convert "City, State" into latitude and longitude
get_lat_lon <- function(location) {
  # Validate that input is in "City, ST" format
  if (!grepl("^.+,\\s?[A-Z]{2}$", location)) {
    stop("Invalid location format. Please enter as 'City, State' (e.g., Raleigh, NC).")
  }
  
  # Build the Nominatim geocoding query
  nominatim_url <- "https://nominatim.openstreetmap.org/search"
  res <- httr::GET(
    url = nominatim_url,
    query = list(
      q = paste(location, "USA"),  # example: "Raleigh, NC USA"
      format = "json",
      limit = 1,
      addressdetails = 0
    ),
    user_agent("R-WeatherApp/1.0")  # Required by Nominatim's terms
  )
  
  # Stop if request failed
  httr::stop_for_status(res)
  
  # Parse JSON response
  geo_data <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  
  # Check if we got a result
  if (length(geo_data) == 0) {
    stop("Could not find location. Please check your input.")
  }
  
  # Extract coordinates
  lat <- as.numeric(geo_data$lat[1])
  lon <- as.numeric(geo_data$lon[1])
  
  list(lat = lat, lon = lon)
}



get_weather_data <- function(location, start_date, end_date) {
  # Get latitude and longitude from the city, state input
  coords <- get_lat_lon(location)
  
  # Open-Meteo base API endpoint
  base_url <- "https://api.open-meteo.com/v1/forecast"
  
  # Define query parameters for API call
  params <- list(
    latitude = coords$lat,
    longitude = coords$lon,
    start_date = start_date,
    end_date = end_date,
    daily = paste(
      "temperature_2m_max",
      "temperature_2m_min",
      "temperature_2m_mean",
      "uv_index_max",
      "rain_sum",
      "cloudcover_mean",
      sep = ","
    ),
    temperature_unit = "fahrenheit",  # Display temps in °F
    timezone = "auto"                 # Automatically adjust timezone based on location
  )
  
  # Make GET request to API
  response <- GET(url = base_url, query = params)
  stop_for_status(response)  # Stop if there’s an error
  
  # Parse response JSON to extract data
  json <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  data <- as_tibble(json$daily)
  
  # Create derived variables:
  # - Convert rain from mm to inches
  # - Create binary "Hot Day?" flag if max temp ≥ 80°F
  data <- data |>
    mutate(
      `Rainfall (in)` = round(rain_sum * 0.0393701, 2),
      `Hot Day?` = ifelse(temperature_2m_max >= 80, "Yes", "No")
    ) |>
    
    # Rename and reorder columns for clarity
    transmute(
      `Date` = time,
      `Max Temp (°F)` = temperature_2m_max,
      `Min Temp (°F)` = temperature_2m_min,
      `Mean Temp (°F)` = temperature_2m_mean,
      `Rainfall (in)`,
      `UV Index` = uv_index_max,
      `Cloud Cover (%)` = cloudcover_mean,
      `Hot Day?`
    )
  
  return(data)
}
