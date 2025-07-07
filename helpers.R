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
