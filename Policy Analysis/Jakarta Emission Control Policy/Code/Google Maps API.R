# Install and load the required R packages
if (!require(googleway)) {
  install.packages("googleway")
}
library(googleway)

# Google Maps API Key
api_key <- "AIzaSyAuNE1yKxEPTspBgXEQ822HvoJGPaJCle8"

# Define the origin and destination coordinates
origin <- c(-6.2293768, 106.7986836)
destination <- c(-6.1811036, 106.8223689)

# Date range for a 3-day test in the Western Indonesia Time (WIB) zone
start_date <- as.POSIXct("2018-01-01 00:00:00", tz = "Asia/Jakarta")
end_date <- as.POSIXct("2018-01-03 00:00:00", tz = "Asia/Jakarta")

# Times in the WIB time zone
morning_time <- as.POSIXct("2018-01-01 07:00:00", tz = "Asia/Jakarta")
afternoon_time <- as.POSIXct("2018-01-01 18:00:00", tz = "Asia/Jakarta")

# Create a function to scrape historical traffic data
get_historical_traffic_data <- function(api_key, origin, destination, start_date, end_date, morning_time, afternoon_time) {
  traffic_data <- data.frame()
  dates <- seq(start_date, end_date, by = "days")
  
  for (date in dates) {
    # Morning request
    morning_request <- google_distance(origin = origin, destination = destination, mode = "driving", departure_time = morning_time, key = api_key)
    
    # Afternoon request with origin and destination swapped
    afternoon_request <- google_distance(origin = destination, destination = origin, mode = "driving", departure_time = afternoon_time, key = api_key)
    
    # Extract travel time or other relevant information from the responses
    morning_travel_time <- morning_request$rows$elements$duration$value / 60  # in minutes
    afternoon_travel_time <- afternoon_request$rows$elements$duration$value / 60  # in minutes
    
    # Create a data frame to store the results
    daily_data <- data.frame(
      Date = date,
      MorningTravelTime = morning_travel_time,
      AfternoonTravelTime = afternoon_travel_time
    )
    
    traffic_data <- rbind(traffic_data, daily_data)
  }
  
  return(traffic_data)
}

# Retrieve historical traffic data for 3 days
historical_traffic_data <- get_historical_traffic_data(api_key, origin, destination, start_date, end_date, morning_time, afternoon_time)

# Print the resulting data frame
print(historical_traffic_data)