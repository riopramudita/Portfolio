# Install and load the required R packages
if (!require(httr)) {
  install.packages("httr")
}
library(httr)

# API Key for Traffic Stats API
api_key <- "YOUR_TRAFFIC_STATS_API_KEY"

# Origin and Destination Coordinates
origin <- c(-6.2293768, 106.7986836)
destination <- c(-6.1811036, 106.8223689)

# Date Range
start_date <- "2018-01-01T07:30:00Z"
end_date <- "2018-12-31T18:00:00Z"

# Function to retrieve historical traffic data
get_historical_traffic_data <- function(api_key, origin, destination, start_time, end_time) {
  url <- paste("https://api.provider.com/traffic/stats/traffic-density", sep = "", "?", 
               paste("api_key=", api_key, sep = "", collapse = "&"), 
               paste("origin=", paste(origin, collapse = ","), sep = "&"),
               paste("destination=", paste(destination, collapse = ","), sep = "&"),
               paste("start_time=", start_time, sep = "&"),
               paste("end_time=", end_time, sep = "&")
  )
  
  response <- GET(url)
  if (http_type(response) == "application/json") {
    traffic_data <- content(response, "text", encoding = "UTF-8")
    return(traffic_data)
  } else {
    stop("Error: Unable to retrieve historical traffic data.")
  }
}

# Usage
historical_traffic_data <- get_historical_traffic_data(api_key, origin, destination, start_date, end_date)

# Print or process the resulting historical traffic data
cat("Historical Traffic Data:\n")
cat(historical_traffic_data)
