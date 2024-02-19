
# Load necessary library
library(writexl)
library(dplyr)
library(tidyr)

# Set the working directory (change the path accordingly)
getwd()

# Define the list of 25 EU countries (EU exclude MT and CY)
countries <- c("AT", "BE", "BG", "CZ", 
               "DE", "DK", "EE", "ES", "FI", 
               "FR", "GR", "HR", "HU", "IE", 
               "IT", "LT", "LU", "LV", "NL", 
               "PL", "PT", "RO", "SE", "SI", 
               "SK")

# Specify the folder where CSV files are located
folder <- "Raw Data"

# Define an empty list to store data frames for each country
list_country_anually <- list()
list_EU <- list()

# Define the power generation column names
column_names <- c(
  "biomass_mw", "browncoal_mw", "coalderivedgas_mw", "gas_mw", "hardcoal_mw", "oil_mw", "oilshale_mw",
  "peat_mw", "geothermal_mw", "hydrostorage_mw", "hydrostorageconsump_mw", "hydroriver_mw",
  "hydroreservoir_mw", "marine_mw", "nuclear_mw", "other_mw", "otherrenewable_mw", "solar_mw",
  "waste_mw", "windoffshore_mw", "windonshore_mw"
)

# Import and clean data for each country and year
for (year in 2020:2023) {
  list_country_anually <- list()
  
  for (country in countries) {
    # Construct the file path to the CSV file within the "October 29" folder
    csv_file_path <- file.path(folder, sprintf("%s_%d.csv", country, year))
    
    # Read the CSV file
    data <- read.csv(csv_file_path, header = TRUE)

    # Split "MTU" column into start and end columns
    data <- separate(data, MTU, into = c("starttime", "endtime"), sep = " - ")

    # Extract timezone information from "endtime" column
    data$tz <- sub("^.*\\((.*)\\)$", "\\1", data$endtime)
    
    # Extract date from "MTU" column
    data$date <- as.Date(sub("(\\d{2}\\.\\d{2}\\.\\d{4}).*", "\\1", data$starttime), format = "%d.%m.%Y")

    # Convert start and end columns to POSIXct
    data$starttime <- as.POSIXct(data$starttime, format = "%d.%m.%Y %H:%M")
    data$endtime <- as.POSIXct(data$endtime, format = "%d.%m.%Y %H:%M")
    
    # Filter data for dates up to the cut of date
    data <- data %>% filter(date <= as.Date("2023-12-31"))
    
    # Rename the columns
    colnames(data)[4:24] <- column_names
    
    # Replace missing values with NA
    data[, column_names] <- lapply(data[, column_names], function(x) {
      ifelse(x == "n/e" | x == "N/A" | x == "", NA, x)
    })
    
    # Convert specified columns to numeric
    data[, column_names] <- lapply(data[, column_names], function(x) {as.numeric(as.character(x))})
    
    # Convert "Area" column to factor
    data$Area <- as.factor(data$Area)
    data$tz <- as.factor(data$tz)
    
    # Calculate difference in minutes
    data$diff_minutes <- as.numeric(difftime(data$endtime, data$starttime, units = "mins"))
    data$diff_minutes <- ifelse(data$diff_minutes > 60, data$diff_minutes - 60, data$diff_minutes) # to consider the daylight saving
    
    # Calculate the MWh for each generation
    for (column in column_names) {
      column_names_new <- paste0(column, "h")
      data[[column_names_new]] <- data[[column]] * (data$diff_minutes/60)
    }
    
    # Select columns to keep
    data <- data %>%
      select(-one_of(column_names))
    
    # Save the data frame in the list
    list_country_anually[[paste(country, year, sep = "_")]] <- data

  }
  # Combine all data frames into one
  df_EU_anually <- do.call(rbind, list_country_anually)
  rownames(df_EU_anually) <- NULL
  
  # Aggregate from sub-sector generation
  df_EU_anually$coal_tot_mwh <- rowSums(df_EU_anually[, c("browncoal_mwh", "coalderivedgas_mwh", 
                                                          "hardcoal_mwh","peat_mwh")], na.rm = TRUE)
  df_EU_anually$oil_tot_mwh <- rowSums(df_EU_anually[, c("oil_mwh", "oilshale_mwh")], na.rm = TRUE)
  df_EU_anually$hydrostorageconsump_mwh <- -abs(df_EU_anually$hydrostorageconsump_mwh)
  df_EU_anually$hydro_tot_mwh <- rowSums(df_EU_anually[, c("hydroriver_mwh", "hydroreservoir_mwh", 
                                                           "hydrostorage_mwh", "hydrostorageconsump_mwh")], na.rm = TRUE)
  df_EU_anually$wind_tot_mwh <- rowSums(df_EU_anually[, c("windonshore_mwh", "windoffshore_mwh")], na.rm = TRUE)
  df_EU_anually$otherrenewable_tot_mwh <- rowSums(df_EU_anually[, c("otherrenewable_mwh", "biomass_mwh",
                                                                    "geothermal_mwh", "marine_mwh", "waste_mwh")], na.rm = TRUE)
  df_EU_anually$total_mwh <- rowSums(df_EU_anually[, c("gas_mwh", "coal_tot_mwh", "oil_tot_mwh", "hydro_tot_mwh",
                                                       "nuclear_mwh", "wind_tot_mwh", "solar_mwh", "otherrenewable_tot_mwh", 
                                                       "other_mwh")], na.rm = TRUE)
  
  # separate Area into country's name and ISO code
  df_EU_anually <- df_EU_anually %>%
    separate(Area, into = c("country", "code"), sep = " \\(") %>%
    mutate(code = as.factor(substr(code, 1, nchar(code) - 1)),
           country = as.factor(country))
  
  # Change the unit of observation into daily to make it uniform for all countries
  df_EU_anually <- df_EU_anually %>%
    group_by(code, country, date) %>%
    summarise(
      gas_twh_sum = sum(gas_mwh, na.rm = TRUE) /1000000,
      coal_tot_twh_sum = sum(coal_tot_mwh, na.rm = TRUE) /1000000,
      oil_tot_twh_sum = sum(oil_tot_mwh, na.rm = TRUE) /1000000,
      hydro_tot_twh_sum = sum(hydro_tot_mwh, na.rm = TRUE) /1000000,
      nuclear_twh_sum = sum(nuclear_mwh, na.rm = TRUE) /1000000,
      wind_tot_twh_sum = sum(wind_tot_mwh, na.rm = TRUE) /1000000,
      solar_twh_sum = sum(solar_mwh, na.rm = TRUE) /1000000,
      otherrenewable_tot_twh_sum = sum(otherrenewable_tot_mwh, na.rm = TRUE) /1000000,
      other_twh_sum = sum(other_mwh, na.rm = TRUE) /1000000,
      total_twh_sum = sum(total_mwh, na.rm = TRUE) /1000000
    ) %>% 
    ungroup()
  
  # Save the df_EU_anually into a CSV file named by the year
  output_file_path <- file.path("Output", sprintf("EU_daily_%d.csv", year))
  write.csv(df_EU_anually, file = output_file_path, row.names = FALSE)
  
  # to calculate monthly aggregate value each of all countries, and change units into twh
  data_month <- df_EU_anually %>%
    group_by(code, country, year = lubridate::year(date), month = lubridate::month(date)) %>%
    summarise(
      gas_twh_sum = sum(gas_twh_sum, na.rm = TRUE),
      coal_tot_twh_sum = sum(coal_tot_twh_sum, na.rm = TRUE),
      oil_tot_twh_sum = sum(oil_tot_twh_sum, na.rm = TRUE),
      hydro_tot_twh_sum = sum(hydro_tot_twh_sum, na.rm = TRUE),
      nuclear_twh_sum = sum(nuclear_twh_sum, na.rm = TRUE),
      wind_tot_twh_sum = sum(wind_tot_twh_sum, na.rm = TRUE),
      solar_twh_sum = sum(solar_twh_sum, na.rm = TRUE),
      otherrenewable_tot_twh_sum = sum(otherrenewable_tot_twh_sum, na.rm = TRUE),
      other_twh_sum = sum(other_twh_sum, na.rm = TRUE),
      total_twh_sum = sum(total_twh_sum, na.rm = TRUE)
    ) %>% 
    ungroup()
  
  # Add all EU aggregate
  data_month_temp <- data_month %>%
    group_by(year, month) %>%
    summarise(
      gas_twh_sum = sum(gas_twh_sum, na.rm = TRUE),
      coal_tot_twh_sum = sum(coal_tot_twh_sum, na.rm = TRUE),
      oil_tot_twh_sum = sum(oil_tot_twh_sum, na.rm = TRUE),
      hydro_tot_twh_sum = sum(hydro_tot_twh_sum, na.rm = TRUE),
      nuclear_twh_sum = sum(nuclear_twh_sum, na.rm = TRUE),
      wind_tot_twh_sum = sum(wind_tot_twh_sum, na.rm = TRUE),
      solar_twh_sum = sum(solar_twh_sum, na.rm = TRUE),
      otherrenewable_tot_twh_sum = sum(otherrenewable_tot_twh_sum, na.rm = TRUE),
      other_twh_sum = sum(other_twh_sum, na.rm = TRUE),
      total_twh_sum = sum(total_twh_sum, na.rm = TRUE)
    ) %>% 
    mutate(code = "EU",
           country = "All EU countries") %>% 
    ungroup()
  
  data_month <- rbind(data_month, data_month_temp)
  data_month <- data_month %>%
    mutate(
      month = as.factor(month),
      year = as.factor(year)
    )
  
  # Save the data_month into a CSV file named by the year
  output_file_path <- file.path("Output", sprintf("EU_monthly_%d.csv", year))
  write.csv(data_month, file = output_file_path, row.names = FALSE)
  
  # Save the data frame in the list yearly
  list_EU[[year]] <- data_month  
}

# Combine all data frames into one
df_EU_all <- do.call(rbind, list_EU)
rownames(df_EU_all) <- NULL

long_df_EU_all <- pivot_longer(df_EU_all, 
                               cols = c(gas_twh_sum, coal_tot_twh_sum, oil_tot_twh_sum, 
                                        hydro_tot_twh_sum, nuclear_twh_sum, wind_tot_twh_sum, 
                                        solar_twh_sum, otherrenewable_tot_twh_sum, other_twh_sum), 
                               names_to = "technology", 
                               values_to = "value") %>% 
  mutate(technology = recode(technology,
                             gas_twh_sum = "Gas",
                             coal_tot_twh_sum = "Coal",
                             oil_tot_twh_sum = "Oil",
                             hydro_tot_twh_sum = "Hydro",
                             nuclear_twh_sum = "Nuclear",
                             wind_tot_twh_sum = "Wind",
                             solar_twh_sum = "Solar",
                             otherrenewable_tot_twh_sum = "Other Renewable",
                             other_twh_sum = "Other")) %>% 
  select(-total_twh_sum) %>%
  mutate(group = case_when(
    technology %in% c("Gas", "Coal", "Oil", "Other") ~ "Fossil",
    technology %in% c("Hydro", "Wind", "Solar", "Other Renewable") ~ "Renewable",
    technology == "Nuclear" ~ "Nuclear",
    TRUE ~ NA_character_  # for handling any unexpected values
  ))

# Save the data_month into a CSV file
output_file_path <- file.path("Output", sprintf("EU_all_longdf.csv"))
write.csv(long_df_EU_all, file = output_file_path, row.names = FALSE)
