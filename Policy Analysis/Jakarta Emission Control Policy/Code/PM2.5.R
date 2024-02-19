library(raster)
library(ncdf4)
library(rasterVis)
library(sf)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)

getwd()

# Set the base directory and file prefix for monthly measurement
base_dir <- "/Users/rioprmdt/Library/CloudStorage/GoogleDrive-rpk2133@columbia.edu/My Drive/1. Columbia/1. Academics/R/Final Project/Data Set/WUSTL/Monthly/"
file_prefix <- "V5GL03.HybridPM25.Asia."

# Set the base directory and file prefix for uncertainty
base_dir_unc <- "/Users/rioprmdt/Library/CloudStorage/GoogleDrive-rpk2133@columbia.edu/My Drive/1. Columbia/1. Academics/R/Final Project/Data Set/WUSTL/Uncertainty/"
file_prefix_unc <- "V5GL03.HybridPM25E.Asia."

# Read the Jakarta administrative boundary from the GeoPackage file
jakarta_gpkg <- st_read("Data Set/DKI Jakarta/Kabupaten-DKI-Jakarta.gpkg")

plot(jakarta_gpkg)

# Specify the bounding box for Jakarta (adjust coordinates accordingly)
jakarta_bbox <- c(xmin = 106.65, xmax = 107, ymin = -6.4, ymax = -6.05)

# Initialize an empty list to store data frames for each year
all_years_df_list <- list()

# Loop through years from 2011 to 2021
for (year in 2021:2021) {
  year_df_list <- list()
  
  for (month in 12:12) {
    month_str <- sprintf("%02d", month)
    file_path <- paste0(base_dir, file_prefix, year, month_str, "-", year, month_str, ".nc")
    #file_path_unc <- paste0(base_dir_unc, file_prefix_unc, year, month_str, "-", year, month_str, ".nc")
    
    # Read PM2.5 data
    raster_data <- raster(file_path)
    clipped_raster <- crop(raster_data, extent(jakarta_bbox))
    clipped_raster <- mask(clipped_raster, jakarta_gpkg)
    raster_df <- as.data.frame(clipped_raster, xy = TRUE, centroids = TRUE, na.rm = TRUE)
    names(raster_df)[names(raster_df) == "Hybrid.PM_2_._5...mug.m.3."] <- "pm2.5"
    
    # Read uncertainty data
    #raster_data_unc <- raster(file_path_unc)
    #clipped_raster_unc <- crop(raster_data_unc, extent(jakarta_bbox))
    #clipped_raster_unc <- mask(clipped_raster_unc, jakarta_gpkg)
    #raster_df_unc <- as.data.frame(clipped_raster_unc, xy = TRUE, centroids = TRUE, na.rm = TRUE)
    #names(raster_df_unc)[names(raster_df_unc) == "Hybrid.PM_2_._5.Uncertainty...mug.m.3."] <- "uncertainty"
    
    # Combine PM2.5 and uncertainty data
    #combined_raster <- merge(raster_df, raster_df_unc, by = c("x", "y"))
    raster_df$Year <- as.factor(year)
    raster_df$Month <- as.factor(month)
    year_df_list[[month]] <- raster_df
    
    # Export to CSV
    csv_filename <- paste0("pollution_month_", month, ".csv")
    write.csv(raster_df, file = csv_filename, row.names = FALSE)
  }
  
  # Combine the year_df_list into one tall dataframe for the current year
  year_combined_df <- do.call(rbind, year_df_list)
  
  # Arrange columns in the desired order
  year_combined_df <- year_combined_df %>%
    select(x, y, Year, Month, pm2.5)
  
  # Append the combined dataframe for the current year to the list
  all_years_df_list[[as.character(year)]] <- year_combined_df
  
  # Export to CSV
  csv_filename <- paste0("pollution_", year, ".csv")
  write.csv(year_combined_df, file = csv_filename, row.names = FALSE)
}

# Combine the all_years_df_list into one tall dataframe for all years
combined_df <- do.call(rbind, all_years_df_list)

# Summarize
summary(combined_df)

levels(combined_df$Year)
levels(combined_df$Month)

# Export the combined_df to a CSV file
csv_file_path <- "Data Set/pollution_all.csv"
write_csv(combined_df, csv_file_path)

# Calculate overall average PM2.5 for each month and year
overall_avg <- combined_df %>%
  group_by(Year, Month) %>%
  summarize(Monthly_Avg_PM25 = mean(pm2.5, na.rm = TRUE))

# Filter data for years 2018 to 2021
filtered_data <- overall_avg %>% 
  filter(Year %in% c("2018", "2019", "2020", "2021"))

# Create a line plot for the overall average PM2.5
ggplot() +
  geom_line(data = filtered_data, aes(x = as.numeric(Month), y = Monthly_Avg_PM25, group = Year, color = as.factor(Year), linetype = as.factor(Year))) +
  labs(title = "Seasonality in Overall Average PM2.5",
       x = "Month",
       y = "Overall Average PM2.5") +
  scale_color_manual(values = c("2018" = "red", "2019" = "red", "2020" = "blue", "2021" = "blue")) +
  scale_linetype_manual(values = c("2018" = 2, "2019" = 1, "2020" = 1, "2021" = 2)) +
  theme_minimal()

# Set the function for raster plotting
plot_raster_with_boundary <- function(base_dir, file_prefix, jakarta_bbox, jakarta_gpkg, year_plot, month_plot) {
  # Format the month and year
  month_str_plot <- sprintf("%02d", month_plot)
  
  # Construct the file path
  file_path <- paste0(base_dir, file_prefix, year_plot, month_str_plot, "-", year_plot, month_str_plot, ".nc")
  
  # Read the NetCDF file and store the raster object
  raster_data <- raster(file_path, varname = "GWRPM25", band = 1)
  clipped_raster <- crop(raster_data, extent(jakarta_bbox))
  clipped_raster <- mask(clipped_raster, jakarta_gpkg)
  
  # Plot the raster
  plot(clipped_raster, main = paste("PM2.5 in Jakarta -", month_str_plot,"/", year_plot))
  
  # Overlay administrative boundaries on the plot
  plot(st_geometry(jakarta_gpkg), col = NA, border = "red", add = TRUE)
}

# Specify the year and month for plotting
year_plot <- 2021
month_plot <- 6
plot_raster_with_boundary(base_dir, file_prefix, jakarta_bbox, jakarta_gpkg, year_plot, month_plot)
