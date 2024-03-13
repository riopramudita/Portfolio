library(raster)
library(ncdf4)
library(rasterVis)
library(sf)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)

# Load the data from the RData file
load("Data Set/PM/pollution_all.RData")

# Read the Jakarta administrative boundary from the GeoPackage file
jakarta_gpkg <- st_read("Data Set/DKI Jakarta/Kabupaten-DKI-Jakarta.gpkg")

# Specify the bounding box for Jakarta (adjust coordinates accordingly)
jakarta_bbox <- c(xmin = 106.65, xmax = 107, ymin = -6.4, ymax = -6.05)

# Calculate overall average PM2.5 for each month and year
overall_avg_yrmnth <- combined_df %>%
  group_by(year, month) %>%
  summarize(monthly_avg_PM25 = mean(pm2.5, na.rm = TRUE))

# Filter data for years 2018 to 2021
filtered_data <- overall_avg_yrmnth %>% filter(year %in% c("2018", "2019", "2020", "2021"))

# Create a line plot for the overall average PM2.5
ggplot() +
  geom_line(data = filtered_data, aes(x = month, y = monthly_avg_PM25, group = year, color = year, linetype = year)) +
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
year_plot <- 2015
month_plot <- 1
plot_raster_with_boundary(base_dir, file_prefix, jakarta_bbox, jakarta_gpkg, year_plot, month_plot)
