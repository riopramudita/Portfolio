# Load necessary libraries
library(ggplot2)
library(sf)
library(tidyr)
library(dplyr)
library(readr)
library(raster)
library(ncdf4)
library(rasterVis)
library(tidyverse)

# Read the Jakarta administrative boundary from the GeoPackage file using sf
jakarta_gpkg <- st_read("Data Set/Kabupaten-DKI-Jakarta.gpkg")
jakarta_gpkg <- jakarta_gpkg %>%
  filter(name != "Kepulauan Seribu")

# Specify the bounding box coordinates
jakarta_bbox <- c(xmin = 106.65, xmax = 107, ymin = -6.4, ymax = -6.05)

# Convert the combined_df to sf object
pollution_all <- read_csv("Data Set/PM/pollution_all.csv") 

# Define custom color gradient
custom_colors <- c("#0505ff", "#ab00b4", "#de216c", "#ff4314")

# Define a function to generate plots for each month and year
plot_pm25_levels <- function(Year, Month) {
  pollution_all_month <- pollution_all %>% 
    filter(year == as.character(Year), month == as.character(Month))
  
  combined_sf_month <- st_as_sf(pollution_all_month, coords = c("x", "y"), crs = st_crs(jakarta_gpkg))
  
  # Plot the Jakarta map
  ggplot() +
    geom_sf(data = jakarta_gpkg, fill = "black", color = "green")  +  # Add this line with cropped jakarta_gpkg as the geometry
    geom_sf(data = combined_sf_month, aes(color = pm2.5), na.rm = TRUE) +
    scale_color_gradient(name = "PM 2.5", low = custom_colors[1], high = custom_colors[4], limits = c(10, 40), na.value = "grey") +
    labs(title = paste("PM2.5 Levels in Jakarta -", format(as.Date(paste(Year, Month, "01", sep = "-")), "%B %Y")),
         subtitle = paste("Data of PM2.5 in", format(as.Date(paste(Year, Month, "01", sep = "-")), "%B %Y")),
         caption = "Source: WUSTL") +
    theme_minimal() +
    coord_sf(xlim = c(jakarta_bbox["xmin"], jakarta_bbox["xmax"]), 
             ylim = c(jakarta_bbox["ymin"], jakarta_bbox["ymax"]))
}

# Iterate through years and months and save plots as PNG
for (year in 2000:2010) {
  for (month in 1:12) {
    plot_filename <- sprintf("Monthly Graph/PMLevel_%04d_%02d.png", year, month)
    ggsave(filename = plot_filename, plot = plot_pm25_levels(year, month), width = 8, height = 6)
  }
}
