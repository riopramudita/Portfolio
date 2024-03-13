library(raster)
library(ncdf4)
library(rasterVis)
library(sf)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(sandwich)
library(lmtest)

getwd()

# Set the base directory and file prefix for monthly measurement
base_dir <- "/Users/rioprmdt/Library/CloudStorage/GoogleDrive-rpk2133@columbia.edu/My Drive/1. Columbia/1. Academics/R/Final Project/Data Set/WUSTL/Monthly/"
file_prefix <- "V5GL03.HybridPM25.Asia."

# Set the base directory and file prefix for uncertainty
#base_dir_unc <- "/Users/rioprmdt/Library/CloudStorage/GoogleDrive-rpk2133@columbia.edu/My Drive/1. Columbia/1. Academics/R/Final Project/Data Set/WUSTL/Uncertainty/"
#file_prefix_unc <- "V5GL03.HybridPM25E.Asia."

# Read the Jakarta administrative boundary from the GeoPackage file
jakarta_gpkg <- st_read("Data Set/Kabupaten-DKI-Jakarta.gpkg")
jakarta_gpkg <- jakarta_gpkg %>%
 filter(name != "Kepulauan Seribu")

# Specify the bounding box for Jakarta (adjust coordinates accordingly)
jakarta_bbox <- c(xmin = 106.65, xmax = 107, ymin = -6.4, ymax = -6.05)

# Initialize an empty list to store data frames for each year
all_years_df_list <- list()

# Loop through years from 2011 to 2021
for (year in 2000:2021) {
  year_df_list <- list()
  
  for (month in 1:12) {
    month_str <- sprintf("%02d", month)
    file_path <- paste0(base_dir, file_prefix, year, month_str, "-", year, month_str, ".nc")
    #file_path_unc <- paste0(base_dir_unc, file_prefix_unc, year, month_str, "-", year, month_str, ".nc")
    
    # Read PM2.5 data
    raster_data <- raster(file_path)
    clipped_raster <- crop(raster_data, extent(jakarta_bbox))
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
    raster_df$year <- as.factor(year)
    raster_df$month <- as.factor(month)
    year_df_list[[month]] <- raster_df
    
    # Export to CSV
    csv_filename <- paste0("Data Set/PM/pollution_", year,"_",month, ".csv")
    write.csv(raster_df, file = csv_filename, row.names = FALSE)
  }
  
  # Combine the year_df_list into one tall dataframe for the current year
  year_combined_df <- do.call(rbind, year_df_list)
  
  # Arrange columns in the desired order
  year_combined_df <- year_combined_df %>%
    dplyr::select(x, y, year, month, `pm2.5`)
  
  # Append the combined dataframe for the current year to the list
  all_years_df_list[[as.character(year)]] <- year_combined_df
  
  # Export to CSV
  csv_filename <- paste0("Data Set/PM/pollution_", year, ".csv")
  write.csv(year_combined_df, file = csv_filename, row.names = FALSE)
}

# Combine the all_years_df_list into one tall dataframe for all years
combined_df <- do.call(rbind, all_years_df_list)

# Summarize
summary(combined_df)

levels(combined_df$year)
levels(combined_df$month)

# Export the combined_df to a RData file
file_path <- "Data Set/PM/pollution_all.RData"
save(combined_df, file = file_path)

# --------------------------------------------------------------

