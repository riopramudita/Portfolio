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
library(scales)
library(viridis)

# Load the data from the RData file
load("Data Set/PM/pollution_all.RData")
pollution_all <- combined_df

pollution_all$year <- as.factor(pollution_all$year)
pollution_all$month <- as.factor(pollution_all$month)
pollution_all$x <- as.factor(pollution_all$x)
pollution_all$y <- as.factor(pollution_all$y)
levels(pollution_all$x)
levels(pollution_all$y)
str(pollution_all)
summary(pollution_all)

pollution_all_coord <- pollution_all %>% 
  filter(year==2000 & month==1) %>% 
  select(x, y)

csv_file_path <- "Data Set/pollution_coord.csv"
write_csv(pollution_all_coord, csv_file_path)