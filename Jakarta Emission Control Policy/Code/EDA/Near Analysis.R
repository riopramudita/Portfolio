# Load libraries
library(tidyverse)
library(weights)
library(lmtest)
library(sandwich)
library(knitr)
library(sf)
library(readr)
library(dplyr)
library(fixest)
library(stargazer)
library(readxl)

getwd()

#-------------------------------------------------------------------------------
## Near Analysis EDA
#-------------------------------------------------------------------------------

near_analysis <- read.csv("Data Set/Near Analysis/Near_Analysis.csv")
str(near_analysis)

near_analysis$x <- as.factor(near_analysis$x)
near_analysis$y <- as.factor(near_analysis$y)

str(near_analysis)
levels(near_analysis$x)
levels(near_analysis$y)

# Assuming 'near_analysis' is your dataframe
# Extracting the relevant distance columns
dist_columns <- near_analysis[, grepl("dist_", names(near_analysis))]

# Finding the minimum distance for each observation and the corresponding district
min_dist <- apply(dist_columns, 1, min)  # Minimum distance for each row
nearest_dist_index <- apply(dist_columns, 1, which.min)  # Index of the nearest district

# Creating a new column with the nearest district
near_analysis$nearest_dist <- names(dist_columns)[nearest_dist_index]
near_analysis$min_dist <- min_dist

# Create a named vector for name replacement
name_replacement <- c(
  dist_1 = "1_gajah_mada",
  dist_2 = "2_sudirman",
  dist_3 = "3_fatmawati",
  dist_4 = "4_tomang",
  dist_5 = "5_gatsu_haryono",
  dist_6 = "6_rasuna_said",
  dist_7 = "7_ahmad_yani",
  dist_8 = "8_pramuka",
  dist_9 = "9_salemba",
  dist_0 = "0_not_gage"
)

# Replace the names in the nearest_dist column
near_analysis <- near_analysis %>%
  mutate(nearest_dist = name_replacement[nearest_dist])

# Create the nearest_road column with conditional assignment
near_analysis$nearest_road <- ifelse(near_analysis$min_dist < 1000, near_analysis$nearest_dist, NA)
near_analysis$nearest_road <- as.factor(near_analysis$nearest_road)

near_analysis %>%
  count(nearest_road)

summary(near_analysis)
