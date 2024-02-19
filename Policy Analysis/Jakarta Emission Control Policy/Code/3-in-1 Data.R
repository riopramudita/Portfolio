# Install and load necessary packages
if (!require("readr")) install.packages("readr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("summarytools")) install.packages("summarytools")
if (!require("lubridate")) install.packages("lubridate")


library(readr)
library(dplyr)
library(ggplot2)
library(summarytools)
library(lubridate)


# Load the CSV data
data <- read_csv("Data/combined_v1_live.csv")


data <- data %>%
  mutate(
    dep_time_local = dmy_hms(dep_time_local),  # Parse the date and time
    dep_date = as.Date(dep_time_local),        # Create a separate date column
    dep_time = format(dep_time_local, "%H:%M:%S")  # Create a separate time column
  ) %>% 
  select(-dep_time_utc)

data$dep_time_local <- as.POSIXct(data$dep_time_local, format = "%Y-%m-%d %H:%M:%S")

# Extract the date and time components into separate columns
data$dep_time <- format(data$dep_time_local, format = "%H:%M")
summary(data)

odid_34 <- data %>% 
  filter(odid == 34)

ggplot(odid_34, aes(x = duration_bg)) +
  geom_histogram()

# Create a line graph
ggplot(odid_34, aes(x = dep_time_local, y = duration_bg)) +
  geom_line() +  # Add a line to the plot
  labs(x = "Departure Time", y = "Traffic Duration (minutes)") +  # Label the axes
  ggtitle("Traffic Duration Over Time")  # Add a title to the graph

odid_34_date <- odid_34 %>% 
  group_by(dep_date) %>% 
  mutate(count = n(),
         mean_duration_bg = mean(duration_bg))

ggplot(odid_34_date, aes(x = dep_date, y = count)) +
  geom_bar(stat = "identity") +  # Create a bar plot
  labs(x = "Departure Date", y = "Number of Observations") +  # Label the axes
  ggtitle("Number of Observations per Departure Date")  # Add a title to the graph

ggplot(odid_34_date, aes(x = duration_bg, y = duration_in_traffic_bg)) +
  geom_point() +  # Create a scatter plot
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "duration_bg", y = "duration_in_traffic_bg") +  # Label the axes
  ggtitle("")  # Add a title to the graph

odid_34_April29 <- odid_34 %>% 
  filter(dep_date == "2016-5-7")

# Create a line graph with two lines of different colors
ggplot(odid_34_April29, aes(x = dep_time_local)) +
  geom_line(aes(y = duration_in_traffic_bg, color = "Duration in Traffic Background")) +
  labs(x = "Departure time", y = "Duration") +
  ggtitle("Traffic congestion duration over time") +
  scale_color_manual(values = c("Duration Background" = "blue", "Duration in Traffic Background" = "red")) +
  theme(legend.position = "bottom") + # Move the legend to the bottom
  scale_x_time(breaks = scales::breaks_width("3 hours"), labels = scales::time_format("%H"))


# Data Summary
summary(odid_34_April29)

# Data Cleaning and Preprocessing (modify as needed)
# For example, handle missing values
# data <- data %>% drop_na()

# Data Visualization (modify as needed)
# Example: Create a histogram for a numeric variable
ggplot(data, aes(x = numeric_variable)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of Numeric Variable")

# Correlation Analysis
correlation_matrix <- cor(data[, sapply(data, is.numeric)], use = "complete.obs")

# Feature Engineering (add your feature engineering code here)

# Hypothesis Testing (add your hypothesis testing code here)

# Reporting
# Create an RMarkdown report or any other reporting method of your choice
