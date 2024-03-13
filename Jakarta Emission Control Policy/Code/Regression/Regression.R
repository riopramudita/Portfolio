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

#-------------------------------------------------------------------------------
## Load Data
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

# Read the Excel file
rainfall <- read_excel("Data Set/rainfall.xlsx") %>% 
  mutate(rainfall = as.numeric(rainfall))

# Calculate monthly averages
monthly_rainfall <- rainfall %>%
  group_by(month) %>%
  summarise(rainfall = mean(rainfall, na.rm = TRUE)) %>% 
  mutate(year = "Overall Average") %>% 
  dplyr::select(year, month, rainfall)

rain_seasonality <- rbind(rainfall, monthly_rainfall)

load("Data Set/PM/pollution_all.RData")
pollution_all <- combined_df
pollution_all$month <- as.factor(pollution_all$month)
pollution_all$x <- as.factor(pollution_all$x)
pollution_all$y <- as.factor(pollution_all$y)

#-------------------------------------------------------------------------------
## EDA
#-------------------------------------------------------------------------------

# Calculate overall average PM2.5 for each month and year
overall_avg <- pollution_all %>%
  dplyr::group_by(year, month) %>%
  dplyr::summarize(Monthly_Avg_PM25 = mean(pm2.5, na.rm = TRUE))

# Calculate overall average
overall_avg_all <- overall_avg %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(Monthly_Avg_PM25 = mean(Monthly_Avg_PM25, na.rm = TRUE)) %>% 
  dplyr::mutate(year = as.factor("Monthly Average")) %>% 
  dplyr::select(year, month, Monthly_Avg_PM25)

overall_avg <- overall_avg %>% 
  mutate(year = as.factor(year))

# Concatenate the two datasets
combined_avg <- rbind(overall_avg, overall_avg_all %>%
                        mutate(year = "Overall Average"))

# Custom grey color palette
grey_palette <- c("#DCDCDC")

# Create a vector of colors
year_colors <- setNames(rep(grey_palette, length.out = 22), as.character(2000:2021))
year_colors["Overall Average"] <- "red"

EDA_seasonality <- ggplot(combined_avg, aes(x = as.factor(month), y = Monthly_Avg_PM25,
                                            group = interaction(year, as.factor(year)),
                                            color = as.factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = year_colors) +  # Assign colors manually
  labs(title = expression("Seasonality in Monthly PM"[2.5]*" for 2000 to 2021 Period"),
       x = "Month",
       y = expression("PM"[2.5]*" (μg/m³)"),
       color = "Year") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 10),
    panel.grid.major = element_line(size = 0.2, color = "gray90"),
    axis.text.x = element_text(angle = 0, hjust = 1),
    legend.key = element_blank()  # Remove legend for grey colors
  )
EDA_seasonality

# Specify the full path to the "Graph" folder
graph_folder <- "Graph"
# Save the ggplot as a PNG file in the "Graph" folder
ggsave(file.path(graph_folder, "EDA_seasonality.png"), plot = EDA_seasonality, width = 6, height = 4, units = "in", dpi = 300)

#-------------------------------------------------------------------------------
# EDA 2
#-------------------------------------------------------------------------------
combined_df$year <- as.factor(combined_df$year)
combined_df$month <- as.factor(combined_df$month)
combined_df$x <- as.factor(combined_df$x)
combined_df$y <- as.factor(combined_df$y)
combined_df <- combined_df %>%
  mutate(xy = as.factor(paste(x, y, sep = ",")))

# Filter to only consider Januari 2018 until March 2020
combined_df_date <- combined_df %>%
  mutate(date = make_date(as.numeric(as.character(year)), as.numeric(as.character(month)), 1)) %>%   # Create a date variable
  filter(date >= make_date(2018, 1, 1) & date <= make_date(2020, 3, 31))  # Filter the date range

# Linear regression to check the significance of month variable in pm2.5
EDA2 <- lm(pm2.5 ~ month, data = combined_df_date)
robust_se2 <- coeftest(EDA2, vcov = vcovHC(EDA2, type = "HC1"))
summary(EDA2)
robust_se2

# Calculate residuals
combined_df_date_res <- combined_df_date %>%
  mutate(residuals = resid(EDA2))

combined_df_date_res <- combined_df_date_res %>% 
  group_by(date) %>% 
  mutate(date_mean = mean(residuals, na.rm = TRUE))

# Create the scatter plot
ggplot(combined_df_date_res, aes(x = date, y = residuals)) +
  geom_point() + # Add points
  geom_line(aes(x = date, y = date_mean))
  labs(title = "Spread of Residuals Over Time",
       x = "Time (Year-Month)",
       y = "Residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------------------
# EDA 3
#-------------------------------------------------------------------------------
  rainfall <- rainfall %>% 
    mutate(month = as.factor(month),
           year = as.factor(year))
  
  combined_df_date_rain <- left_join(combined_df_date, rainfall, 
                                     by = c("year", "month"))
  
# Linear regression to check the significance of month variable in pm2.5
  EDA3 <- lm(pm2.5 ~ month + rainfall, data = combined_df_date_rain)
  robust_se3 <- coeftest(EDA3, vcov = vcovHC(EDA3, type = "HC1"))
  summary(EDA3)
  robust_se3
  
# Linear regression to check the significance of month variable in pm2.5
  EDA3_month <- lm(pm2.5 ~ month, data = combined_df_date_rain)
  robust_se3_month <- coeftest(EDA3_month, vcov = vcovHC(EDA3_month, type = "HC1"))
  summary(EDA3_month)
  robust_se3_month

# Calculate residuals
combined_df_date_rain_res <- combined_df_date_rain %>%
  mutate(residuals = resid(EDA3),
         residuals_month = resid(EDA3_month))

combined_df_date_rain_res <- combined_df_date_rain_res %>% 
  group_by(date) %>% 
  mutate(date_mean = mean(residuals, na.rm = TRUE))

# Create the scatter plot
ggplot(combined_df_date_rain_res, aes(x = date, y = residuals)) +
  geom_point() + # Add points
  geom_line(aes(x = date, y = date_mean)) +
  labs(title = "Spread of Residuals Over Time",
       x = "Time (Year-Month)",
       y = "Residuals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Improve x-axis label readability

#-------------------------------------------------------------------------------
# EDA 4 Panel Data for every road
#-------------------------------------------------------------------------------

# Left join filt_map to combined_data by x and y
road_panel_data <- left_join(combined_df_date_rain_res, near_analysis, by = c("x", "y"))

road_panel_data <- road_panel_data %>% 
  select(-OID_, -fid_1, -fid_2, -fid_3, -fid_4, -fid_5, -fid_6, -fid_7, -fid_8, -fid_9, -fid_0,
         -ang_1, -ang_2, -ang_3, -ang_4, -ang_5, -ang_6, -ang_7, -ang_8, -ang_9, -ang_0)

         #-dist_1, -dist_2, -dist_3, -dist_4, -dist_5, -dist_6, -dist_7, -dist_8, -dist_9, -dist_0)

road_panel_data %>%
  ungroup() %>% 
  count(nearest_road)

EDA4 <- lm(pm2.5 ~ month + rainfall + dist_1 + dist_2 +
             dist_3 + dist_4 + dist_5 + dist_6 + dist_7 +
             dist_8 + dist_9 + dist_0, data = road_panel_data)
robust_se4 <- coeftest(EDA4, vcov = vcovHC(EDA4, type = "HC1"))
summary(EDA4)
robust_se4

road_panel_data <- road_panel_data %>% 
  group_by(nearest_road, date) %>% 
  mutate(avg_date_road = mean(residuals))

# Filter the data where min_dist is <= 1000
filtered_data <- road_panel_data %>%
  filter(min_dist <= 1000,
         !nearest_road=="0_not_gage")

# Create the line graph
ggplot(filtered_data, aes(x = date, y = pm2.5, group = nearest_road, color = nearest_road)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Average Residuals by Date for Each Nearest Road",
    x = "Date",
    y = "Average Residuals",
    color = "Nearest Road"
  ) 

#-------------------------------------------------------------------------------
# EDA 5 Scatter plot residuals and nearest_dist
#-------------------------------------------------------------------------------

# Create the scatter plot
ggplot(road_panel_data, aes(x = min_dist, y = residuals)) +
  geom_point() +  # Add points
  theme_minimal() +  # Optional: Use a minimal theme
  labs(
    title = "Scatter Plot of Min Distance vs Residuals",
    x = "Distance to the nearest road",
    y = "Residuals PM 2.5"
  ) +
  geom_smooth(method = "lm", color = "blue", se = TRUE)

EDA5 <- lm(residuals ~ min_dist, data = road_panel_data)
robust_se5 <- coeftest(EDA5, vcov = vcovHC(EDA5, type = "HC1"))
summary(EDA5)
robust_se5

# Create the scatter plot with the filtered data
ggplot(filtered_data, aes(x = min_dist, y = residuals)) +
  geom_point() + 
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Scatter Plot of Min Distance vs Residuals (Distance <= 1000m)",
    x = "Distance to the nearest road (<= 1000m)",
    y = "Residuals PM 2.5"
  )

EDA5b <- lm(residuals ~ min_dist, data = filtered_data)
robust_se5b <- coeftest(EDA5b, vcov = vcovHC(EDA5b, type = "HC1"))
summary(EDA5b)
robust_se5b


#-------------------------------------------------------------------------------
# Clean data
#-------------------------------------------------------------------------------
prog_data <- road_panel_data %>%
  dplyr::select(x, y, year, month, pm2.5, nearest_road, min_dist, rainfall) %>% 
  filter(!is.na(nearest_road)) %>% 
  mutate(gage = ifelse(nearest_road == "0_not_gage", 0, 1),
         year_gage = case_when(nearest_road == "1_gajah_mada" ~ 2019,
                               nearest_road == "2_sudirman" ~ 2018,
                               nearest_road == "3_fatmawati" ~ 2019,
                               nearest_road == "4_tomang" ~ 2019,
                               nearest_road == "5_gatsu_haryono" ~ 2018,
                               nearest_road == "6_rasuna_said" ~ 2018,
                               nearest_road == "7_ahmad_yani" ~ 2018,
                               nearest_road == "8_pramuka" ~ 2019,
                               nearest_road == "9_salemba" ~ 2019,
                               nearest_road == "0_not_gage" ~ 0,
                              TRUE ~ NA_integer_),
         month_gage = case_when(nearest_road == "1_gajah_mada" ~ 9,
                                nearest_road == "2_sudirman" ~ 8,
                                nearest_road == "3_fatmawati" ~ 9,
                                nearest_road == "4_tomang" ~ 9,
                                nearest_road == "5_gatsu_haryono" ~ 8,
                                nearest_road == "6_rasuna_said" ~ 8,
                                nearest_road == "7_ahmad_yani" ~ 8,
                                nearest_road == "8_pramuka" ~ 9,
                                nearest_road == "9_salemba" ~ 9,
                                nearest_road == "0_not_gage" ~ 0,
                               TRUE ~ NA_integer_),
         od = ifelse(gage == 1, "Odd-even", "No Odd-even")
         )

# Assign event study time for control group
control_obs <- prog_data %>% 
  filter(nearest_road == "0_not_gage") %>% 
  filter(date >= make_date(2018, 3, 1) & date <= make_date(2020, 3, 31)) %>% 
  mutate(year_gage = ifelse(year == 2018, 2018, 2019),
         month_gage = ifelse(year_gage == 2018, 8, 9),
         year_dif = as.integer(year) - as.integer(year_gage) +1999,
         month_dif = as.integer(month) - as.integer(month_gage),
         tot_month_dif = year_dif * 12 + month_dif)
  
str(treat_obs)


# Assign event study time for treatment group
treat_obs <- prog_data %>% 
  filter(!nearest_road == "0_not_gage") %>% 
  filter(date >= make_date(2018, 3, 1) & date <= make_date(2020, 3, 31)) %>% 
  mutate(
    year_dif = as.integer(year) - as.integer(year_gage) + 1999, 
    month_dif = as.integer(month) - as.integer(month_gage),
    tot_month_dif = year_dif * 12 + month_dif
  )

# Bind the control and treatment
combined_data <- bind_rows(control_obs, treat_obs)

# Adding the road width
road_status <- read.csv(file="Data Set/status_ruas_jalan.csv", sep=";", header=TRUE) %>% 
  mutate(nearest_road = as.factor(nearest_road)) %>% 
  select(-year_gage, -route, -road)

combined_data <- left_join(combined_data, road_status, by = "nearest_road")

# Adding the holidays and mudik
holidays <- read.csv(file="Data Set/holidays.csv", header=TRUE) %>% 
  mutate( month = as.factor(month),
          year = as.factor(year))
combined_data <- left_join(combined_data, holidays, by = c("year", "month") )

# Filter month difference
event_data <- combined_data %>% 
  filter(tot_month_dif < 7,
         tot_month_dif > -7) %>% 
  mutate(od = ifelse(gage == 1, "Odd-even", "No Odd-even"),
         log_pm2.5 = log(pm2.5))


# Convert y$year to numeric
rainfall$year <- as.factor(rainfall$year)
event_data$month <- as.factor(as.character(event_data$month))

# Perform the left join
event_data <- left_join(event_data, rainfall, by = c("year", "month"))

event_data <- event_data %>% 
  mutate(rainfall = as.numeric(rainfall.y)) %>% 
  select(-rainfall.x, -rainfall.y)

# Check for NAs in 'rainfall' and replace them with 0 if needed
event_data$rainfall[is.na(event_data$rainfall)] <- 0

# Convert 'rainfall' to numeric
event_data$rainfall <- as.numeric(event_data$rainfall)


#Panel data looks like this
ggplot(event_data, aes(x = tot_month_dif, y = pm2.5, group = od)) +
  geom_smooth(aes(group=gage, color = od)) +
  geom_point(aes(group=gage, color = od)) +
  geom_vline(aes(xintercept = -1)) +
  labs(
    x = "Months Since Odd-Even Policy",
    y = "Pollution PM 2.5 (microgram/m3)"
  ) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.8, 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, color = "black")
  )

#boxplot

# Assuming event_data contains columns: tot_month_dif, pm2.5, od, lower_bound, upper_bound
# lower_bound and upper_bound represent the range for each observation


# Specify the full path to the "Graph" folder
graph_folder <- "Graph"
# Save the ggplot as a PNG file in the "Graph" folder
ggsave(file.path(graph_folder, "panel_data.png"), plot = panel_data, width = 6, height = 4, units = "in", dpi = 300)


#-------------------------------------------------------------------------------
## Empirical strategy
#-------------------------------------------------------------------------------

# Estimate the model
fe0 <- feols(
  pm2.5 ~ od + nearest_road + month*year,
  data = prog_data
)

# Display results with robust SEs
res_fe0 <- coeftest(fe0, vcov = vcovHC(fe0, type = "HC1"))

res_fe0


#Event Study withouth rainfall
fe1 <- feols(
  pm2.5 ~ od + nearest_road + month*year,
  data = event_data
)

# Display results with robust SEs
res_fe1 <- coeftest(fe1, vcov = vcovHC(fe1, type = "HC1"))

res_fe1


#Event Study with rainfall
fe2 <- feols(
  pm2.5 ~ od + nearest_road + month*year + rainfall,
  data = event_data
)

# Display results with robust SEs
res_fe2 <- coeftest(fe2, vcov = vcovHC(fe2, type = "HC1"))

res_fe2

fe2 <- feols(
  pm2.5 ~ od + tot_month_dif + nearest_road + month + rainfall,
  data = event_data
)

# Does not show anything on OD because the effect of OD is affected a lot with the road and street


# Event Study
# Estimate fixed-effects model
fe3 <- feols(
  pm2.5 ~ i(factor(tot_month_dif), gage, ref = -1) + nearest_road + month*year + rainfall,
  data = event_data
)


# Display results with robust SEs using the correct model (fe2)
res_fe3 <- coeftest(fe3, vcov = vcovHC(fe3, type = "HC1"))

res_fe3

# Plot event study
iplot(
  fe3,
  pt.join = TRUE,
  ci.join = TRUE,
  ci.lwd = 0,
  grid.par = list(vert = FALSE),
  drop = c("-6", "6"),
  main = "Pollution and Odd-even Policy",
  ylab = "Pollution PM 2.5",
  xlab = "Months since Odd-even Policy implementation"
)


# Table of results
stargazer(res_fe0,res_fe1, res_fe2, type = 'latex', header = FALSE, title = 'Result of regression')

# Assuming fe2 includes robust standard errors
stargazer(res_fe2, type = 'latex', header = FALSE, title = 'Model without Robust SEs')
