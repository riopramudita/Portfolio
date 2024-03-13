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
## Rainfall data EDA
#-------------------------------------------------------------------------------

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

# Custom grey color palette
grey_palette <- c("#DCDCDC")

# Create a vector of colors
year_colors <- setNames(rep(grey_palette, length.out = 22), as.character(2018:2021))
year_colors["Overall Average"] <- "red"

rain_seasonality_graph <- ggplot(rain_seasonality, aes(x = as.factor(month), y = rainfall,
                                                       group = interaction(year, as.factor(year)),
                                                       color = as.factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = year_colors) +
  labs(title = expression("Monthly Rainfall Seasonality in 2018 to 2021 Period"),
       x = "Month",
       y = "Rainafall rate (mm)",
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
  ) +
  annotate("text", x = Inf, y = Inf, label = "Data Source: Badan Pusat Statistik Indonesia", 
           hjust = 1.1, vjust = 1.1, size = 3, angle = 0, color = "grey50")

rain_seasonality_graph
