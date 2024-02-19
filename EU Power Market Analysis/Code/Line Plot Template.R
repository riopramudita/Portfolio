# Install and load the necessary packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Read Data

monthly_2020 <- read.csv("Output/EU_monthly_2020.csv")
monthly_2021 <- read.csv("Output/EU_monthly_2021.csv")
monthly_2022 <- read.csv("Output/EU_monthly_2022.csv")
monthly_2023 <- read.csv("Output/EU_monthly_2023.csv")

data_month <- rbind(monthly_2020, monthly_2021, monthly_2022, monthly_2023)
rm(monthly_2020, monthly_2021, monthly_2022, monthly_2023)
data_month$month <- factor(data_month$month, levels = 1:12, labels = month.name)

# Specify the countries you want to include in the plots
selected_countries <- c("Italy", "Spain", "France", "Germany", "All EU countries")

# Define consistent colors for each technology
technology_colors <- c("gas_twh_sum" = "darkgrey",
                       "oil_tot_twh_sum" = "brown",
                       "coal_tot_twh_sum" = "black",
                       "solar_twh_sum" = "orange",
                       "wind_tot_twh_sum" = "darkgreen",
                       "hydro_tot_twh_sum" = "blue",
                       "nuclear_twh_sum" = "magenta",
                       "otherrenewable_tot_twh_sum" = "lightgreen",
                       "other_twh_sum" = "purple",
                       "total_twh_sum" = "red")

# Define legend labels
legend_labels <- c("gas_twh_sum" = "Gas",
                   "oil_tot_twh_sum" = "Oil",
                   "coal_tot_twh_sum" = "Coal",
                   "solar_twh_sum" = "Solar",
                   "wind_tot_twh_sum" = "Wind",
                   "hydro_tot_twh_sum" = "Hydro",
                   "nuclear_twh_sum" = "Nuclear",
                   "otherrenewable_tot_twh_sum" = "Other Renew",
                   "other_twh_sum" = "Other Non-Renew",
                   "total_twh_sum" = "Total")

# Create a function to map country code to technologies
get_technologies <- function(country_code) {
  switch(country_code,
         "Italy" = c("gas_twh_sum", "hydro_tot_twh_sum"),
         "Spain" = c("gas_twh_sum", "solar_twh_sum"),
         "France" = c("nuclear_twh_sum", "gas_twh_sum"),
         "Germany" = c("coal_tot_twh_sum", "nuclear_twh_sum", "gas_twh_sum"),
         "All EU countries" = c("solar_twh_sum", "wind_tot_twh_sum", "hydro_tot_twh_sum"),
         stop("Invalid country code")
  )
}

# Create separate plots for each country
for (countries in selected_countries) {
  # Filter data for the current country
  country_data <- data_month %>% 
    filter(year == 2022 | year == 2023) %>% 
    filter(country == countries) %>% 
    mutate(code = as.factor(code),
           country = as.factor(country),
           year = as.factor(year))
  
  # Get the relevant technologies for the current countries
  selected_technologies <- get_technologies(countries)
  
  # Create the line graph using ggplot2
  line_plot <- ggplot(country_data, aes(x = month, group = year)) +
    lapply(selected_technologies, function(tech) {
      geom_line(aes(y = !!sym(tech), color = tech, linetype = factor(year)), linewidth = 1)
    }) +
    ggtitle(paste("Monthly Trend of Power Generation in", countries)) +
    labs(subtitle = "Different colors represent various energy sources; solid lines indicate 2023 trends, dashed lines for 2022.",  
         caption = "Power data source: ENTSO-E Transparency Platform") +  
    xlab("Month") + ylab("Electricity Generation (TWh)") +
    theme_minimal() +
    scale_color_manual(values = technology_colors[selected_technologies],
                       labels = legend_labels) +
    scale_linetype_manual(values = c("2022" = "dashed", "2023" = "solid")) +
    scale_x_discrete(limits = month.name, expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    theme(legend.position = "bottom", legend.box = "horizontal",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          plot.title = element_text(hjust = 0, size = 20),
          axis.title = element_text(size = 12),
          axis.text.y = element_text(size = 11),
          panel.grid.major.x = element_line(color="grey", linewidth = 0.5),
          panel.grid.major.y = element_line(color="grey", linewidth = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          legend.key.size = unit(1, "cm"))
  
  # Dynamically generate the file name based on the country
  file_name <- paste("Visualization/month_trend_", countries, ".png", sep = "")
  
  # Export the ggplot to PNG with a 16:9 ratio and 25.4 cm width
  ggsave(file_name, plot = line_plot, width = 25.4, height = 14.29, units = "cm", dpi = 300)
}