
library(waterfalls)
library(ggplot2)
library(ggalluvial)
library(dplyr)
library(tidyr)
library(officer)

# Read Data

monthly_2020 <- read.csv("Output/EU_monthly_2020.csv")
monthly_2021 <- read.csv("Output/EU_monthly_2021.csv")
monthly_2022 <- read.csv("Output/EU_monthly_2022.csv")
monthly_2023 <- read.csv("Output/EU_monthly_2023.csv")

data_month <- rbind(monthly_2020, monthly_2021, monthly_2022, monthly_2023)
rm(monthly_2020, monthly_2021, monthly_2022, monthly_2023)

# List of countries including EU aggregate
countries_list <- c("Italy", "Spain", "France", "Germany", "All EU countries")

end_year <- 2023
start_year <- end_year-1
end_month <- 12

# Function to create waterfall plot for a given set of countries
create_waterfall_plot <- function(data_input, countries_of_interest, title_suffix = "") {
  filtered_data <- data_input %>%
    filter(country == countries_of_interest & month <= end_month & (year == start_year | year == end_year)) %>% 
    group_by(year) %>% 
    summarise(
      gas_twh_sum = sum(gas_twh_sum, na.rm = TRUE),
      coal_tot_twh_sum = sum(coal_tot_twh_sum, na.rm = TRUE),
      oil_tot_twh_sum = sum(oil_tot_twh_sum, na.rm = TRUE),
      hydro_tot_twh_sum = sum(hydro_tot_twh_sum, na.rm = TRUE),
      nuclear_twh_sum = sum(nuclear_twh_sum, na.rm = TRUE),
      wind_tot_twh_sum = sum(wind_tot_twh_sum, na.rm = TRUE),
      solar_twh_sum = sum(solar_twh_sum, na.rm = TRUE),
      otherrenewable_tot_twh_sum = sum(otherrenewable_tot_twh_sum, na.rm = TRUE),
      other_twh_sum = sum(other_twh_sum, na.rm = TRUE),
      total_twh_sum = sum(total_twh_sum, na.rm = TRUE)
    ) %>% 
    arrange(year) %>% 
    subset(select = -year)
  
  waterfall_data <- filtered_data %>%
    mutate(across(everything(), ~. - lag(.))) %>% 
    slice(-1) %>% 
    t()
  
  rownames(waterfall_data) <- NULL
  
  group <- c("Gas", "Coal", "Oil", "Hydro",
             "Nuclear", "Wind", "Solar", "Other Renew",
             "Other Non-Renew")
  
  waterfall_df <- data.frame(x = as.factor(group), y = as.numeric(waterfall_data[1:9,])) %>% 
    arrange(y)
  
  # Add the total rows to the data frame
  waterfall_df <- rbind(
    data.frame(x = "Total 2022", y = filtered_data$total_twh_sum[1]),
    waterfall_df
  )
  
  # Add some padding to the limits for better visualization
  max_y <- max(cumsum(waterfall_df$y))
  min_y <- min(cumsum(waterfall_df$y))
  y_limits <- c(min_y - 0.3 * abs(min_y), max_y + 0.1 * abs(max_y))
  
  # Plot the waterfall chart
  waterfall_plot <- waterfall(
    .data = waterfall_df,
    rect_text_labels = round(waterfall_df$y, 1),
    rect_text_size = 1.5,
    put_rect_text_outside_when_value_below = 1 * (max(cumsum(waterfall_df$y)) -
                                                    min(cumsum(waterfall_df$y))),
    calc_total = TRUE,
    total_axis_text = "Total 2023",
    total_rect_text = round(waterfall_df$y, 2),
    total_rect_color = "lightblue",
    total_rect_border_color = "black",
    total_rect_text_color = "black",
    fill_by_sign = TRUE,
    draw_lines = TRUE,
    lines_anchors = c("right", "left"),
    linetype = "dashed",
    draw_axis.x = "behind",
    theme_text_family = "",
    print_plot = FALSE
  ) +
  # Add titles, labels, and captions
  labs(
    title = paste("Power Generation Change in", paste(countries_of_interest)),
    x = "Generation Source",
    y = "TWh",
    subtitle = paste("Changes in power generation source during whole period of", end_year, "compared to", start_year),
    caption = "Power data source: ENTSO-E Transparency Platform;
    Geothermal, biomass, waste, and marine are inlcuded in Other Renew category"
  ) +
    # Apply themes
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      plot.title = element_text(hjust = 0, size = 20),
      axis.title = element_text(size = 12), # Adjust axis title size
      axis.text.y = element_text(size = 11), # Adjust y-axis text size
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5)  # Add an outer box
    ) +
    coord_cartesian(ylim = y_limits)
  waterfall_plot
}

# Loop through each country and create waterfall plot
for (country in countries_list) {
  plot <- create_waterfall_plot(data_month, countries_of_interest = country)
  
  # Dynamically generate the file name based on the country
  file_name <- paste("Visualization/waterfall_plot_", country, "_", end_year, ".png", sep = "")
  
  # Export the ggplot to PNG with a 16:9 ratio and 25.4 cm width
  ggsave(file_name, plot = plot, width = 25.4, height = 14.29, units = "cm", dpi = 300)
}