# Install and load necessary packages
# install.packages(c("ggplot2", "dplyr"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Load the dataset
school_data <- read.csv("SchoolJakarta_Kota.csv")

# Filter out observations without data in the "nama_kelurahan" column
school_data_filtered <- school_data %>%
  filter(!is.na(tahun))

# Convert "nama_kelurahan" to a factor variable
school_data_filtered$nama_kelurahan <- factor(school_data_filtered$nama_kelurahan)
school_data_filtered$nama_provinsi <- factor(school_data_filtered$nama_provinsi)
school_data_filtered$nama_kabupaten_kota <- factor(school_data_filtered$nama_kabupaten_kota)
school_data_filtered$nama_kecamatan <- factor(school_data_filtered$nama_kecamatan)

# Filter out observations without data in the "nama_kelurahan" column
school_data_filtered <- school_data_filtered %>%
  filter(!is.na(nama_kelurahan))

# Rename the column "jumlah_populasi_.jiwa.km2." to "population_communities"
school_data_filtered <- school_data_filtered %>%
  rename(population_communities = jumlah_populasi_.jiwa.km2.)
# Rename the column "jumlah_populasi_.jiwa.km2." to "pop_density"
school_data_filtered <- school_data_filtered %>%
  rename(pop_density = jumlah_kepadatan_.jiwa.km2.)

# Create a new column for distance group
school_data_filtered <- school_data_filtered %>%
  mutate(distance_group = case_when(
    NEAR_DIST <= 833 ~ "<=10 minutes",
    NEAR_DIST <= 1667 ~ "<=20 minutes",
    NEAR_DIST <= 2500 ~ "<=30 minutes",
    TRUE ~ "More than 30 minutes"
  ))
# Create a more visually appealing color palette
color_palette <- c("#bdffb9", "#ddd6a3", "#f2ab8e", "#ff7979")
color_palette_kabupaten <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

# Create a histogram colored by the distance group
histogram_plot <- ggplot(school_data_filtered, aes(x = NEAR_DIST, fill = distance_group)) +
  geom_histogram(binwidth = 50, alpha = 0.7, position = "identity", boundary = 0) +  # Add boundary for better appearance
  scale_fill_manual(values = color_palette) +
  labs(
       x = "Walking Distance (meters)",
       y = "Frequency",
       caption = "source: author’s calculation with raw data from openstreetmap.id"
       ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = scales::comma_format()) +
  labs(fill = "Walking Duration") +  # Change the legend label
  guides(fill = guide_legend(title = "Walking Duration"))  # Improve legend title

# Save plot as PNG
ggsave("walking_distance_histogram.png", histogram_plot, width = 8, height = 4, dpi = 300)

histogram_plot

# Create a vertical bar chart for each distance group showing the distribution of kecamatan
bar_chart_plot <- ggplot(school_data_filtered, aes(x = distance_group, fill = nama_kabupaten_kota)) +
  geom_bar(position = "dodge", alpha = 0.7, stat = "count") +
  scale_fill_manual(values = color_palette_kabupaten) +
  labs(
    x = "Distance Group",
    y = "Frequency",
    caption = "source: author’s calculation with raw data from openstreetmap.id, data.jakarta.go.id, and ArcGIS portal"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "District") +  # Change the legend label
  scale_y_continuous(labels = scales::comma_format()) +  # Format y-axis labels
  scale_x_discrete(labels = c("<=10 minutes", "<=20 minutes", "<=30 minutes", "More than 30 minutes")) +  # Improve x-axis labels
  geom_text(stat = "count", aes(label = paste0(round((..count.. / sum(..count..) * 100), 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 4) +  # Adjust text size
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Save plot as PNG
ggsave("kecamatan_distribution_chart.png", bar_chart_plot, width = 8, height = 5, dpi = 300)

bar_chart_plot

# Replace 'your_file.xlsx' with the actual path to your Excel file
file_path <- "data-jumlah-penduduk-provinsi-dki-jakarta-berdasarkan-kelompok-usia-dan-jenis-kelamin-tahun-2021.xlsx"

# Read the Excel file into a data frame
df <- read_excel(file_path)

summary(df)

df <- df %>% 
  mutate(jenis_kelamin=as.factor(jenis_kelamin),
         usia=as.factor(usia),
         nama_provinsi=as.factor(nama_provinsi),
         nama_kabupaten_kota=as.factor(nama_kabupaten_kota),
         nama_kecamatan=as.factor(nama_kecamatan),
         nama_kelurahan=as.factor(nama_kelurahan))

df_tot <- df %>% 
  group_by(nama_kelurahan, usia) %>% 
  mutate(tot_pop = sum(jumlah_penduduk)) %>% 
  filter(jenis_kelamin!="Perempuan")

df_tot$jenis_kelamin <- NULL
df_tot$jumlah_penduduk <- NULL

summary(df_tot)

# Pivot the data to wide format
wide_data <- df_tot %>%
  pivot_wider(
    names_from = "usia",
    values_from = "tot_pop"
  )

# Assuming your data frame is named 'wide_data' and you want to change the name of a column from 'old_column_name' to 'new_column_name'
colnames(wide_data)[colnames(wide_data) == "01-10"] <- "10-14"

wide_data <- wide_data %>%
  mutate(school_pop = `05-09` + `10-14` + `15-19`/2,
         tot_pop = `00-04` + `05-09` + `10-14` + `15-19` + `20-24` + `25-29` + `30-34` + `35-39` + `40-44` + `45-49`
         + `50-54` + `55-59` + `60-64`+ `65-69` + `70-74` + `75+`,
         school_perc = school_pop/tot_pop)

summary(wide_data)

summary(school_data_filtered)
school_data_filtered <- school_data_filtered %>% 
  mutate(nama_provinsi=as.factor(nama_provinsi),
         nama_kabupaten_kota=as.factor(nama_kabupaten_kota),
         nama_kecamatan=as.factor(nama_kecamatan),
         nama_kelurahan=as.factor(nama_kelurahan))

nlevels(school_data_filtered$nama_kelurahan)
nlevels(wide_data$nama_kelurahan)
nlevels(school_data_filtered$nama_kecamatan)
nlevels(wide_data$nama_kecamatan)
nlevels(school_data_filtered$nama_kabupaten_kota)
nlevels(wide_data$nama_kabupaten_kota)

wide_data <- wide_data %>% 
  select(nama_kelurahan, school_pop, tot_pop, school_perc)

# Assuming wide_data and school_data_filtered are your data frames
wide_data$nama_kelurahan <- tolower(trimws(wide_data$nama_kelurahan))
school_data_filtered$nama_kelurahan <- tolower(trimws(school_data_filtered$nama_kelurahan))

# Manual cleaning
wide_data$nama_kelurahan[wide_data$nama_kelurahan == "krendang"] <- "kerendang"
wide_data$nama_kelurahan[wide_data$nama_kelurahan == "balekambang"] <- "bale kambang"
wide_data$nama_kelurahan[wide_data$nama_kelurahan == "kampung rawa"] <- "kampung tengah"
wide_data$nama_kelurahan[wide_data$nama_kelurahan == "kramatjati"] <- "kramat jati"

wide_data$nama_kelurahan[wide_data$nama_kelurahan == "kalibaru"] <- "kali baru"
wide_data$nama_kelurahan[wide_data$nama_kelurahan == "pinangranti"] <- "pinang ranti"
wide_data$nama_kelurahan[wide_data$nama_kelurahan == "rawajati"] <- "rawa jati"
wide_data$nama_kelurahan[wide_data$nama_kelurahan == "jatipulo"] <- "jati pulo"

wide_data$nama_kelurahan[wide_data$nama_kelurahan == "palmeriam"] <- "pal meriam"

# Calculate the percentage of schools in each distance group
merged_data <- school_data_filtered %>%
  left_join(wide_data, by = "nama_kelurahan") 

temp_failed <- merged_data %>% 
  filter(is.na(school_pop))

unique(temp_failed$nama_kelurahan)
  
school_percentage_by_distance <- merged_data %>%   
  group_by(distance_group) %>%
  summarise(school_count = n()) %>%
  mutate(percentage = (school_count / sum(school_count)) * 100)

# Print the result
print(school_percentage_by_distance)

# Group data by communities and calculate total population and number of schools
school_data_pop <-merged_data %>%
  group_by(nama_kelurahan) %>%
  mutate(num_schools = n(),
         pop_per_school = school_pop / num_schools)

summary(school_data_pop)

school_data_pop$jenis_kelamin <- NULL
school_data_pop$jumlah_penduduk <- NULL

# Assuming school_data_pop is your data frame
school_accessibility_plot <- ggplot(school_data_pop, aes(x = NEAR_DIST, y = pop_per_school, color = nama_kabupaten_kota)) +
  geom_point(alpha = 0.5, size = 2) +  # Adjust color and point size
  labs(
    x = "Walking Distance (meters)",
    y = "Young Population per School",
    caption = "source: author’s calculation with raw data from openstreetmap.id, data.jakarta.go.id, and ArcGIS portal",
    color = "District"  # Improved legend title
  ) +
  scale_color_manual(values = color_palette_kabupaten) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::comma_format(), limits = c(0, 2000)) +
  scale_x_continuous(labels = scales::comma_format())

# Print or view the updated plot
print(school_accessibility_plot)

# Save plot as PNG
ggsave("school_accessibility_plot.png", school_accessibility_plot, width = 8, height = 5, dpi = 300)

# Filter schools with walking distance more than 20 minutes
school_data_pop_filtered <- school_data_pop %>%
  filter(NEAR_DIST > 1667)  # Assuming 20 minutes walking distance is more than 1667 meters

total_jakarta_population <- sum(df$jumlah_penduduk)

# Calculate the total young population served for schools with more than 20 minutes walking distance
total_young_population_over_20_min <- sum(school_data_pop_filtered$pop_per_school, na.rm = TRUE)

# Calculate the percentage
percentage_young_over_20_min <- (total_young_population_over_20_min / total_jakarta_population) * 100

# Print the result
cat("Percentage of young people walking more than 20 minutes compared to total Jakarta population:", percentage_young_over_20_min, "%\n")

walk_by_city <- school_data_pop %>% 
  group_by(nama_kabupaten_kota, distance_group) %>% 
  summarise(pop_served = sum(pop_per_school))

walk_by_city_graph <- ggplot(walk_by_city, aes(x = distance_group, y = pop_served, fill = nama_kabupaten_kota)) +
  geom_bar(position = "dodge", alpha = 0.7, stat = "identity") +  # Use stat = "identity" to plot the y values directly
  geom_text(stat = "identity", aes(label = paste0(round((pop_served / sum(pop_served)) * 100, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Add text labels for percentage
  scale_fill_manual(values = color_palette_kabupaten) +
  labs(
       x = "Distance Group",
       y = "Young Population",
       caption = "source: author’s calculation with raw data from openstreetmap.id, data.jakarta.go.id, and ArcGIS portal",
       fill = "District") +  # Corrected the labs() argument for fill
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::comma_format()) +  # Format y-axis labels
  scale_x_discrete(labels = c("<=10 minutes", "<=20 minutes", "<=30 minutes", "More than 30 minutes")) +  # Improve x-axis labels
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Save plot as PNG
ggsave("walk_by_city_graph.png", walk_by_city_graph, width = 8, height = 5, dpi = 300)

# Print or view the updated plot
print(walk_by_city_graph)
