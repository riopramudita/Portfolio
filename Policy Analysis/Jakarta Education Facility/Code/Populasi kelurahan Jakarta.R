# Install and load necessary packages
# install.packages(c("ggplot2", "dplyr"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Replace 'your_file.xlsx' with the actual path to your Excel file
file_path <- "data-jumlah-penduduk-provinsi-dki-jakarta-berdasarkan-kelompok-usia-dan-jenis-kelamin-tahun-2021.xlsx"

# Read the Excel file into a data frame
df <- read_excel(file_path)

df <- df %>% 
  mutate(jenis_kelamin=as.factor(jenis_kelamin),
         usia=as.factor(usia),
         nama_provinsi=as.factor(nama_provinsi),
         nama_kabupaten_kota=as.factor(nama_kabupaten_kota),
         nama_kecamatan=as.factor(nama_kecamatan),
         nama_kelurahan=as.factor(nama_kelurahan))

summary(df)

df_tot <- df %>% 
  group_by(nama_kelurahan, usia) %>% 
  mutate(tot_pop = sum(jumlah_penduduk)) %>% 
  filter(jenis_kelamin!="Perempuan")

df_tot$jenis_kelamin <- NULL
df_tot$jumlah_penduduk <- NULL

wide_data <- df_tot %>%
  pivot_wider(
    names_from = "usia",
    values_from = "tot_pop")

colnames(wide_data)[colnames(wide_data) == "01-10"] <- "10-14"

summary(wide_data)

wide_data <- wide_data %>%
  mutate(age_under20 = `00-04` + `05-09` + `10-14` + `15-19`,
         total_pop = `00-04` + `05-09` + `10-14` + `15-19` + `20-24` + `25-29` + `30-34` + `35-39` + `40-44` + `45-49`
         + `50-54` + `55-59` + `60-64`+ `65-69` + `70-74` + `75+`,
         age_above60 = `60-64`+ `65-69` + `70-74` + `75+`,
         age_productive = total_pop - age_under20 - age_above60,
         prod_age_perc = age_productive/total_pop)

summary(wide_data)

populasi_kelurahan <- wide_data %>% 
  select(nama_kelurahan, total_pop, prod_age_perc, age_under20, age_productive, age_above60)

summary(populasi_kelurahan)

populasi_kelurahan <- populasi_kelurahan %>% 
  mutate(nama_kelurahan=as.factor(nama_kelurahan))

# Convert the factor variable to character
populasi_kelurahan$nama_kelurahan <- as.character(populasi_kelurahan$nama_kelurahan)

summary (populasi_kelurahan)
# Manual cleaning
populasi_kelurahan$nama_kelurahan[populasi_kelurahan$nama_kelurahan == "KRENDANG"] <- "KERENDANG"
populasi_kelurahan$nama_kelurahan[populasi_kelurahan$nama_kelurahan == "BALEKAMBANG"] <- "BALE KAMBANG"
populasi_kelurahan$nama_kelurahan[populasi_kelurahan$nama_kelurahan == "KAMPUNG TENGAH"] <- "KAMPUNG RAWA"
populasi_kelurahan$nama_kelurahan[populasi_kelurahan$nama_kelurahan == "KRAMATJATI"] <- "KRAMAT JATI"

populasi_kelurahan$nama_kelurahan[populasi_kelurahan$nama_kelurahan == "KALI BARU"] <- "KALIBARU"
populasi_kelurahan$nama_kelurahan[populasi_kelurahan$nama_kelurahan == "PINANGRANTI"] <- "PINANG RANTI"
populasi_kelurahan$nama_kelurahan[populasi_kelurahan$nama_kelurahan == "RAWAJATI"] <- "RAWA JATI"
populasi_kelurahan$nama_kelurahan[populasi_kelurahan$nama_kelurahan == "JATIPULO"] <- "JATI PULO"

populasi_kelurahan$nama_kelurahan[populasi_kelurahan$nama_kelurahan == "PALMERIAM"] <- "PAL MERIAM"

# Specify the file path where you want to save the CSV file
csv_file_path <- "populasi_kelurahan_2021.csv"

# Write the data frame to a CSV file
write.csv(populasi_kelurahan, file = csv_file_path, row.names = FALSE)
