library(tidyverse)
library(dplyr)
library(summarytools)
library(knitr)
library(kableExtra)


#data set of monthly passengers of transjakarta
January <- read.csv("tj data/1_tj.csv") %>% 
  rename(type = jenis,
         code = kode_trayek,
         corridor = trayek,
         passenger = jumlah_penumpang) %>% 
  group_by(corridor) %>% 
  mutate(month = "1",
         id = cur_group_rows())

February <- read.csv("tj data/2_tj.csv") %>% 
  rename(type = jenis,
         code = kode_trayek,
         corridor = trayek,
         passenger = jumlah_penumpang) %>% 
  group_by(corridor) %>% 
  mutate(month = "2",
         id = cur_group_rows())

March <- read.csv("tj data/3_tj.csv") %>% 
  rename(type = jenis,
         code = kode_trayek,
         corridor = trayek,
         passenger = jumlah_penumpang) %>% 
  group_by(corridor) %>% 
  mutate(month = "3",
         id = cur_group_rows())

April <- read.csv("tj data/4_tj.csv") %>% 
  rename(type = jenis,
         code = kode_trayek,
         corridor = trayek,
         passenger = jumlah_penumpang) %>% 
  group_by(corridor) %>% 
  mutate(month = "4",
         id = cur_group_rows())

May <- read.csv("tj data/5_tj.csv") %>% 
  select(-X) %>%
  rename(type = jenis,
         code = kode_trayek,
         corridor = trayek,
         passenger = jumlah_penumpang) %>% 
  group_by(corridor) %>% 
  mutate(month = "5",
         id = cur_group_rows())

June <- read.csv("tj data/6_tj.csv") %>% 
  rename(type = jenis,
         code = kode_trayek,
         corridor = trayek,
         passenger = jumlah_penumpang) %>% 
  group_by(corridor) %>% 
  mutate(month = "6",
         id = cur_group_rows())

July <- read.csv("tj data/7_tj.csv") %>% 
  rename(type = jenis,
         code = kode_trayek,
         corridor = trayek,
         passenger = jumlah_penumpang) %>% 
  group_by(corridor) %>% 
  mutate(month = "7",
         id = cur_group_rows())

August <- read.csv("tj data/8_tj.csv") %>% 
  rename(type = jenis,
         code = kode_trayek,
         corridor = trayek,
         passenger = jumlah_penumpang) %>% 
  group_by(corridor) %>% 
  mutate(month = "8",
         id = cur_group_rows())

September <- read.csv("tj data/9_tj.csv") %>% 
  rename(type = jenis,
         code = kode_trayek,
         corridor = trayek,
         passenger = jumlah_penumpang) %>% 
  group_by(corridor) %>% 
  mutate(month = "9",
         id = cur_group_rows())

October <- read.csv("tj data/10_tj.csv") %>% 
  rename(type = jenis,
         code = kode_trayek,
         corridor = trayek,
         passenger = jumlah_penumpang) %>% 
  group_by(corridor) %>% 
  mutate(month = "10",
         id = cur_group_rows())

November <- read.csv("tj data/11_tj.csv") %>% 
  rename(type = jenis,
         code = kode_trayek,
         corridor = trayek,
         passenger = jumlah_penumpang) %>% 
  group_by(corridor) %>% 
  mutate(month = "11",
         id = cur_group_rows())

December <- read.csv("tj data/12_tj.csv") %>% 
  rename(type = jenis,
         code = kode_trayek,
         corridor = trayek,
         passenger = jumlah_penumpang) %>% 
  group_by(corridor) %>% 
  mutate(month = "12",
         id = cur_group_rows())




#bind
tj_passenger_data <- rbind(January, February, March, 
                        April, May, June, July, 
                        August, September, October, 
                        November, December)

str(tj_passenger_data)

#cleaning the numerical data and dropping irrelevant modes
tj_passenger_data$passenger <- gsub("\\.", "", tj_passenger_data$passenger)

tj_passenger_clean <- tj_passenger_data %>%
  filter(!is.na(code) & code != "")

tj_passenger_clean_F <- tj_passenger_clean %>%
  filter(type != "LAMPIRAN RINCIAN GRATIS GATE NON BRT" & type != "ANGKUTAN WISATA" & 
           type != "LAMPIRAN LAYANAN GRATIS BRT")

tj_passenger_clean_F$passenger <- as.numeric(tj_passenger_clean_F$passenger)
tj_passenger_clean_F$month <- as.factor(tj_passenger_clean_F$month)
tj_passenger_clean_F$type <- as.factor(tj_passenger_clean_F$type)

tj_passenger_clean_F %>%
  group_by(type) %>%
  summarise(Count = n_distinct(.))

month_order <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
#create summary table for monthly data
tj_summary_month <- tj_passenger_clean_F %>%
  mutate(month = factor(month, levels = month_order)) %>%
  group_by(month) %>%
  select_if(is.numeric) %>%
  select(-id) %>%
  summarise_all(list(
    Mean = ~mean(.),
    SD = ~sd(.)
  ))  %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  kable(format = "markdown", align = "c", caption = "Summary by Month") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) 

print(tj_summary_month)

#create summary table for mode type data
tj_summary_type <- tj_passenger_clean_F %>%
  group_by(type) %>%
  select_if(is.numeric) %>%
  select(-id) %>%
  summarise_all(list(
    Mean = ~mean(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE)
  )) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  kable(format = "markdown", align = "c", caption = "Summary by Type") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) 

print(tj_summary_type)

#create summary table for per corridor data
tj_summary_corr <- tj_passenger_clean_F %>%
  group_by(corridor) %>%
  select_if(is.numeric) %>%
  select(-id) %>%
  summarise_all(list(
    Mean = ~mean(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE)
  )) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  kable(format = "markdown", align = "c", caption = "Summary by Corridor") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) 

print(tj_summary_corr)

#plotting the average passenger per month data
avg_pass <- tj_passenger_clean_F %>% 
  mutate(month = factor(month, levels = month_order)) %>%
  group_by(month) %>%
  summarise(avg_pass = mean(passenger, na.rm = TRUE))
avg_pass

ggplot(data = avg_pass, 
       aes(x = month, y = avg_pass)) + 
  geom_col() + 
  ggtitle("Average Number of Passengers per Month") +
  xlab("Month") +
  ylab("Average Number of Passengers") + 
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K"))

# plotting the passenger per mode
mode_avg <- tj_passenger_clean_F %>%
  group_by(type) %>%
  summarise(mode_avg = mean(passenger, na.rm = TRUE))

ggplot(data = mode_avg, 
       aes(x = type, y = mode_avg)) + 
  geom_col() + 
  ggtitle("Average Number of Passengers by Public Transportation Type") +
  xlab("Type of Public Transportation Mode") +
  ylab("Average Number of Passengers") + 
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





