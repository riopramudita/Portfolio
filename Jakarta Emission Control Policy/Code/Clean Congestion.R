################################################################################
##
## [ PROJ ] Final Project
## [ FILE ] Final Project.R
## [ AUTH ] < Group >
## [ INIT ] < November 11, 2023 >
##
################################################################################



## POLICY QUESTION:
## What is the impact of the private car usage reduction under the odd-even policy in 2019?
## Is there a correlation between the odd-even policy and the reduction of traffic jams in several segments of Jakarta in 2019?
## Is there a correlation between the odd-even policy and pollution reduction in 2019?


## --------------------------------------
## 1. load libraries and check directory
## --------------------------------------

# Load libraries
library(tidyverse)
library(weights)
library(lmtest)
library(sandwich)
library(knitr)
library(stargazer)
library(kableExtra)

getwd()

## --------------------------------------
## 2. load files 
## --------------------------------------


#data set of 41 congested streets in Jakarta
January <- read.csv("Data Set/January_2019.csv") %>% 
  select(-target_kecepatan, -target_waktu_tempuh, -pencapaian_kecepatan, -pencapaian_kecepatan_per_koridor) %>% 
  rename(time = waktu,
         direction = arah,
         corridor = koridor,
         distance = jarak,
         travel_time = pencapaian_waktu_tempuh) %>% 
  group_by(corridor) %>% 
  mutate(month = "1",
         id = cur_group_rows())


February <- read.csv("Data Set/February_2019.csv")%>% 
  select(-target_kecepatan, -target_waktu_tempuh, -pencapaian_kecepatan, -pencapaian_kecepatan_per_koridor) %>% 
  rename(time = waktu,
         direction = arah,
         corridor = koridor,
         distance = jarak,
         travel_time = pencapaian_waktu_tempuh) %>% 
  mutate(month = "2",
         id = cur_group_rows())

March <- read.csv("Data Set/March_2019.csv")%>% 
  select(-target_kecepatan, -target_waktu_tempuh, -pencapaian_kecepatan, -pencapaian_kecepatan_per_koridor) %>% 
  rename(time = waktu,
         direction = arah,
         corridor = koridor,
         distance = jarak,
         travel_time = pencapaian_waktu_tempuh) %>% 
  mutate(month = "3",
         id = cur_group_rows())

April <- read.csv("Data Set/April_2019.csv")%>% 
  select(-target_kecepatan, -target_waktu_tempuh, -pencapaian_kecepatan, -pencapaian_kecepatan_per_koridor) %>% 
  rename(time = waktu,
         direction = arah,
         corridor = koridor,
         distance = jarak,
         travel_time = pencapaian_Waktu_tempuh) %>% 
  mutate(month = "4",
         id = cur_group_rows())

May <- read.csv("Data Set/May_2019.csv")%>% 
  select(-target_kecepatan, -target_waktu_tempuh, -pencapaian_kecepatan, -pencapaian_kecepatan_per_koridor) %>% 
  rename(time = waktu,
         direction = arah,
         corridor = koridor,
         distance = jarak,
         travel_time = pencapaian_waktu_tempuh) %>% 
  mutate(month = "5",
         id = cur_group_rows())

June <- read.csv("Data Set/June_2019.csv")%>% 
  select(-target_kecepatan, -target_waktu_tempuh, -pencapaian_kecepatan, -pencapaian_kecepatan_per_koridor) %>% 
  rename(time = waktu,
         direction = arah,
         corridor = koridor,
         distance = jarak,
         travel_time = pencapaian_waktu_tempuh) %>% 
  mutate(month = "6",
         id = cur_group_rows())

July <- read.csv("Data Set/July_2019.csv")%>% 
  select(-target_kecepatan, -target_waktu_tempuh, -pencapaian_kecepatan, -pencapaian_kecepatan_per_koridor) %>% 
  rename(time = waktu,
         direction = arah,
         corridor = koridor,
         distance = jarak,
         travel_time = pencapaian_waktu_tempuh) %>% 
  mutate(month = "7",
         id = cur_group_rows())

August <- read.csv("Data Set/August_2019.csv")%>% 
  select(-target_kecepatan, -target_waktu_tempuh, -pencapaian_kecepatan, -pencapaian_kecepatan_per_koridor) %>% 
  rename(time = waktu,
         direction = arah,
         corridor = koridor,
         distance = jarak,
         travel_time = pencapaian_waktu_tempuh) %>% 
  mutate(month = "8",
         id = cur_group_rows())

September <- read.csv("Data Set/September_2019.csv")%>% 
  select(-target_kecepatan, -target_waktu_tempuh, -pencapaian_kecepatan, -pencapaian_kecepatan_per_koridor) %>% 
  rename(time = waktu,
         direction = arah,
         corridor = koridor,
         distance = jarak,
         travel_time = pencapaian_waktu_tempuh) %>% 
  mutate(month = "9",
         id = cur_group_rows())

October <- read.csv("Data Set/October_2019.csv")%>% 
  select(-target_kecepatan, -target_waktu_tempuh, -pencapaian_kecepatan, -pencapaian_kecepatan_per_koridor) %>% 
  rename(time = waktu,
         direction = arah,
         corridor = koridor,
         distance = jarak,
         travel_time = pencapaian_waktu_tempuh) %>% 
  mutate(month = "10",
         id = cur_group_rows())

November <- read.csv("Data Set/November_2019.csv")%>% 
  select(-target_kecepatan, -target_waktu_tempuh, -pencapaian_kecepatan, -pencapaian_kecepatan_per_koridor) %>% 
  rename(time = waktu,
         direction = arah,
         corridor = koridor,
         distance = jarak,
         travel_time = pencapaian_waktu_tempuh) %>% 
  mutate(month = "11",
         id = cur_group_rows())

December <- read.csv("Data Set/December_2019.csv")%>% 
  select(-target_kecepatan, -target_waktu_tempuh, -pencapaian_kecepatan, -pencapaian_kecepatan_per_koridor) %>% 
  rename(time = waktu,
         direction = arah,
         corridor = koridor,
         distance = jarak,
         travel_time = pencapaian_waktu_tempuh) %>% 
  mutate(month = "12",
         id = cur_group_rows())

#read the category of the streets
od_code <- read.csv("Data Set/OD COde.csv", head = TRUE, sep = ";") %>% 
  select(-target_kecepatan, -arah, -waktu, -jarak, -target_waktu_tempuh, -pencapaian_kecepatan_per_koridor, -pencapaian_kecepatan, -pencapaian_waktu_tempuh) %>% 
  rename(od_code = gage,
         corridor_ = koridor) %>% 
  mutate(id = cur_group_rows())
  



#bind and join
congestion_jkt <- rbind(January, February, March, 
                        April, May, June, July, 
                        August, September, October, 
                        November, December) %>% 
  mutate(avg_speed = distance/(travel_time/60)) %>% 
  na.omit() %>% 
  inner_join(od_code, by = c("id")) %>% 
  select(-corridor_)
  
  

str(congestion_jkt)
summary(congestion_jkt)

#before and after
clean_cng_jkt <- congestion_jkt %>% 
  mutate(before = ifelse(od_code > 0 &  od_code < 3, 1, 0),
         after = od_code) %>% 
  select(-od_code)

## --------------------------------------
## 3. Exploratory Data Analysis
## --------------------------------------

#EDA
clean_cng_jkt  %>% 
  group_by(as.numeric(month)) %>% 
  summarise(monthly_avg_speed = mean(avg_speed))


eda_cng_jkt <- clean_cng_jkt  %>% 
  mutate(month = month.abb[as.numeric(month)],
         after = as.factor(after)) %>% 
  group_by(month, after, before) %>% 
  summarise(monthly_avg_speed = mean(avg_speed))


eda_cng_jkt <- clean_cng_jkt  %>% 
  mutate(month = as.numeric(month),
         after = as.factor(after)) %>% 
  group_by(month, after, before) %>% 
  summarise(monthly_avg_speed = mean(avg_speed))


#Scatterplot
ggplot(eda_cng_jkt, 
       aes(x = month, y = monthly_avg_speed)) + 
  geom_line(aes (color = after)) + 
  scale_x_discrete(limits = month.abb) +
  xlab("Month") +
  ggtitle("Time series plot of monthly average speed") +
  ylab("Average speed") +
  labs(color ="after") +
  scale_color_brewer(palette = "Set2")


## --------------------------------------
## 4. Summary Table
## --------------------------------------

#create summary table for mode month data
month_order <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

cng_summary_month <- clean_cng_jkt %>%
  mutate(month = factor(month, levels = month_order)) %>% 
  group_by(month) %>%
  select(-time, -direction, -corridor, -id, -before, -after, -distance, -travel_time) %>%
  summarise_all(list(
    'Monthly Average Speed' = ~mean(avg_speed),
    SD = ~sd(avg_speed)
  )) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  kable(format = "markdown", align = "c", caption = "Summary by Month") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) 

print(cng_summary_month)

devtools::install_github("kupietz/kableExtra")

#create summary table for mode month data

cng_summary_treat <- clean_cng_jkt %>%
  mutate(month = factor(month, levels = month_order)) %>% 
  group_by(after) %>%
  select(-time, -direction, -corridor, -id, -before, -distance, -travel_time, -month) %>%
  summarise_all(list(
    'Monthly Average Speed' = ~mean(avg_speed),
    SD = ~sd(avg_speed)
  )) %>%
  mutate(across(where(is.numeric), ~round(., 2))) %>%
  kable(format = "markdown", align = "c", caption = "Summary by Type") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) 

print(cng_summary_treat)


