library(dplyr)
library(lubridate)
library(knitr)
library(tibbletime)
library(scales)
library(tidyr)
library(tidyverse)


np_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#Which state has the highest number of prisoners?
high_state <- np_data %>%
  group_by(state) %>%
  summarise(num_prisoners = n()) %>%
  filter(num_prisoners == max(num_prisoners)) %>%
  pull(state)

#Which state has the lowest number of prisoners?
low_state <- np_data %>%
  group_by(state) %>%
  summarise(num_prisoners = n()) %>%
  filter(num_prisoners == min(num_prisoners)) %>%
  pull(state)

#Top 5 county with highest number of prisoners:
high_county <- np_data %>% group_by(county_name) %>% 
  summarize(number_of_prisoners = n()) %>% arrange(desc(number_of_prisoners)) %>% slice(1:5) %>% pull(county_name)



#Ratio of black prisoners and total prisoners (15-64 years old) per year from 2008 to 2018
black_total_pop_per_year_set <- np_data %>%
  filter(year >= 2008 & year <= 2018) %>% 
  group_by(year) %>%
  summarize(Num_black = sum(black_pop_15to64, na.rm = T),
            Num_total = sum(total_pop_15to64, na.rm = T))

ratio_black_total <- black_total_pop_per_year_set %>% mutate(Ratio_Black_Total = Num_black/Num_total) %>% 
  pull(Ratio_Black_Total)


#Top 3 region has the highest number of prisoners?
high_region <- np_data %>%
  group_by(region) %>%
  summarise(num_prisoners = n()) %>%
  arrange(desc(num_prisoners)) %>% slice(1:3) %>% pull(region)

#Trend over time of number of prison population by 3 race: Asian American, Latinx, and Native American from 1970-2018. Order from top to bottom.
pop_by_race <- np_data %>%
  group_by(year) %>%
  summarize(Asian_American = sum(aapi_pop_15to64, na.rm = T),
            Latinx = sum(latinx_pop_15to64, na.rm = T),
            Native_American = sum(native_pop_15to64, na.rm = T))

trend_data <- pivot_longer(pop_by_race, 2:4, names_to = "Race", values_to = "Avg_Pop")

trend_over_time_3_races <- trend_data %>% group_by(Race) %>% 
  summarize(number_of_prisoners = n()) %>% arrange(desc(number_of_prisoners)) %>% slice(1:3) %>% pull(Race)



