library(dplyr)
library(ggplot2)
library(tibbletime)
library(scales)
library(tidyr)
library(tidyverse)
library("mapproj")
library("maps")

np_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

pop_per_year <- np_data %>%
  filter(year >= 2008 & year <= 2018) %>% 
  group_by(year) %>%
  summarize(Black = mean(black_pop_15to64, na.rm = T),
            Whie = mean(white_pop_15to64, na.rm = T))

bl_cleaned_trend_data <- pivot_longer(pop_per_year, 2:3, names_to = "Race", values_to = "Avg_Pop")

ggplot(data = bl_cleaned_trend_data) +
  geom_col(mapping= aes(x=year, y = Avg_Pop, fill = Race), position = "dodge") +
  labs(title = 'Average Black vs White prison population (15-64) 2008 - 2018', x='Year', y='Average Prison Population')

