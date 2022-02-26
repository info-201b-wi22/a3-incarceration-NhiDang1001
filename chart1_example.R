library(dplyr)
library(ggplot2)
library(tibbletime)
library(scales)
library(tidyr)
library(tidyverse)
library("mapproj")
library("maps")

#Total Asian American prison population trend from 2000-2018
np_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


num_pop_per_year <- np_data %>%
  filter(year >= 2000 & year <= 2018) %>% 
  group_by(year) %>%
  summarize(Asian_American = mean(aapi_pop_15to64, na.rm = T),
            Latinx = mean(latinx_pop_15to64, na.rm = T),
            Native_American = mean(native_pop_15to64, na.rm = T))

aln_cleaned_trend_data <- pivot_longer(num_pop_per_year, 2:4, names_to = "Race", values_to = "Avg_Pop")

ggplot(data = aln_cleaned_trend_data, aes(x=year, y = Avg_Pop, color = Race)) +
  geom_line() +
  labs(title = 'Trend average Asian American vs Latinx prison population (15-64) 2000 - 2018',
       x='Year', y='Average Prison Population')



