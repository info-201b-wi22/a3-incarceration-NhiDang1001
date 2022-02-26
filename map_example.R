library("mapproj")
library("maps")
library(ggplot2)
library(dplyr)
library(patchwork)

np_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

np_state_data <- np_data  %>%  group_by(state) %>% summarize(state_total = sum(total_pop))
state_shape <- map_data('state')
state_abbrevs <- data.frame(state.abb, state.name)
np_state_data <- left_join(np_state_data, state_abbrevs, by = c('state' = 'state.abb'))
np_state_data <- np_state_data %>% mutate(region = tolower(state.name))

state_shape <- left_join(state_shape, np_state_data)

library(scales)

blank_theme <- theme_bw() +
  theme(
    axis.title = element_blank(), 
    plot.background = element_blank(), 
  )


ggplot(state_shape)+
  geom_polygon(mapping = aes(x=long, y=lat, group = group, fill = state_total))+
  blank_theme + 
  coord_map()+
  labs(title = 'Total Prison Population All Ages 1970-2018', fill = 'Population') +
  scale_fill_continuous(low = 'yellow', high ='red', labels = scales::label_number_si()) 
