library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(janitor)

# WRANGLING FOR THIS GRAPH ONLY
data <- read_csv('data/clean/full_data.csv') %>% 
  mutate(perc_gdp = (trade_usd/gdp)*100) %>% 
  arrange(desc(perc_gdp)) %>% 
  filter(year == 2018,               ## MAKE THIS LINE PART OF CALLBACK
         direction == 'Export') %>%  ## MAKE THIS LINE PART OF CALLBACK
  head(16)

ggplot(data, aes(x = reorder(country, -perc_gdp), y = perc_gdp)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = 'Country', 
       y = 'Percentage of GDP', 
       title = 'Exports as a Percentage of GDP in 2018') + ## MAKE THIS LINE PART OF CALLBACK
  ggsave('./media/perc-of-gdp-graph-draft.png')

# Sources:
# https://stackoverflow.com/questions/1330989/rotating-and-spacing-axis-labels-in-ggplot2
# https://sebastiansauer.github.io/ordering-bars/