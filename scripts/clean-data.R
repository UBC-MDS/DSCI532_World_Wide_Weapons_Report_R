library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(janitor)

# Wrangling for GDP Data
gdp_data <- read_csv('./data/dirty/gdp_1960_2018_worldbank.csv', skip=4) %>% 
  janitor::clean_names() %>% 
  select(-country_code, -indicator_name, -indicator_code) %>% 
  gather(key = 'year',
         value = 'gdp',
         -country_name) %>% 
  mutate(year = as.integer(str_replace_all(year, "x", "")),
         country_name = str_replace_all(country_name, 'Bosnia and Herzegovina', 'Bosnia Herzegovina'),
         country_name = str_replace_all(country_name, 'Central African Republic', 'Central African Rep.'),
         country_name = str_replace_all(country_name, "Cote d'Ivoire", "Côte d'Ivoire"),
         country_name = str_replace_all(country_name, 'Czech Republic', 'Czech Rep.'),
         country_name = str_replace_all(country_name, 'Dominican Republic', 'Dominican Rep.'),
         country_name = str_replace_all(country_name, 'Solomon Islands', 'Solomon Isds'),
         country_name = str_replace_all(country_name, 'United States', 'USA')
         ) %>% 
  rename('country' = 'country_name') %>% 
  filter(year >= 1988,
         year <= 2018) %>% 
  write_csv(path = './data/clean/gdp_data.csv')

# Wrangling for Arms Data
arms_data <- read_csv('./data/dirty/un-arms-and-ammunition_1988-2018.csv') %>% 
  janitor::clean_names() %>% 
  select(-commodity, -weight_kg, -quantity_name, -quantity) %>% 
  rename('direction' = 'flow',
         'country' = 'country_or_area') %>% 
  mutate(direction = str_replace_all(direction, 'Re-Import', 'Import'),
         direction = str_replace_all(direction, 'Re-Export', 'Export'),
         direction = as.factor(direction),
         year = as.integer(year)) %>% 
  group_by(country, year, direction) %>% 
  mutate(trade_usd = sum(trade_usd)) %>% 
  distinct() %>% 
  write_csv(path = './data/clean/arms_data.csv')

# Merged Data
data <- left_join(arms_data, gdp_data, by = c('country', 'year')) %>% 
  write_csv(path = './data/clean/full_data.csv')

