library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
# library(tidyverse)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(plotly)
library("rnaturalearth")
library("rnaturalearthdata")

# We'll replace our styles with an external stylesheet 
# for simplicity
app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

full_data <- read_csv("https://raw.githubusercontent.com/evelynmoorhouse/DSCI532_World_Wide_Weapons_Report_R/master/data/clean/full_data.csv")

# yearMarks <- map(unique(full_data$year), as.character)
# names(yearMarks) <- unique(full_data$year)
# yearSlider <- dccRangeSlider(
#   id = "year",
#   marks = yearMarks,
#   min = 1990,
#   max = 2018,
#   step = 5,
#   value = list(1990, 2018)
# )

countryDropdown <- dccDropdown(
  id = "country",
  # map/lapply can be used as a shortcut instead of writing the whole list
  # especially useful if you wanted to filter by country!
  options = map(
    unique(full_data$country), function(x){
      list(label=x, value=x)
    }),
  value = 'Canada' 
)

statisticDropdown <- dccDropdown(
  id = "statistic",
  # map/lapply can be used as a shortcut instead of writing the whole list
  # especially useful if you wanted to filter by country!
  options = map(
    unique(full_data$direction), function(x){
      list(label=x, value=x)
    }),
  value = 'Import'
)

yearSlider <- dccSlider(
  id = "year_value",
  min = 1988,
  max = 2018,
  marks = list(as.character(unique(full_data$year))),
  value = 2018
)

# Uses default parameters such as all_continents for initial graph
make_GDP_percent_graph <- function(country_shown = "Canada",
                       statistic = "Import"){

  
  #filter our data based on the stat/country selections
  data <- full_data %>%
    mutate('gdp_percent' = trade_usd/gdp) %>% 
    filter(direction == statistic) %>% 
    filter(country == country_shown) 
 
  # make the plot!
  # on converting yaxis string to col reference (quosure) by `!!sym()`
  # see: https://github.com/r-lib/rlang/issues/116

  p1 <- ggplot(data, aes(x = year, y = gdp_percent)) +
    geom_bar(stat = "identity", colour = 'black', fill = 'orange') +
    ylab('% of GDP') +
    xlab('Year') +
    ggtitle(paste0(country_shown, " Weapons ", statistic, " share in GDP")) +
    theme_classic()
    
  p1
  
  ggplotly(p1)
}

make_USD_total_graph <- function(country_shown = "Canada",
                       statistic = "Import"){

  
  #filter our data based on the stat/country selections
  data <- full_data %>%
    filter(direction == statistic) %>% 
    filter(country == country_shown) 
 
  # make the plot!
  # on converting yaxis string to col reference (quosure) by `!!sym()`
  # see: https://github.com/r-lib/rlang/issues/116

  p2 <- ggplot(data, aes(x = year, y = trade_usd)) +
    geom_area(fill = 'orange', colour = 'black') +
    ylab('USD Value') +
    xlab('Year') +
    ggtitle(paste0(country_shown, " Weapons ", statistic, " value in USD")) +
    theme_classic()
  
  p2

  ggplotly(p2)
}

make_gdp_perc_year_graph <- function(statistic = "Import",
                                year_val = 2018) {
  
  # wrangling for this graph
  data <- full_data %>% 
    mutate(perc_gdp = (trade_usd/gdp)*100) %>% 
    arrange(desc(perc_gdp)) %>% 
    filter(year == year_val,
           direction == statistic) %>%
    head(16)
  
  # make the plot
  p3 <- ggplot(data, aes(x = reorder(country, -perc_gdp), y = perc_gdp)) +
    geom_bar(stat = 'identity', colour = 'black', fill = 'orange') +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = 'Country', 
         y = 'Percentage of GDP') +
    ggtitle(paste0( statistic, "s as a Percentage of GDP in ", year_val))
    
  
  p3
  
  ggplotly(p3)
}

# Now we define the graph as a dash component using generated figure
graph_gdp_cont <- dccGraph(
  id = 'GDP_perc_country',
  figure=make_GDP_percent_graph() # gets initial data using argument defaults
)

graph_USD_cont <- dccGraph(
  id = 'USD_total_country',
  figure=make_USD_total_graph() # gets initial data using argument defaults
)

graph_perc_gdp <- dccGraph(
  id = 'Perc_GDP_graph',
  figure=make_gdp_perc_year_graph() # gets initial data using argument defaults
)

app$layout(
  htmlDiv(
    list(
      htmlH1('World Wide Arms and Ammunition Movement and GDP Effects'),
      htmlH3("This app is designed to explore how the movement of weapons globally has changed over the last 30 years, and how imports and exports of arms and ammunition relate to a country's GDP."),
      htmlP("Select a year to view cross-sectional data:"),
      yearSlider,
      htmlP("Select a statistic:"),
      statisticDropdown,
      graph_perc_gdp,
      htmlP("Select a country to view time-series data:"),
      countryDropdown,
      graph_gdp_cont,
      graph_USD_cont,
      htmlDiv(), #spacer
      dccMarkdown("[Data Source](https://cran.r-project.org/web/packages/gapminder/README.html)")
    )
  )
)

app$callback(
  #update figure of gap-graph
  output=list(id = 'GDP_perc_country', property='figure'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'country', property='value'),
              input(id = 'statistic', property='value')),
  #this translates your list of params into function arguments
  function(country_value, statistic_value) {
    make_GDP_percent_graph(country_value, statistic_value)
  })

app$callback(
  #update figure of gap-graph
  output=list(id = 'USD_total_country', property='figure'),
  #based on values of year, continent, y-axis components
  params=list(input(id = 'country', property='value'),
              input(id = 'statistic', property='value')),
  #this translates your list of params into function arguments
  function(country_value, statistic_value) {
    make_USD_total_graph(country_value, statistic_value)
  })

app$callback(
  #update figure of gdp-perc-year graph
  output=list(id = 'Perc_GDP_graph', property='figure'),
  params=list(input(id = 'statistic', property='value'),
              input(id = 'year_value', property='value')),
  #this translates your list of params into function arguments
  function(statistic_value, year_value) {
    make_gdp_perc_year_graph(statistic_value, year_value)
  })

app$callback(
  #update figure of gdp-perc-year graph
  output=list(id = 'Perc_GDP_graph', property='figure'),
  params=list(input(id = 'statistic', property='value'),
              input(id = 'year_value', property='value')),
  #this translates your list of params into function arguments
  function(statistic_value, year_value) {
    make_gdp_perc_year_graph(statistic_value, year_value)
  })

app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))