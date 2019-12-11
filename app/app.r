library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(gapminder)
library(repr)
library(gridExtra)

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
    ggtitle(paste0(country_shown, " Weapons ", statistic, " share in GDP"))
    
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
    ggtitle(paste0(country_shown, " Weapons ", statistic, " value in USD"))
  
  p2

  ggplotly(p2)
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

app$layout(
  htmlDiv(
    list(
      htmlH1('World Wide Arms and Ammunition Movement and GDP Effects'),
      htmlH3("This app is designed to explore how the movement of weapons globally has changed over the last 30 years, and how imports and exports of arms and ammunition relate to a country's GDP."),
      #yearMarks,
      countryDropdown,
      statisticDropdown,
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

app$run_server()

