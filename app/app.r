library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(plotly)
library(scales)
library("rnaturalearth")
library("rnaturalearthdata")

# Need to load the styles from external source
app <- Dash$new(external_stylesheets = "https://raw.githubusercontent.com/UBC-MDS/DSCI532_World_Wide_Weapons_Report_R/master/app/assets/styles.css")

full_data <- read_csv("https://raw.githubusercontent.com/UBC-MDS/DSCI532_World_Wide_Weapons_Report_R/master/data/clean/full_data.csv")
arms_map_data <- read_csv("https://raw.githubusercontent.com/UBC-MDS/DSCI532_World_Wide_Weapons_Report_R/master/data/clean/arms_map.csv")

countryDropdown <- dccDropdown(
  id = "country",
  # map/lapply can be used as a shortcut instead of writing the whole list
  # especially useful if you wanted to filter by country!
  options = map(
    unique(full_data$country), function(x) {
      list(label = x, value = x)
    }),
  value = 'Canada'
)

statisticDropdown <- dccDropdown(
  id = "statistic",
  # map/lapply can be used as a shortcut instead of writing the whole list
  # especially useful if you wanted to filter by country!
  options = map(
    unique(full_data$direction), function(x) {
      list(label = x, value = x)
    }),
  value = 'Import'
)

yearSlider <- dccSlider(
  id = "year_value",
  min = 1988,
  max = 2018,
  marks = list(1990, 1995, 2000, 2005, 2010, 2015),
  value = 2018,
  tooltip = 'always visible',
  included = FALSE
)

# Map-specific pre-wrangling (has to be done in app for performance reasons)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(sovereignt = str_replace_all(sovereignt, 'Iran', 'Iran, Islamic Republic of'),
         sovereignt = str_replace_all(sovereignt, 'Russia', 'Russian Federation'),
         sovereignt = str_replace_all(sovereignt, 'Brunei', 'Brunei Darussalam'),
         sovereignt = str_replace_all(sovereignt, 'Central African Republic', 'Central African Rep.'),
         sovereignt = str_replace_all(sovereignt, 'Czech Republic', 'Czech Republic'),
         sovereignt = str_replace_all(sovereignt, 'Democratic Republic of the Congo', 'Congo, the Democratic Republic of the'),
         sovereignt = str_replace_all(sovereignt, 'Dominican Republic', 'Dominican Rep.'),
         sovereignt = str_replace_all(sovereignt, 'North Korea', "Korea, Democratic People's Republic of"),
         sovereignt = str_replace_all(sovereignt, 'South Korea', 'Korea, Republic of'),
         sovereignt = str_replace_all(sovereignt, 'Laos', "Lao People's Democratic Republic"),
         sovereignt = str_replace_all(sovereignt, 'Macedonia', 'Macedonia, the former Yugoslav Republic of'),
         sovereignt = str_replace_all(sovereignt, 'Moldova', 'Moldova, Republic of'),
         sovereignt = str_replace_all(sovereignt, 'Taiwan', 'Taiwan, Province of China'),
         sovereignt = str_replace_all(sovereignt, 'Syria', 'Syrian Arab Republic'),
         sovereignt = str_replace_all(sovereignt, 'Vietnam', 'Viet Nam'),
         sovereignt = str_replace_all(sovereignt, 'Venezuela', 'Venezuela, Bolivarian Republic of'))
arms_geo_df <- merge(arms_map_data, world, by.x = 'Country', by.y = 'sovereignt') %>%
  select(id, Country, Year, Direction, USD_Value, GDP, percent_GDP, geometry) %>%
  sf::st_as_sf()

make_world_map_graph <- function(year = 2018, statistic = 'Import', map_options = list()) {
  print(map_options)

  filtered_df <- arms_geo_df %>%
    filter(Year == year, Direction == statistic)

  if (!'usa' %in% map_options) {
    filtered_df <- filter(filtered_df, Country != 'United States of America')
  }

  if ('gdp_pct' %in% map_options) {
    stat_column = expr(percent_GDP)
    stat_title = 'GDP %'
    label_formatter = NULL
  }
  else {
    stat_column = expr(USD_Value)
    stat_title = 'USD Value'
    if ('usa' %in% map_options) {
      label_formatter = number_format(scale = 1e-9, suffix = " B")
    }
    else {
      label_formatter = number_format(scale = 1e-6, suffix = " M")
    }
  }

  p <- filtered_df %>%
    ggplot(aes(text = Country)) +
    geom_sf(aes(fill = !!stat_column),
            color = "white",
            size = 0.1) +
    scale_fill_gradient(low = 'orange',
                        high = 'brown',
                        label = label_formatter) +
    labs(fill = stat_title) +
    theme_classic() +
    theme(panel.border = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())

  ggplotly(p, height = 400, width = 800)
}

# Uses default parameters such as all_continents for initial graph
make_GDP_percent_graph <- function(country_shown = "Canada",
                                   statistic = "Import",
                                   year = 2018) {

  #filter our data based on the stat/country selections
  data <- full_data %>%
    mutate('gdp_percent' = trade_usd / gdp) %>%
    filter(direction == statistic) %>%
    filter(country == country_shown)

  # make the plot!
  # on converting yaxis string to col reference (quosure) by `!!sym()`
  # see: https://github.com/r-lib/rlang/issues/116

  p1 <- ggplot(data, aes(x = year, y = gdp_percent)) +
    geom_bar(stat = "identity", colour = 'black', fill = 'orange', size = 0.2) +
    ylab('% of GDP') +
    xlab('Year') +
    geom_vline(xintercept = year, color = 'red') +
    ggtitle(paste0(country_shown, " Weapons ", statistic, " share in GDP")) +
    theme_classic()

  p1

  ggplotly(p1, height = 350, width = 500)
}

make_USD_total_graph <- function(country_shown = "Canada",
                                 statistic = "Import",
                                 year = 2018) {


  #filter our data based on the stat/country selections
  data <- full_data %>%
    filter(direction == statistic) %>%
    filter(country == country_shown)

  # make the plot!
  # on converting yaxis string to col reference (quosure) by `!!sym()`
  # see: https://github.com/r-lib/rlang/issues/116

  p2 <- ggplot(data, aes(x = year, y = trade_usd)) +
    geom_area(fill = 'orange', colour = 'black', size = 0.2) +
    scale_y_continuous(label = number_format(scale = 1e-6, suffix = " M")) +
    ylab('USD Value') +
    xlab('Year') +
    geom_vline(xintercept = year, color = 'red') +
    ggtitle(paste0(country_shown, " Weapons ", statistic, " value in USD")) +
    theme_classic()

  p2

  ggplotly(p2, height = 350, width = 500)
}

make_gdp_perc_year_graph <- function(statistic = "Import",
                                     year_val = 2018) {

  # wrangling for this graph
  data <- full_data %>%
    mutate(perc_gdp = (trade_usd / gdp) * 100) %>%
    arrange(desc(perc_gdp)) %>%
    filter(year == year_val,
           direction == statistic) %>%
    head(20)

  # make the plot
  p3 <- ggplot(data, aes(x = reorder(country, -perc_gdp), y = perc_gdp, fill = perc_gdp)) +
    geom_bar(stat = 'identity', colour = 'black', size = 0.25) +
    scale_fill_gradient(low = 'orange', high = 'brown', guide = FALSE) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = 'Country',
         y = 'Percentage of GDP') +
    ggtitle(paste0(statistic, "s as a Percentage of GDP in ", year_val))

  p3

  ggplotly(p3)
}

# Now we define the graph as a dash component using generated figure
graph_world_map <- dccGraph(
  id = 'world_map',
  figure = make_world_map_graph()
)

graph_gdp_cont <- dccGraph(
  id = 'GDP_perc_country',
  figure = make_GDP_percent_graph() # gets initial data using argument defaults
)

graph_USD_cont <- dccGraph(
  id = 'USD_total_country',
  figure = make_USD_total_graph() # gets initial data using argument defaults
)

graph_perc_gdp <- dccGraph(
  id = 'Perc_GDP_graph',
  figure = make_gdp_perc_year_graph() # gets initial data using argument defaults
)

app$layout(
  htmlDiv(
    list(
      htmlH1('World Wide Arms and Ammunition Movement and GDP Effects'),

      htmlDiv(list(
        htmlH3("This app is designed to explore how the movement of weapons globally has changed over the last 30 years, and how imports and exports of arms and ammunition relate to a country's GDP."),

        htmlDiv(
          list(
            htmlDiv(list(htmlP("Select a statistic:"),
                         statisticDropdown,
                         htmlP("Select a country:"),
                         countryDropdown,
                         dccChecklist(options = list(list("label" = "Show GDP %", "value" = "gdp_pct"),
                                                     list("label" = "Include USA", "value" = "usa")),
                                      id = "map_options",
                                      value = list("gdp_pct", "usa"))), className = 'left-col'),
            htmlDiv(list(graph_world_map,
                         htmlP("Select a year to view cross-sectional data:"),
                         yearSlider), className = 'right-col')),
          className = 'top-container'
        ),

        htmlDiv(list(graph_perc_gdp), className = 'middle-container'),

        htmlDiv(list(htmlDiv(list(graph_gdp_cont), className = 'col'),
                     htmlDiv(list(graph_USD_cont), className = 'col')), className = 'bottom-container'),

        htmlDiv(), #spacer
        dccMarkdown("[Data Source](https://cran.r-project.org/web/packages/gapminder/README.html)")
      ), className = 'inner-container')
    ), className = 'outer-container'
  )
)

app$callback(
#update figure of gap-graph
output = list(id = 'GDP_perc_country', property = 'figure'),
#based on values of year, continent, y-axis components
params = list(input(id = 'country', property = 'value'),
              input(id = 'statistic', property = 'value'),
              input(id = 'year_value', property = 'value')),
#this translates your list of params into function arguments
function(country_value, statistic_value, year) {
  make_GDP_percent_graph(country_value, statistic_value, year)
})

app$callback(
#update figure of gap-graph
output = list(id = 'USD_total_country', property = 'figure'),
#based on values of year, continent, y-axis components
params = list(input(id = 'country', property = 'value'),
              input(id = 'statistic', property = 'value'),
              input(id = 'year_value', property = 'value')),
#this translates your list of params into function arguments
function(country_value, statistic_value, year) {
  make_USD_total_graph(country_value, statistic_value, year)
})

app$callback(
#update figure of gdp-perc-year graph
output = list(id = 'Perc_GDP_graph', property = 'figure'),
params = list(input(id = 'statistic', property = 'value'),
              input(id = 'year_value', property = 'value')),
#this translates your list of params into function arguments
function(statistic_value, year_value) {
  make_gdp_perc_year_graph(statistic_value, year_value)
})

app$callback(
#update figure of gdp-perc-year graph
output = list(id = 'Perc_GDP_graph', property = 'figure'),
params = list(input(id = 'statistic', property = 'value'),
              input(id = 'year_value', property = 'value')),
#this translates your list of params into function arguments
function(statistic_value, year_value) {
  make_gdp_perc_year_graph(statistic_value, year_value)
})

app$callback(
#update world map graph
output = list(id = 'world_map', property = 'figure'),
params = list(input(id = 'year_value', property = 'value'),
              input(id = 'statistic', property = 'value'),
              input(id = 'map_options', property = 'value')),
function(year_value, statistic_value, map_options) {
  make_world_map_graph(year_value, statistic_value, map_options)
})

app$run_server()