# Reflection

## Effective Components of the dashboard:
-	Our two research questions focused on different areas: total of imports/exports in USD, in addition to the relationship of imports/exports as a percentage of GDP. By being able to view both time-series and cross-functional data within the same screen, we were successfully able to capture both research questions, and a user has to scroll minimally to get the takeaways.
-	The ability to look at time-series data for an individual country enables users to see imports/exports around internal and external conflicts.
- The default of the map in the dashboard is to include the US in the map, however since it is such a large importer and exporter, it dominates the scale very heavily. The choice to add a button to either include or remove the US allows the user to see other countries much more clearly - when the US is distinguished as an outlier, the rest of the data is much easier to understand.
- The R dashboard now implements interactive map zooming, allowing user to get a closer look on individual smaller areas and countries. 


## Areas of Improvement/Enhancement for the dashboard:
-	The arms dataset that was chosen was incomplete, and in the earlier years of data available, there are very few countries in the dataset, in addition to other challenges such as gaps in year data, and there were changes to country borders over 30 years. To mitigate this, we could have chosen to look at a smaller time horizon that had more complete data or restricted the regions that were looked at (ie. looked only at North America rather than globally). 
-	The original intention was to have a “Country” callback included in the app, which would have allowed users to interactively select a country in the map and the time-series data for that country would have shown below. This was not technically feasible given the time frame, and so we opted for a dropdown where users can select the country that they would like to see time-series data for.
- The year slider bar only shows the year when the toggle is held by the user, an improvement would be to add the labels to the slider bar. 
- Updating the dashboard takes longer than the same updates in the python replica, a faster update would be ideal but beyond the scope of this project. 
- Due to dependencies conflict, the app could not be easily deployed to Heroku for now. It would be good to figure this out and have the dashboard running there at some point.