#Load packages
library(readr)
library(leaflet)
library(htmlwidgets)

#Import data
map_test_data <- read_csv("/workspace/data/input/map_test_data.csv")

#Create map
test_map <- leaflet() %>%
  addTiles() %>%
  addMarkers(data = map_test_data, clusterOptions = markerClusterOptions(),
             ~Longitude, ~Latitude)
#View
test_map

#Save widget
saveWidget(test_map, file = "/workspace/data/output/test_map.html")

