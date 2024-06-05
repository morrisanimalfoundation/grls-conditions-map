#Load packages
library(readr)
library(leaflet)
library(htmlwidgets)

#Import data
map_test_data <- read_csv("C:/Users/AroRoseman/OneDrive - Morris Animal Foundation/Desktop/map_test_data.csv")

#Create map
test_map <- leaflet() %>%
  addTiles() %>%
  addMarkers(data = map_test_data, clusterOptions = markerClusterOptions(),
             ~Longitude, ~Latitude)
#View
test_map

#Save widget
saveWidget(test_map, file = "C:/Users/AroRoseman/OneDrive - Morris Animal Foundation/Desktop/test_map.html")

