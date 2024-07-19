#Load libraries
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(htmltools)
library(readr)
library(geojsonio)
library(zipcodeR)
library(stringr)
library(htmlwidgets)

#datadir <- Sys.getenv("CONDITIONS_MAP_DATA_DIR")

setwd('C:/Users/AroRoseman/OneDrive - Morris Animal Foundation/Desktop/Projects/grls-conditions-map')

#Import data
conditions <- read_csv("./data/input/CONDITIONSFINAL.csv")

###ATOPY

#Subset for atopy
atopy <- subset(conditions, select = c(1:4, 11))

#Restrict zip codes to 5 digits
atopy5 <- atopy %>%
  mutate(PRIMZIP = str_pad(PRIMZIP, width = 5, pad = "0") %>%
           str_sub(1, 5))

#Subset to choose one row per dog (did dog have atopy ever?)
atopy1 <- subset(atopy5, atopy == 1)
atopy_ever <- atopy1 %>% distinct(subject_id, .keep_all = TRUE)

#Rename state column to "name" to match states sf
colnames(atopy_ever)[3] <- "name"

#Count of atopy by state
atopy_ever$atopy <- as.integer(atopy_ever$atopy)
atopy_count <- atopy_ever %>%
  group_by(name) %>%
  summarise(count = n())

#Download state shapes file
states_atopy <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Merge data set to shape file
atopy_states <- merge(states_atopy, atopy_count, by = "name", all.x = T)
atopy_states$count[is.na(atopy_states$count)] <- 0

#Create color scheme for atopy
paletteNum_atopy <- colorNumeric('Blues', domain = atopy_states$count)

### BACTERIAL DERMATITIS

#Subset for bacterial dermatitis
bacterial_dermatitis <- subset(conditions, select = c(1:4, 12))

#Restrict zip codes to 5 digits
bacterial_dermatitis5 <- bacterial_dermatitis %>%
  mutate(PRIMZIP = str_pad(PRIMZIP, width = 5, pad = "0") %>%
           str_sub(1, 5))

#Subset to choose one row per dog (did dog have bacterial dermatitis ever?)
bacterial_dermatitis1 <- subset(bacterial_dermatitis5, bacterial_dermatitis == 1)
bacterial_dermatitis_ever <- bacterial_dermatitis1 %>% distinct(subject_id, .keep_all = TRUE)

#Rename state column to "name" to match states sf
colnames(bacterial_dermatitis_ever)[3] <- "name"

#Count of bacterial dermatitis by state
bacterial_dermatitis_ever$bacterial_dermatitis <- as.integer(bacterial_dermatitis_ever$bacterial_dermatitis)
bacterial_dermatitis_count <- bacterial_dermatitis_ever %>%
  group_by(name) %>%
  summarise(count = n())

#Download state shapes file
states_bacterial_dermatitis <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Merge data set to shape file
bacterial_dermatitis_states <- merge(states_bacterial_dermatitis, bacterial_dermatitis_count, by = "name", all.x = T)
bacterial_dermatitis_states$count[is.na(bacterial_dermatitis_states$count)] <- 0

#Create color scheme for atopy
paletteNum_bacterial_dermatitis <- colorNumeric('YlOrRd', domain = bacterial_dermatitis_states$count)

### CONTACT DERMATITIS

#Subset for contact dermatitis
contact_dermatitis <- subset(conditions, select = c(1:4, 13))

#Restrict zip codes to 5 digits
contact_dermatitis5 <- contact_dermatitis %>%
  mutate(PRIMZIP = str_pad(PRIMZIP, width = 5, pad = "0") %>%
           str_sub(1, 5))

#Subset to choose one row per dog (did dog have contact dermatitis ever?)
contact_dermatitis1 <- subset(contact_dermatitis5, contact_dermatitis == 1)
contact_dermatitis_ever <- contact_dermatitis1 %>% distinct(subject_id, .keep_all = TRUE)

#Rename state column to "name" to match states sf
colnames(contact_dermatitis_ever)[3] <- "name"

#Count of contact dermatitis by state
contact_dermatitis_ever$contact_dermatitis <- as.integer(contact_dermatitis_ever$contact_dermatitis)
contact_dermatitis_count <- contact_dermatitis_ever %>%
  group_by(name) %>%
  summarise(count = n())

#Download state shapes file
states_contact_dermatitis <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Merge data set to shape file
contact_dermatitis_states <- merge(states_contact_dermatitis, contact_dermatitis_count, by = "name", all.x = T)
contact_dermatitis_states$count[is.na(contact_dermatitis_states$count)] <- 0

#Create color scheme for contact dermatitis
paletteNum_contact_dermatitis <- colorNumeric('Greens', domain = contact_dermatitis_states$count)

### DERMATITIS

#Subset for dermatitis
dermatitis <- subset(conditions, select = c(1:4, 14))

#Restrict zip codes to 5 digits
dermatitis5 <- dermatitis %>%
  mutate(PRIMZIP = str_pad(PRIMZIP, width = 5, pad = "0") %>%
           str_sub(1, 5))

#Subset to choose one row per dog (did dog have dermatitis ever?)
dermatitis1 <- subset(dermatitis5, dermatitis == 1)
dermatitis_ever <- dermatitis1 %>% distinct(subject_id, .keep_all = TRUE)

#Rename state column to "name" to match states sf
colnames(dermatitis_ever)[3] <- "name"

#Count of dermatitis by state
dermatitis_ever$dermatitis <- as.integer(dermatitis_ever$dermatitis)
dermatitis_count <- dermatitis_ever %>%
  group_by(name) %>%
  summarise(count = n())

#Download state shapes file
states_dermatitis <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Merge data set to shape file
dermatitis_states <- merge(states_dermatitis, dermatitis_count, by = "name", all.x = T)
dermatitis_states$count[is.na(dermatitis_states$count)] <- 0

#Create color scheme for dermatitis
paletteNum_dermatitis <- colorNumeric('viridis', domain = dermatitis_states$count)

### HOT SPOTS

#Subset for hot spots
hot_spots <- subset(conditions, select = c(1:4, 17))

#Restrict zip codes to 5 digits
hot_spots5 <- hot_spots %>%
  mutate(PRIMZIP = str_pad(PRIMZIP, width = 5, pad = "0") %>%
           str_sub(1, 5))

#Subset to choose one row per dog (did dog have hot spots ever?)
hot_spots1 <- subset(hot_spots5, hot_spots == 1)
hot_spots_ever <- hot_spots1 %>% distinct(subject_id, .keep_all = TRUE)

#Rename state column to "name" to match states sf
colnames(hot_spots_ever)[3] <- "name"

#Count of hot spots by state
hot_spots_ever$hot_spots <- as.integer(hot_spots_ever$hot_spots)
hot_spots_count <- hot_spots_ever %>%
  group_by(name) %>%
  summarise(count = n())

#Download state shapes file
states_hot_spots <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Merge data set to shape file
hot_spots_states <- merge(states_hot_spots, hot_spots_count, by = "name", all.x = T)
hot_spots_states$count[is.na(hot_spots_states$count)] <- 0

#Create color scheme for hot spots
paletteNum_hot_spots <- colorNumeric('Oranges', domain = hot_spots_states$count)

#Create labels for map
##ATOPY
labels_atopy <- sprintf(
  "<strong>%s</strong><br/>%g dogs",
  atopy_states$name, atopy_states$count
) %>% lapply(HTML)

##BACTERIAL DERMATITIS
labels_bacterial_dermatitis <- sprintf(
  "<strong>%s</strong><br/>%g dogs",
  bacterial_dermatitis_states$name, bacterial_dermatitis_states$count
) %>% lapply(HTML)

##CONTACT DERMATITIS
labels_contact_dermatitis <- sprintf(
  "<strong>%s</strong><br/>%g dogs",
  contact_dermatitis_states$name, contact_dermatitis_states$count
) %>% lapply(HTML)

##DERMATITIS
labels_dermatitis <- sprintf(
  "<strong>%s</strong><br/>%g dogs",
  dermatitis_states$name, dermatitis_states$count
) %>% lapply(HTML)

##HOT SPOTS
labels_hot_spots <- sprintf(
  "<strong>%s</strong><br/>%g dogs",
  hot_spots_states$name, hot_spots_states$count
) %>% lapply(HTML)

#Create map
conditions_map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
  addPolygons(data = atopy_states,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum_atopy(count),
              label = labels_atopy,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "dodgerblue"),
              group = "Atopy"
  ) %>%
  addPolygons(data = bacterial_dermatitis_states,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum_bacterial_dermatitis(count),
              label = labels_bacterial_dermatitis,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "yellow"),
              group = "Bacterial Dermatitis"
  ) %>%
  addPolygons(data = contact_dermatitis_states,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum_contact_dermatitis(count),
              label = labels_contact_dermatitis,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "green"),
              group = "Contact Dermatitis"
  ) %>%
  addPolygons(data = dermatitis_states,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum_dermatitis(count),
              label = labels_dermatitis,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "purple"),
              group = "Dermatitis"
  ) %>%
  addPolygons(data = hot_spots_states,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum_hot_spots(count),
              label = labels_hot_spots,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "orange"),
              group = "Hot Spots"
  ) %>%
  addLegend(pal = paletteNum_atopy, values = atopy_states$count,
            title = '<small>Number of dogs with atopy per state</small>',
            position = 'bottomleft',  group = "Atopy") %>%
  addLegend(pal = paletteNum_bacterial_dermatitis, values = bacterial_dermatitis_states$count,
            title = '<small>Number of dogs with bacterial dermatitis per state</small>',
            position = 'bottomleft',  group = "Bacterial Dermatitis") %>%
  addLegend(pal = paletteNum_contact_dermatitis, values = contact_dermatitis_states$count,
            title = '<small>Number of dogs with contact dermatitis per state</small>',
            position = 'bottomleft', group = "Contact Dermatitis") %>%
  addLegend(pal = paletteNum_dermatitis, values = dermatitis_states$count,
            title = '<small>Number of dogs with dermatitis per state</small>',
            position = 'bottomleft', group = "Dermatitis") %>%
  addLegend(pal = paletteNum_hot_spots, values = hot_spots_states$count,
            title = '<small>Number of dogs with hot spots per state</small>',
            position = 'bottomleft', group = "Hot Spots") %>%
  addLayersControl(overlayGroups = c("Atopy", "Bacterial Dermatitis", "Contact Dermatitis", "Dermatitis", "Hot Spots"),
                   options = layersControlOptions(collapsed = TRUE))
conditions_map

#Save widget
saveWidget(conditions_map, file = "./data/output/conditions_map.html")
