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

#Import data
datadir <- Sys.getenv("CONDITIONS_MAP_DATA_DIR")
file_path <- file.path(datadir, "input/CONDITIONSFINAL.CSV")

conditions <- read_csv(file_path)
conditions <- conditions %>% filter(row_number() <= n()-1)

#Remove NAs in state
conditions <- conditions[!is.na(conditions$PRIMSTATE),]

#Rename state column to match states file
colnames(conditions)[3] <- "name"

#Restrict zip codes to 5 digits
conditions5 <- conditions %>%
  mutate(PRIMZIP = str_pad(PRIMZIP, width = 5, pad = "0") %>%
           str_sub(1, 5))

#Subset dataset for unique IDs
unique_id <- conditions5 %>% distinct(subject_id, .keep_all = TRUE)

#Total number of dogs
dogs_total <- unique_id %>%
  group_by(name) %>%
  summarize(subject_id = n())
#There are 1775 unique ids

#Rename subject_id column
colnames(dogs_total)[2] <- "number"

###ATOPY
#Subset for atopy
atopy <- subset(unique_id, atopy ==1, select = c(1:4, 11))

#Count of atopy per state
atopy$atopy <- as.integer(atopy$atopy)
atopy_count <- atopy %>%
  group_by(name) %>%
  summarise(count = n())

#Merge total dogs with dogs with atopy
atopy_percent <- merge(atopy_count, dogs_total, by = "name", all.x = T)

#Calculate percentage by state
atopy_percent$percent <- (atopy_percent$count/atopy_percent$number)*100

#Download states shape file
states_atopy <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Remove states with 0 total dogs
states_atopy <- subset(states_atopy, !(name %in% c("Alaska", "District of Columbia", "Hawaii", "Puerto Rico")))

#Merge data set to shape file
atopy_states <- merge(states_atopy, atopy_percent, by = "name", all.x = T)

atopy_states$number <- dogs_total$number

#Replace NAs with 0s
atopy_states$count[is.na(atopy_states$count)] <- 0
atopy_states$percent[is.na(atopy_states$percent)] <- 0

#Add percentages to states shape file
states_atopy$percent <- atopy_states$percent

###BACTERIAL DERMATITIS
#Subset for bacterial dermatitis
bacterial_dermatitis <- subset(unique_id, bacterial_dermatitis == 1, select = c(1:4, 12))

#Count of bacterial dermatitis per state
bacterial_dermatitis$bacterial_dermatitis <- as.integer(bacterial_dermatitis$bacterial_dermatitis)
bacterial_dermatitis_count <- bacterial_dermatitis %>%
  group_by(name) %>%
  summarise(count = n())

#Merge total dogs with dogs with bacterial dermatitis
bacterial_dermatitis_percent <- merge(bacterial_dermatitis_count, dogs_total, by = "name", all.x = T)

#Calculate percentage by state
bacterial_dermatitis_percent$percent <- (bacterial_dermatitis_percent$count/bacterial_dermatitis_percent$number)*100

#Download states shape file
states_bacterial_dermatitis <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Remove states with 0 total dogs
states_bacterial_dermatitis <- subset(states_bacterial_dermatitis, !(name %in% c("Alaska", "District of Columbia", "Hawaii", "Puerto Rico")))

#Merge data set to shape file
bacterial_dermatitis_states <- merge(states_bacterial_dermatitis, bacterial_dermatitis_percent, by = "name", all.x = T)

bacterial_dermatitis_states$number <- dogs_total$number

#Replace NAs with 0s
bacterial_dermatitis_states$count[is.na(bacterial_dermatitis_states$count)] <- 0
bacterial_dermatitis_states$percent[is.na(bacterial_dermatitis_states$percent)] <- 0

#Add percentages to states shape file
states_bacterial_dermatitis$percent <- bacterial_dermatitis_states$percent

###CONTACT DERMATITIS
#Subset for contact_dermatitis
contact_dermatitis <- subset(unique_id, contact_dermatitis == 1, select = c(1:4, 13))

#Count of contact_dermatitis per state
contact_dermatitis$contact_dermatitis <- as.integer(contact_dermatitis$contact_dermatitis)
contact_dermatitis_count <- contact_dermatitis %>%
  group_by(name) %>%
  summarise(count = n())

#Merge total dogs with dogs with contact_dermatitis
contact_dermatitis_percent <- merge(contact_dermatitis_count, dogs_total, by = "name", all.x = T)

#Calculate percentage by state
contact_dermatitis_percent$percent <- (contact_dermatitis_percent$count/contact_dermatitis_percent$number)*100

#Download states shape file
states_contact_dermatitis <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Remove states with 0 total dogs
states_contact_dermatitis <- subset(states_contact_dermatitis, !(name %in% c("Alaska", "District of Columbia", "Hawaii", "Puerto Rico")))

#Merge data set to shape file
contact_dermatitis_states <- merge(states_contact_dermatitis, contact_dermatitis_percent, by = "name", all.x = T)

contact_dermatitis_states$number <- dogs_total$number

#Replace NAs with 0s
contact_dermatitis_states$count[is.na(contact_dermatitis_states$count)] <- 0
contact_dermatitis_states$percent[is.na(contact_dermatitis_states$percent)] <- 0

#Add percentages to states shape file
states_contact_dermatitis$percent <- contact_dermatitis_states$percent

###DERMATITIS
#Subset for dermatitis
dermatitis <- subset(unique_id, dermatitis == 1, select = c(1:4, 14))

#Count of dermatitis per state
dermatitis$dermatitis <- as.integer(dermatitis$dermatitis)
dermatitis_count <- dermatitis %>%
  group_by(name) %>%
  summarise(count = n())

#Merge total dogs with dogs with dermatitis
dermatitis_percent <- merge(dermatitis_count, dogs_total, by = "name", all.x = T)

#Calculate percentage by state
dermatitis_percent$percent <- (dermatitis_percent$count/dermatitis_percent$number)*100

#Download states shape file
states_dermatitis <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Remove states with 0 total dogs
states_dermatitis <- subset(states_dermatitis, !(name %in% c("Alaska", "District of Columbia", "Hawaii", "Puerto Rico")))

#Merge data set to shape file
dermatitis_states <- merge(states_dermatitis, dermatitis_percent, by = "name", all.x = T)

dermatitis_states$number <- dogs_total$number

#Replace NAs with 0s
dermatitis_states$count[is.na(dermatitis_states$count)] <- 0
dermatitis_states$percent[is.na(dermatitis_states$percent)] <- 0

#Add percentages to states shape file
states_dermatitis$percent <- dermatitis_states$percent

###HOT SPOTS
#Subset for hot_spots
hot_spots <- subset(unique_id, hot_spots == 1, select = c(1:4, 17))

#Count of hot_spots per state
hot_spots$hot_spots <- as.integer(hot_spots$hot_spots)
hot_spots_count <- hot_spots %>%
  group_by(name) %>%
  summarise(count = n())

#Merge total dogs with dogs with hot_spots
hot_spots_percent <- merge(hot_spots_count, dogs_total, by = "name", all.x = T)

#Calculate percentage by state
hot_spots_percent$percent <- (hot_spots_percent$count/hot_spots_percent$number)*100

#Download states shape file
states_hot_spots <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Remove states with 0 total dogs
states_hot_spots <- subset(states_hot_spots, !(name %in% c("Alaska", "District of Columbia", "Hawaii", "Puerto Rico")))

#Merge data set to shape file
hot_spots_states <- merge(states_hot_spots, hot_spots_percent, by = "name", all.x = T)

hot_spots_states$number <- dogs_total$number

#Replace NAs with 0s
hot_spots_states$count[is.na(hot_spots_states$count)] <- 0
hot_spots_states$percent[is.na(hot_spots_states$percent)] <- 0

#Add percentages to states shape file
states_hot_spots$percent <- hot_spots_states$percent

#Create labels for map
##ATOPY
labels_atopy <- sprintf(
  "<strong>%s</strong><br/>%d of %d dogs<br/>%0.0f%% of dogs",
  states_atopy$name, 
  atopy_states$count, atopy_states$number,
  states_atopy$percent
) %>% lapply(HTML)

##BACTERIAL DERMATITIS
labels_bacterial_dermatitis <- sprintf(
  "<strong>%s</strong><br/>%d of %d dogs<br/>%0.0f%% of dogs",
  states_bacterial_dermatitis$name, 
  bacterial_dermatitis_states$count, bacterial_dermatitis_states$number,
  states_bacterial_dermatitis$percent
) %>% lapply(HTML)

##CONTACT DERMATITIS
labels_contact_dermatitis <- sprintf(
  "<strong>%s</strong><br/>%d of %d dogs<br/>%0.0f%% of dogs",
  states_contact_dermatitis$name, 
  contact_dermatitis_states$count, contact_dermatitis_states$number,
  states_contact_dermatitis$percent
) %>% lapply(HTML)

##DERMATITIS
labels_dermatitis <- sprintf(
  "<strong>%s</strong><br/>%d of %d dogs<br/>%0.0f%% of dogs",
  states_dermatitis$name, 
  dermatitis_states$count, dermatitis_states$number,
  states_dermatitis$percent
) %>% lapply(HTML)

##HOT SPOTS
labels_hot_spots <- sprintf(
  "<strong>%s</strong><br/>%d of %d dogs<br/>%0.0f%% of dogs",
  states_hot_spots$name, 
  hot_spots_states$count, hot_spots_states$number,
  states_hot_spots$percent
) %>% lapply(HTML)

#Create color scheme
#(note: hot spots has the largest percentage range of all conditions, so the color scheme will be tied to hot spots)
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = states_hot_spots$percent)

#Create map
conditions_map_percent <- leaflet() %>%
  addTiles() %>%
  setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
  addPolygons(data = states_atopy,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~pal(percent),
              label = labels_atopy,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#E35205"),
              group = "Atopy"
  ) %>%
  addPolygons(data = states_bacterial_dermatitis,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~pal(percent),
              label = labels_bacterial_dermatitis,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#E35205"),
              group = "Bacterial Dermatitis"
  ) %>%
  addPolygons(data = states_contact_dermatitis,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~pal(percent),
              label = labels_contact_dermatitis,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#E35205"),
              group = "Contact Dermatitis"
  ) %>%
  addPolygons(data = states_dermatitis,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~pal(percent),
              label = labels_dermatitis,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#E35205"),
              group = "Dermatitis"
  ) %>%
  addPolygons(data = states_hot_spots,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~pal(percent),
              label = labels_hot_spots,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "#E35205"),
              group = "Hot Spots"
  ) %>%
  addLegend(pal = pal, values = states_hot_spots$percent,
            title = '<small>Percentage of dogs with skin condition per state</small>',
            position = 'bottomleft') %>%
   addLayersControl(baseGroups = c("Atopy", "Bacterial Dermatitis", "Contact Dermatitis", "Dermatitis", "Hot Spots"),
                   options = layersControlOptions(collapsed = TRUE))
conditions_map_percent

#Save widget
file_path_out <- file.path(datadir, "output/conditions_map_percent.html")

saveWidget(conditions_map_percent, file = file_path_out)
