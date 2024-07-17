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

##Add state name to dataframe
#get_state_from_zip <- function(zip_code) {
 # zip_info <- reverse_zipcode(zip_code)
  #return(zip_info$state)
#}

#atopy_name <- atopy_ever %>%
 # rowwise() %>%
  #mutate(state = get_state_from_zip((zipcode)))

#state_lookup <- data.frame(
 # state_code = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  #state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
#)

#atopy_name <- atopy_name %>%
 # left_join(state_lookup, by = c("state" = "state_code"))

#atopy_name$name <- atopy_name$state_name

#atopy_name <- atopy_name %>%
 # select(`dog code`, lat, long, atopy, name)

#Rename state column to "name" to match states sf
colnames(atopy_ever)[3] <- "name"

#Download state shapes file
states_atopy <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Remove Puerto Rico because it's causing problems for some reason
states_atopy <- states[states$name != "Puerto Rico",]

#Merge data set to shape file
#is.element(atopy_name$name, states$name) %>%
#  all()

atopy_states <- merge(states_atopy, atopy_ever, by = "name", all.x = T)

#Count of atopy by state
atopy_states$atopy <- factor(atopy_states$atopy)
state_counts_atopy <- table(atopy_states$name, atopy_states$atopy)
counts_atopy <- state_counts_atopy[,"1"]
all_states_atopy <- unique(atopy_states$name)
counts_atopy_df <- data.frame(state = all_states_atopy,
                              count = as.integer(counts_atopy))
counts_atopy_df$count[is.na(counts_atopy_df$count)] <- 0

#Add count of atopy to states shapes file
states_atopy$count <- counts_atopy_df$count

##Merge count with rest of dataframe
#atopy_finalcount <- merge(atopy_count, atopy_states, by = "name")
#atopy_finalcount$count <- atopy_finalcount$n

##Remove duplicate states
#atopy_final <- atopy_finalcount %>% distinct(name, .keep_all = TRUE)

#atopy_final <- atopy_final %>%
 # select(lat, long, name, count)

#Create color scheme for atopy
paletteNum_atopy <- colorNumeric('Blues', domain = states_atopy$count)

#Create base map
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
  addPolygons(data = states_atopy,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum_atopy(count))
m

#Add labels and legend
labels <- sprintf(
  "<strong>%s</strong><br/>%g dogs",
  states_atopy$name, states_atopy$count
) %>% lapply(HTML)

m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
  addPolygons(data = states_atopy,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum_atopy(count),
              label = labels,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "dodgerblue"
              )
              ) %>%
  addLegend(pal = paletteNum_atopy, values = states_atopy$count,
            title = '<small>Number of dogs with atopy per state</small>',
            position = 'bottomleft')
m

#Save widget
saveWidget(m, file = "./data/output/conditions_map.html")


### BACTERIAL_DERMATITIS

#Subset for bacterial_dermatitis
bacterial_dermatitis <- subset(conditions, select = c(1:4, 12))
bacterial_dermatitis1 <- subset(bacterial_dermatitis, bacterial_dermatitis == 1)

#Restrict zip codes to 5 digits
bacterial_dermatitis5 <- bacterial_dermatitis1 %>%
  mutate(PRIMZIP = str_pad(PRIMZIP, width = 5, pad = "0") %>%
           str_sub(1, 5))

#Subset to choose one row per dog (did dog have bacterial dermatitis ever?)
bacterial_dermatitis_ever <- bacterial_dermatitis5 %>% distinct(subject_id, .keep_all = TRUE)

#Rename state column to "name" to match states sf
bacterial_dermatitis_ever$name <- bacterial_dermatitis_ever$PRIMSTATE

#Download state shapes file
states_bd <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Remove Puerto Rico because it's causing problems for some reason
states_bd <- states[states$name != "Puerto Rico",]

#Merge data set to shape file
bacterial_dermatitis_states <- merge(states_bd, bacterial_dermatitis_ever, by = "name", all.x = T)

#Count of bacterial_dermatitis by state
bacterial_dermatitis_states$bacterial_dermatitis <- factor(bacterial_dermatitis_states$bacterial_dermatitis)
state_counts_bd <- table(bacterial_dermatitis_states$name, bacterial_dermatitis_states$bacterial_dermatitis)
counts_bd <- state_counts_bd[,"1"]
all_states_bd <- unique(bacterial_dermatitis_states$name)
counts_bd_df <- data.frame(state = all_states_bd,
                              count = as.integer(counts_bd))
counts_bd_df$count[is.na(counts_bd_df$count)] <- 0

#Add count of bacterial_dermatitis to states shapes file
states_bd$count <- counts_bd_df$count

#Create color scheme for bacterial_dermatitis
paletteNum_bd <- colorNumeric('YlOrRd', domain = states_bd$count)

#Update map and labels to include atopy and add bacterial dermatitis
#Add labels
labels_atopy <- sprintf(
  "<strong>%s</strong><br/>%g dogs",
  states_atopy$name, states_atopy$count
) %>% lapply(HTML)

labels_bd <- sprintf(
  "<strong>%s</strong><br/>%g dogs",
  states_bd$name, states_bd$count
) %>% lapply(HTML)

#Map
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
  addPolygons(data = states_atopy,
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
  addPolygons(data = states_bd,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum_bd(count),
              label = labels_bd,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "orange"),
              group = "Bacterial Dermatitis"
              ) %>%
  addLegend(pal = paletteNum_atopy, values = states_atopy$count,
            title = '<small>Number of dogs with atopy per state</small>',
            position = 'bottomleft',  group = "Atopy") %>%
  addLegend(pal = paletteNum_bd, values = states_bd$count,
            title = '<small>Number of dogs with bacterial dermatitis per state</small>',
            position = 'bottomleft',  group = "Bacterial Dermatitis") %>%
  addLayersControl(baseGroups = c("Atopy", "Bacterial Dermatitis"),
                   options = layersControlOptions(collapsed = TRUE))
m
