#Load libraries
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(htmltools)
library(readr)
library(geojsonio)
library(zipcodeR)
library(stringr)

#datadir <- Sys.getenv("CONDITIONS_MAP_DATA_DIR")

#Import data
conditions <- read_csv("/workspace/data/input/CONDITIONSFINAL.csv")

#Subset for atopy
atopy <- subset(conditions, select = c(1:5))

#Restrict zip codes to 5 digits
atopy5 <- atopy %>%
  mutate(zipcode = str_pad(zipcode, width = 5, pad = "0") %>%
           str_sub(1, 5))

#Subset to choose one row per dog (did dog have atopy ever?)
atopy1 <- subset(atopy5, atopy == 1)
atopy_ever <- atopy1 %>% distinct(`dog code`, .keep_all = TRUE)

#Add state name to dataframe
get_state_from_zip <- function(zip_code) {
  zip_info <- reverse_zipcode(zip_code)
  return(zip_info$state)
}

atopy_name <- atopy_ever %>%
  rowwise() %>%
  mutate(state = get_state_from_zip((zipcode)))

state_lookup <- data.frame(
  state_code = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  state_name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
)

atopy_name <- atopy_name %>%
  left_join(state_lookup, by = c("state" = "state_code"))

atopy_name$name <- atopy_name$state_name

atopy_name <- atopy_name %>%
  select(`dog code`, lat, long, atopy, name)

#Download state shapes file
states <- geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

#Remove Puerto Rico because it's causing problems for some reason
states <- states[states$name != "Puerto Rico",]

#Merge data set to shape file
#is.element(atopy_name$name, states$name) %>%
#  all()

atopy_states <- merge(states, atopy_name, by = "name", all.x = T)

#Count of atopy by state
atopy_states$atopy <- factor(atopy_states$atopy)
state_counts <- table(atopy_states$name, atopy_states$atopy)
counts <- state_counts[,"1"]
all_states <- unique(atopy_states$name)
counts <- counts[match(all_states, names(counts))]
counts[is.na(counts)] <- 0
counts_df <- data.frame(state=names(counts), count = as.integer(counts))


#Add count of atopy to states shapes file
states$count <- counts_df$count

##Merge count with rest of dataframe
#atopy_finalcount <- merge(atopy_count, atopy_states, by = "name")
#atopy_finalcount$count <- atopy_finalcount$n

##Remove duplicate states
#atopy_final <- atopy_finalcount %>% distinct(name, .keep_all = TRUE)

#atopy_final <- atopy_final %>%
 # select(lat, long, name, count)

#Create color scheme
paletteNum <- colorNumeric('Blues', domain = states$count)

#Create base map
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
  addPolygons(data = states,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum(count))
m

#Add labels and legend
labels <- sprintf(
  "<strong>%s</strong><br/>%g dogs",
  states$name, states$count
) %>% lapply(HTML)

m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -96.25, lat = 39.50, zoom = 4) %>%
  addPolygons(data = states,
              color = "white",
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum(count),
              label = labels,
              labelOptions = labelOptions(
                style = list(color = "gray30"),
                textsize = "10px"),
              highlightOptions = highlightOptions(
                weight = 3,
                color = "dodgerblue"
              )
              ) %>%
  addLegend(pal = paletteNum, values = states$count,
            title = '<small>Number of dogs with atopy per state</small>',
            position = 'bottomleft')
m

#Save widget
saveWidget(conditions_map, file = "/workspace/data/output/conditions_map.html")