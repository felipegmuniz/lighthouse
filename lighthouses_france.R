# To clean data
library(tidyverse)
library(lubridate)
library(janitor)
# To scrape data
library(rvest)
library(httr)
library(polite)

# To map data
library(leaflet)
# To geocode
library(ggmap)


# Read the table from wikipedia
url <- "https://en.wikipedia.org/wiki/List_of_lighthouses_in_France"
  
# Get permission to scrape URL  
url_bow <- polite::bow(url)
url_bow

# Obtain the piece of the web page
lh_table <-
  polite::scrape(url_bow) %>%  # scrape web page
  rvest::html_nodes("table.wikitable") %>% # pull out specific table
  rvest::html_table(fill = TRUE) 

# Convert lh_table from list to data frame and remove unnecessary columns Image and Notes
lh_table <- data.frame(lh_table)

lh_table <- lh_table[, -c(2, 7)]
str(lh_table)

# Create a list of the location & coordinates column
##lh_coord <- lh_table[,4]

# Extract the latitudes from lh_coord
lh_lats <- str_extract_all(lh_coord, "([0-9]{2}).([0-9]{4,7});")
lh_lons <- str_extract_all(lh_coord, "-([0-9]{1,2}).([0-9]{4,7})$")

# Remove alien characters from latitude values
lh_lats <- str_extract_all(lh_lats, "([0-9]{2}).([0-9]{4,7})")

# Remove Location & Coordinates column from lh_table
lh_table <- lh_table[, -4]
lh_table_name <- str_remove_all(lh_table$Name, "\\[[^\\]\\[]*]")
lh_table_year <- str_remove_all(lh_table$Year.built, "\\[[^\\]\\[]*]")
lh_table <- lh_table %>%
  mutate(Name = lh_table_name, Year.built = lh_table_year)

# Fix the names of observations 1 and 11 to allow geocode

lh_table[11,1] <- "Phare du Four"
lh_table[3,1] <- "Phare de la Giraglia"
lh_table[15,1] <- "Phare de la Jument"
lh_table[6,1] <- "Roches-Douvres, France"
lh_table[8,1] <- "Triagoz"
lh_table[9,1] <- "Phare d'Ar-men, îlle-de-Sein, France"
lh_table[18,1] <- "Phare de La Vieille"
lh_table[24,1] <- "Phare des Pierres Noires, Finistère, France"
lh_table[29,1] <- "Phare du Four"
lh_table[36,1] <- "Phare du Risban"
lh_table[41,1] <- "Pierre-de-Herpin, France"


# Geocode lighthouse locations with lats and lons
lh_coord <- geocode(lh_table$Name)

# Insert geocoded lat and lon values to lh_table
lh_table <- lh_table %>%
  mutate(Latitude = lh_coord$lat, Longitude = lh_coord$lon)

lh_table[6,5] <- 49.11696108425436
lh_table[6,6]<- -2.8166881826868746

# Create map with geocoded lighthouses in France
lh_map <- leaflet() %>%
    addProviderTiles("Esri") %>%
    clearMarkers() %>%
    addCircleMarkers(data = lh_table, 
                     radius = 2,
                     color = "red",
                     popup = ~paste0(
                       "<b>",Name,"</b>","<br/>",
                       Lat = lh_table$Latitude,"<br/>", 
                       Lon = lh_table$Longitude))

lh_map
