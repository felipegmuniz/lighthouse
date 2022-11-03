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

options(digits = 9)

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
lh_coord <- lh_table[,4]

# Extract the latitudes from lh_coord
lh_lats <- str_extract_all(lh_coord, "([0-9]{2}).([0-9]{4,7});")
lh_lons <- str_extract_all(lh_coord, "-([0-9]{1,2}).([0-9]{4,7})$")

# Remove alien characters from latitude values
lh_lats <- str_extract_all(lh_lats, "([0-9]{2}).([0-9]{4,7})")

# Remove Location & Coordinates column from lh_table
lh_table <- lh_table[, -4]

# Insert Lat and Lon columns from lh_lats and lh_lons converted as numeric
lh_table <- lh_table %>% add_column(Latitude = as.numeric(lh_lats), 
                                    Longitude = as.numeric(lh_lons))

# Filter observations with missing lats or lons
lh_uncoded <- lh_table %>% filter(is.na(Longitude) & is.na(Latitude))

# Fix the name of observation 11 to "Phare du Four"
lh_uncoded[11,1] <- "Phare du Four"
lh_uncoded[1,1] <- "Phare de la Giraglia"

# Geocode missing values for lat and lon
lh_uncoded_fixed <- geocode(lh_uncoded$Name)

# Update missing values of lats and lons to lh_uncoded
lh_uncoded <- lh_uncoded %>% 
  mutate(Latitude = lh_uncoded_fixed$lat, Longitude = lh_uncoded_fixed$lon)

# Update lh_table with lh_uncoded
lh_table <- lh_uncoded

as_tibble(lh_table)



lighthouse_france <- data.frame(lh_names, lh_lats, lh_lngs) %>%
  rename(Name = lh_names, Latitude = lh_lats, Longitude = lh_lngs)

lh_map <- leaflet() %>%
    addProviderTiles("CartoDB") %>%
    addMarkers(data = lighthouse_france)

lh_map
