library(osmdata)
library(ggplot2)
library(sf)
library(readr)
library(dplyr)
library(tidyr)
library(jsonlite)

bb_sf <- getbb(place_name = "greater london", format_out = "sf_polygon") 

osm_bike_sf <- opq(bbox = bb_sf) %>%                                  # select bounding box.
  add_osm_feature(key = 'amenity', value = 'bicycle_rental') %>%      # select keys and values.
  osmdata_sf() %>%                                                    # specify class (sf or sp).
  trim_osmdata(bb_poly = bb_sf)                                       # trim by the study region.   

osm_bike_sf
osm_bike_sf <- osm_bike_sf$osm_points # extract point only

osm_bike_sf <- osm_bike_sf %>% 
  filter(brand == "Santander Cycles")

# TfL API
tfl_bike_sf <- api_call <- fromJSON(readLines("https://api.tfl.gov.uk/bikepoint"))

# Make spatial.
tfl_bike_sf <- tfl_bike_sf %>%
  st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) 

# Plot comparison.
ggplot() + 
  geom_sf(data = bb_sf) +
  geom_sf(data = osm_bike_sf) +
  geom_sf(data = tfl_bike_sf, col = "red", alpha = 0.3)

