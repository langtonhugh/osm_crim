library(osmdata)
library(ggplot2)
library(sf)
library(readr)
library(dplyr)
library(tidyr)

# All queries begin with a bounding box specification i.e. the study region.
# This can be obtained manually, which requires some existing knowledge about
# an area using the latitude and longitude coordinates, but it is generally
# easier to use a search term. For instance

bb <- getbb(place_name = "greater london", format_out = "sf_polygon") 

p <- opq(bbox = bb) %>%                                           # select bounding box
  add_osm_feature(key = 'amenity', value = 'bicycle_rental') %>%
  osmdata_sf() %>%                                                    # specify class (sf or sp)
  trim_osmdata(bb_poly = bb)   

p
p.sf <- p$osm_points # extract point only

ggplot() +
  geom_sf(data = bb) +
  geom_sf(data = p.sf)

tfl_bike <- api_call <- fromJSON(readLines("https://api.tfl.gov.uk/bikepoint"))

tfl_bike_sf <- tfl_bike %>% 
  st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326)



p.sf <- st_transform(p.sf, 4326)

ggplot() + 
  geom_sf(data = bb) +
  geom_sf(data = tfl_bike_sf) +
  geom_sf(data = p.sf, col = "Red", alpha = 0.2)
  
  
ggplot(p.sf) + geom_sf()

ggplot() +
  geom_sf(data = bb) +
  geom_sf(data = tfl_bike_sf) +


# crime data
crime.df <- read_csv("data/2020-01-metropolitan-street.csv")

bike.crime.sf <- crime.df %>% 
  filter(`Crime type` == "Bicycle theft") %>% 
  drop_na(Longitude, Latitude) %>% 
  st_as_sf(coords = c(x = "Longitude", y = "Latitude"), crs = 4326) %>% 
  st_transform(27700) %>% 
  st_intersection(bb.sf)

# plot
ggplot() +
  geom_sf(data = bb) +
  geom_sf(data = p.clip.sf, size = 0.8) +
  geom_sf(data = bike.crime.sf, size = 0.8, col = "red")

# polygons
bb <- getbb(place_name = "coventry united kingdom", format_out = "sf_polygon") 

p <- opq(bbox = bb) %>%                                        
  add_osm_feature(key = 'public_transport', value = 'station') %>% 
  osmdata_sf() %>% 
  trim_osmdata(bb_poly = bb)

p
p.sf <- p$osm_polygons

ggplot() +
  geom_sf(data = bb) +
  geom_sf(data = p.sf, fill = "black")