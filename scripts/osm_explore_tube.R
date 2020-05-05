library(jsonlite)
library(osmdata)
library(ggplot2)
library(sf)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(cowplot) # maybe don't show?
library(stringr)

# OSM query ==============================================================================================

# All queries begin with a bounding box specification i.e. the study region.
# This can be obtained manually, which requires some existing knowledge about
# an area using the latitude and longitude coordinates, but it is generally
# easier to use a search term. For instance

bb_sf <- getbb(place_name = "greater london united kingdom", format_out = "sf_polygon") 

osm_stat_sf <- opq(bbox = bb_sf) %>%                               # select bounding box
  add_osm_feature(key = 'public_transport', value = 'station') %>% # select features
  osmdata_sf() %>%                                                 # specify class (sf or sp)
  trim_osmdata(bb_poly = bb_sf)                                    # trim by bounding box  

# View contents.
osm_stat_sf

# Extract points only.
osm_stat_sf <- osm_stat_sf$osm_points 

# Check the network tags.
table(osm_stat_sf$network)
table(osm_stat_sf$operator)

# Filter any that include "London Underground".
osm_tube_sf <- osm_stat_sf %>% 
  filter(str_detect(network, "London Underground"))

osm_tube_sf <- st_transform(osm_tube_sf, 27700)
bb_sf      <- st_transform(bb_sf, 27700)

ggplot() +
  geom_sf(data = bb_sf) +
  geom_sf(data = osm_tube_sf, mapping = aes(colour = line))

# TfL scrape ==============================================================================================

# Scape bus stops on route 24 from TfL. Warning is fine.
api_call <- fromJSON(readLines("https://api.tfl.gov.uk/Line/Meta/Modes"))



api_call <- fromJSON(readLines("https://api.tfl.gov.uk/line/northern/route/sequence/outbound"))

# Extract bus stop names and the lat-long coordinates, transform to BNG.
tfl_bus91_df <- api_call %>% 
  pluck("stations") %>% 
  select(name, lat, lon) %>% 
  st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) %>% 
  st_transform(27700)

# Plot difference.
ggplot() +
  geom_sf(data = tfl_bus91_df, size = 2) +
  geom_sf(data = osm_bus_sf, col = "red", alpha = 0.5) +
  theme_minimal()

# Keep only one of the OSM records. This is a scenario where we don't have the 'real' alternative.
osm_bus91_dup_sf <- osm_bus91_sf %>% 
  group_by(name) %>% 
  slice(1)

# Plot difference.
ggplot() +
  geom_sf(data = tfl_bus91_df, size = 2) +
  geom_sf(data = osm_bus91_dup_sf, col = "red", alpha = 0.8) +
  theme_minimal()

  