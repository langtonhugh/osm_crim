library(jsonlite)
library(osmdata)
library(ggplot2)
library(sf)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(patchwork)

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
temp <- osm_stat_sf$osm_polygons
osm_stat_sf <- osm_stat_sf$osm_points

  
  
# Check the network tags.
table(osm_stat_sf$network)

# Filter Northern line only/
osm_north_sf <- osm_stat_sf %>% 
  filter(line == "Northern")

osm_north_sf <- st_transform(osm_north_sf, 27700)
bb_sf      <- st_transform(bb_sf, 27700)

ggplot() +
  geom_sf(data = bb_sf) +
  geom_sf(data = osm_north_sf, size = 0.8)

# TfL scrape ==============================================================================================

# Scape northern line locations from TfL. Warning is fine.
api_call <- fromJSON(readLines("https://api.tfl.gov.uk/line/northern/stoppoints"))

# Extract bus stop names and the lat-long coordinates, transform to BNG.
tfl_north_sf <- api_call %>% 
  select(commonName, lat, lon) %>% 
  st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) %>% 
  st_transform(27700)

# Plot difference.
ggplot() +
  geom_sf(data = osm_north_sf) +
  geom_sf(data = tfl_north_sf, color = "red", alpha = 0.5) 

# Assessing the impact in terms of crime. BTP data in January 2020.
btp_df <- read_csv("data/2020-01-btp-street.csv")

# Make spatial.
btp_sf <- btp_df %>%
  drop_na(Longitude, Latitude) %>%
  st_as_sf(coords = c(x = "Longitude", y = "Latitude"), crs = 4326) %>%
  st_transform(27700) %>%
  st_intersection(bb_sf)

# Create buffers to define 'in and around'.
osm_buff_sf <- st_buffer(osm_north_sf, dist = 50)
tfl_buff_sf <- st_buffer(tfl_north_sf, dist = 50)

# Aggregate to each station.
osm_north_sf <- osm_buff_sf %>% 
  mutate(crimes = lengths(st_intersects(osm_buff_sf, btp_sf)))

tfl_north_sf <- tfl_buff_sf %>% 
  mutate(crimes = lengths(st_intersects(tfl_buff_sf, btp_sf)))

# Map out difference.
p1 <- ggplot(data = osm_north_sf) +
  geom_sf(mapping = aes(colour = crimes), size = 3) +
  labs(title = "Open Street Map") +
  scale_colour_viridis_c()
  
p2 <- ggplot(data = tfl_north_sf) +
  geom_sf(mapping = aes(colour = crimes), size = 3) +
  labs(title = "Transport for London") +
  scale_colour_viridis_c()

p1 + p2