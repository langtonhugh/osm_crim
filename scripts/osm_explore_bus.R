library(jsonlite)
library(osmdata)
library(ggplot2)
library(sf)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(cowplot) # maybe don't show?

# OSM query ==============================================================================================

# All queries begin with a bounding box specification i.e. the study region.
# This can be obtained manually, which requires some existing knowledge about
# an area using the latitude and longitude coordinates, but it is generally
# easier to use a search term. For instance

bb_sf <- getbb(place_name = "greater london united kingdom", format_out = "sf_polygon") 

osm_bus_sf <- opq(bbox = bb_sf) %>%                                    # select bounding box
  add_osm_feature(key = 'highway', value = 'bus_stop') %>% # select features
  osmdata_sf() %>%                                         # specify class (sf or sp)
  trim_osmdata(bb_poly = bb_sf)

osm_bus_sf
osm_bus_sf <- osm_bus_sf$osm_points # extract point only

osm_bus_sf <- st_transform(osm_bus_sf, 27700)
bb_sf      <- st_transform(bb_sf, 27700)

ggplot() +
  geom_sf(data = bb_sf) +
  geom_sf(data = osm_bus_sf, size = 0.3) 

# TfL scrape ==============================================================================================

# Scape bus stops on route 24 from TfL. Warning is fine.
api_call <- fromJSON(readLines("https://api.tfl.gov.uk/line/91/route/sequence/outbound"))

# Extract bus stop names and the lat-long coordinates, transform to BNG.
tfl_bus91_df <- api_call %>% 
  pluck("stations") %>% 
  select(name, lat, lon) %>% 
  st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) %>% 
  st_transform(27700)

# Check names.
osm_bus91_sf <- osm_bus_sf %>% 
  filter(name %in% tfl_bus91_df$name) 

# Plot difference.
ggplot() +
  geom_sf(data = tfl_bus91_df, size = 2) +
  geom_sf(data = osm_bus91_sf, col = "red", alpha = 0.5) +
  theme_minimal()

# Keep only one of the OSM records. This is a scenario where we don't have the 'real' alternative.
osm_bus91_dup_sf <- osm_bus91_sf %>% 
  group_by(name) %>% 
  slice(1) %>% 
  ungroup()

# Plot difference.
ggplot() +
  geom_sf(data = tfl_bus91_df, size = 2) +
  geom_sf(data = osm_bus91_dup_sf, col = "red", alpha = 0.8) +
  theme_minimal()

# Load crime data.
crime_df <- read_csv("data/2020-01-metropolitan-street.csv")

# Make spatial.
crime_sf <- crime_df %>% 
  drop_na(Latitude, Longitude) %>% 
  st_as_sf(coords = c(x = "Longitude", y = "Latitude"), crs = 4326) %>% 
  st_transform(27700)

# Create buffers around each bus stop.
osm_bus_buf_sf <- st_buffer(osm_bus91_dup_sf, 70)
tfl_bus_buf_sf <- st_buffer(tfl_bus91_df, 70)

# Aggregate for each.
osm_bus_buf_sf <- osm_bus_buf_sf %>% 
  mutate(crimes = lengths(st_intersects(osm_bus_buf_sf, crime_sf)))

tfl_bus_buf_sf <- tfl_bus_buf_sf %>% 
  mutate(crimes = lengths(st_intersects(tfl_bus_buf_sf, crime_sf)))

# Frequencies.
table(osm_bus_buf_sf$crimes)
table(tfl_bus_buf_sf$crimes)

# Plot.
p1 <- ggplot(data = tfl_bus_buf_sf) +
  geom_sf(mapping = aes(fill = crimes), colour = "transparent") +
  scale_fill_viridis_c() +
  theme_minimal()
  
p2 <- ggplot(data = osm_bus_buf_sf) +
  geom_sf(mapping = aes(fill = crimes), colour = "transparent") +
  scale_fill_viridis_c() +
  theme_minimal()

plot_grid(p1, p2, ncol = 2)
