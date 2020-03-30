library(osmdata)
library(ggplot2)
library(sf)

# All queries begin with a bounding box specification i.e. the study region.
# This can be obtained manually, which requires some existing knowledge about
# an area using the latitude and longitude coordinates, but it is generally
# easier to use a search term.

bb <- getbb(place_name = "manchester united kingdom", format_out = "sf_polygon") 

ggplot(data = bb) +
  geom_sf()


p <- opq(bbox = bb) %>%                                  # select bounding box
  add_osm_feature(key = 'amenity', value = 'pub') %>%    # select features
  osmdata_sf() %>%                                       # specify class
  trim_osmdata (bb)                                            

class(p)
p.sf <- p$osm_points

p.clip.sf <- st_intersection(bb, p.sf)

ggplot() +
  geom_sf(data = bb) +
  geom_sf(data = p.df)
