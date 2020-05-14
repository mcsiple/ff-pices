# NewMap
library(tidyverse)
library(sf) #functions: st_read and st_centroid
#library(ggforce)

world_map <- map_data("world")


# Basic world map ---------------------------------------------------------
map_plot <- ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group), fill = "grey30", colour = "grey45", size = 0.1) + 
  theme_classic() +
  xlab('Longitude') +
  ylab('Latitute')

map_plot 

# Get shapefiles of LMEs --------------------------------------------------
lmes <- sf::st_read(here::here('data','LMEs66','LMEs66.shp')) #%>%
  #st_transform(32617) # not sure if I need the transform

# Look at LME data
class(lmes)
names(lmes)

lmes_simple <- lmes %>% 
  st_simplify() # simplify so it loads faster
sf_cent <- st_centroid(lmes) # get centroids of LMEs 


# Get shapefiles of ICES management areas ---------------------------------
ices <- sf::st_read(here::here('data','ICES_ecoregions','ICES_ecoregions_20171207_erase_ESRI.shp'))
class(ices)
names(ices)

# Merge LME data with stock data ------------------------------------------
# This is what needs to happen next!




# Add LME centroids to world map
map_plot +
  geom_sf(data=sf_cent,aes(colour = Shape_Area))
  #geom_sf(data=lmes_simple,colour = 'pink') + #only need to do this 1x to check that centroids are in the right place



# Other centroid options --------------------------------------------------
#Using rgeos:
# centroids <- rgeos::gCentroid(lmes,byid=TRUE)
# plot(lmes)
# points(coordinates(lmes),pch=1)
# points(centroids,pch=2)