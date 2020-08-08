#Waffle map
library(tidyverse)
library(sf) #functions: st_read and st_centroid
library(here)
#library(ggforce)

world_map <- map_data("world")


# Basic world map ---------------------------------------------------------
map_plot <- ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group),
               fill = "grey30", colour = "grey45", size = 0.1) + 
  xlab('Longitude') +
  ylab('Latitute') +
  coord_fixed() +
  theme_void(base_size=14)

map_plot 

# Get centroids of each LME (may not need if waffles are cowplotted in)
load("LME_centroids.RData") #dataframe: sf_cent
spatial_ffs <- sf_cent %>%
  right_join(ffs,by=c('LME_NUMBER'="LME"))

coords <- spatial_ffs %>%
  st_coordinates() %>%
  as.data.frame()

# Get ff mse info
ffs <- read.csv(here('data','fishdata','ffs_063020.csv'))
length(unique(ffs$LME))

