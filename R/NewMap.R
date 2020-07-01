# NewMap
library(tidyverse)
library(sf) #functions: st_read and st_centroid
library(here)
#library(ggforce)

world_map <- map_data("world")


# Basic world map ---------------------------------------------------------
map_plot <- ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group), fill = "grey30", colour = "grey45", size = 0.1) + 
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
save(sf_cent,file = "LME_centroids.RData") 
load("LME_centroids.RData") #dataframe: sf_cent

sf_cent@data 


# Get ff data from google sheets ------------------------------------------

ffs <- read.csv(here('data','fishdata','ffs_063020.csv'))

with_data2 <- sf_cent %>%
  right_join(ffs_summary,by=c('LME_NUMBER'="LME"))

built <- ffs %>%
  group_by(LME,MSE...OMP.built.) %>%
  summarize(Nstocks = length(Stock)) %>%
  left_join(sf_cent,by=c("LME"="LME_NUMBER"))
used <- ffs %>%
  group_by(LME,MSE.operational.in.management.) %>%
  summarize(Nstocks = length(Stock))  %>%
  left_join(sf_cent,by=c("LME"="LME_NUMBER")) 

b <- sf_cent %>%
  right_join(built,by=c('LME_NUMBER'="LME"))
u <- sf_cent %>%
  right_join(used,by=c('LME_NUMBER'="LME"))



map_plot +
  geom_sf(data = b,aes(size = Nstocks),alpha=0.3,colour = 'grey') +
  geom_sf(data = u,
          aes(colour = MSE.operational.in.management.,
              size=Nstocks),alpha=0.7) +
  scale_colour_manual("MSE or OMP \n operational in management?",
                      values = c('#d73027','#fee090','#91bfdb','#4575b4'))+
  theme_void(base_size=14)


# add coords of lmes 

# Get shapefiles of ICES management areas ---------------------------------
ices <- sf::st_read(here('data','ICES_ecoregions','ICES_ecoregions_20171207_erase_ESRI.shp'))
class(ices)
names(ices)

# Get stock data from google sheets







# Other centroid options --------------------------------------------------
#Using rgeos:
# centroids <- rgeos::gCentroid(lmes,byid=TRUE)
# plot(lmes)
# points(coordinates(lmes),pch=1)
# points(centroids,pch=2)