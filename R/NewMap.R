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

sf_cent


# Get ff data from google sheets ------------------------------------------

ffs <- read.csv(here('data','fishdata','ffs_063020.csv'))

spatial_ffs <- sf_cent %>%
  right_join(ffs, by = c('LME_NUMBER'="LME"))

coords <- spatial_ffs %>%
  st_coordinates() %>%
  as.data.frame()

spatialdf <- ffs %>%
  add_column(coords) %>%
  group_by(LME,MSE...OMP.built.,MSE.operational.in.management.) %>%
  mutate(nstocks = length(Stock)) %>%
  distinct(LME,X,Y,MSE...OMP.built.,MSE.operational.in.management.,nstocks) %>%
  group_by(MSE...OMP.built.) 

pdat <- spatialdf %>%
  mutate(new.X = ifelse(MSE...OMP.built.=="Yes",X,ifelse(MSE...OMP.built.=="No",X-10,X-5)))

m1 <- map_plot +
  geom_point(data = pdat, aes(x=new.X,y=Y,colour=MSE...OMP.built.,size=nstocks),alpha=0.6) +
  scale_colour_manual("MSE or OMP \n built?",
                      values = c('#d73027','#fee090','#91bfdb','#4575b4')) +
  coord_fixed() +
  geom_point(data = filter(pdat,MSE.operational.in.management. %in% c("Yes","Probably")), 
             aes(x=new.X,y=Y,size=nstocks),colour= '#4575b4') +
  theme_void(base_size=14)

png("FFMSE_map.png",width = 8,height=6,units = 'in',res = 200)
m1
dev.off()

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