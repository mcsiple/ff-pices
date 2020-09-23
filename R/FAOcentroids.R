# FAO areas
library(tidyverse)
library(sf) #functions: st_read and st_centroid
library(here)

fao_areas <- sf::st_read(here::here('data','FAO_AREAS','FAO_AREAS.shp'))

# Look at FAO data
class(fao_areas)
names(fao_areas)

fao_simple <- fao_areas %>% 
  st_simplify() # simplify so it loads faster

sf_cent <- st_centroid(fao_areas) # get centroids of LMEs 
save(sf_cent,file = "FAO_centroids.RData") 



load("FAO_centroids.RData") #dataframe: sf_cent

