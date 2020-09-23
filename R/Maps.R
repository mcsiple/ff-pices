# Global map of LMEs and degree of asynchrony or sardine-anchovy dominance
#library(rgdal)
library(leaflet)
library(data.table)
library(mapview)    # For saving leaflet maps as pdf or html 
library(sf) # contains function for getting  polygon centroids

mapwd <- here::here('data') # shapefile for Large Marine Ecosystems


lmes <- st_read(here::here('data','LMEs66.shp')) %>%
  st_transform(32617)
sf_cent <- st_centroid(lmes)

plot(lmes)
# Look at the data
head(lmes@data)
class(lmes) # "SpatialPolygonsDataFrame"
names(lmes)

# Color polygons by asynchrony variable in lmes@data or otherwise
# I got the following code from: https://github.com/rstudio/leaflet/issues/169
lmes.dt <- data.table(lmes@data)


# Use leaflet to make map -------------------------------------------------

# Make sure polygons IDs and data.frame row.names match
# bins <- c(0, 0.5,1)
# pal <- colorBin("YlOrRd", domain = lmes$asynchrony, bins = bins)
# (lmap <- leaflet(lmes) %>%
#     addPolygons(
#       fillColor = ~pal(asynchrony),
#       color = "darkgrey", 
#       weight = 1, smoothFactor = 0.5, 
#       opacity = 1.0, fillOpacity = 0.5))


# Use maps to make map ----------------------------------------------------
library(maps)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Assign LMEs to study regions --------------------------------------------
mylmes <- data.frame(lme=c(3,13,29,21,22,24,59,60,20,49,50,51))
mylmes$region <- c("California","Humboldt","Benguela",
                   rep("NE Atlantic",times=6),
                   rep("Kuroshio-Oyashio",times=3))

lmes3 <- sf::st_as_sf(lmes) %>% #select(region.name)
  filter(lme %in% unique(mylmes$lme)) #hoping that this speeds things up, but who knows

pal<- brewer.pal(n = 10,name = "RdBu")

# Now add countries in the background -------------------------------------
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data=world) + 
  geom_sf(color="darkgrey",fill="darkgrey") +
  geom_sf(data = lmes3,
          fill="lightblue",
          aes(fill=asynchrony),
          size=0.1) + 
  scale_fill_gradientn(colours = pal) + 
  theme_classic() 

# Add asynchrony information to table with lmes ---------------------------
# load(file.path(datwd,"LogDiffsMax.RData")) #df is st, summary of log diffs.
# # ml column is log ratio of anchovy/sardine. It's the median of the log ratios from all data soures
# 
# # Asynchrony table
# head(st)
# replaceability <- left_join(st,mylmes,by="region") 
# ssbreplace.dt <- filter(replaceability,newvar=="Spawning stock biomass") %>% as.data.table()

