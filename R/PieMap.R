# Scatterpies attempt
# Code and basics here: https://guangchuangyu.github.io/2016/12/scatterpie-for-plotting-pies-on-ggplot/
library(tidyverse)
library(sf)
library(here)
library(janitor) # for google sheet cleanup
library(scatterpie)
library(ggrepel) # test

# Palettes etc ------------------------------------------------------------
msecols <- c('#fee090','#91bfdb','#4575b4','grey45')



# World map ---------------------------------------------------------------
world_map <- map_data("world")
map_plot <- ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group),
               fill = "lightgrey", colour = "white", size = 0.1) + 
  xlab('Longitude') +
  ylab('Latitute') +
  coord_fixed() +
  theme_void(base_size=14)

map_plot 


# LMEs --------------------------------------------------------------------
# Get centroids of each LME (may not need if waffles are cowplotted in)
load("LME_centroids.RData") #dataframe: sf_cent


# MSE information ---------------------------------------------------------
ffs <- read.csv(here('data','fishdata','ffs_081920.csv')) #ffs_063020.csv

fdat <- ffs %>% 
  clean_names() %>%
  select(common_name,species,stock, lme, mse_omp_built,mse_operational_in_management) %>%
  rename(operational = mse_operational_in_management,
         created = mse_omp_built) %>%
  mutate(mse_category = case_when(
    operational %in% c('Yes','Probably') ~ "O",
    created == 'Yes' & operational %in% c('No','Not sure') ~ "BNO",
    created == 'No' & operational %in% c('No','Not sure') ~ "NB",
    created %in% c('unknown', 'Not Sure') & operational %in% c('No','Not sure') ~ "unknown"
  ))


spatial_ffs <- sf_cent %>%
  right_join(fdat,by=c('LME_NUMBER'='lme'))

coords <- spatial_ffs %>% # just the coordinates of the LME centroids
  st_coordinates() %>%
  as.data.frame() 
  
df <- spatial_ffs %>%
  add_column(coords) %>%
  as_tibble() %>%
  rename(lme = LME_NUMBER) %>%
  select(common_name, species, stock, lme, mse_category, X, Y)

# LME key for names/labels
lmekey <- sf_cent %>%
  select(LME_NUMBER,LME_NAME) %>%
  as_tibble() 

coords_lmes <- df %>%
  select(lme, X, Y) %>%
 distinct(.keep_all = TRUE) %>%
  left_join(lmekey, by = c('lme' = 'LME_NUMBER'))


df2 <- df %>%
  group_by(lme,mse_category) %>%
  count() %>%
  pivot_wider(names_from = mse_category,values_from = n,values_fill = 0) %>%
  left_join(coords_lmes) %>%
  mutate(nstocks = sum(c(BNO, NB, O, unknown),na.rm=TRUE)) %>%
  select(-`NA`) %>%
  mutate(Built = BNO/nstocks,
         Not_built = NB/nstocks,
         Operational = O/nstocks,
         Unknown = unknown/nstocks) %>%
  as.data.frame()

pie <- map_plot + 
  geom_scatterpie(data = df2,
                  aes(x=X,y=Y,group=lme,r=nstocks*1.1),
                  cols = c('Built','Not_built','Operational','Unknown'),color=NA, alpha=.8) +
  scale_fill_manual('Management strategy evaluations \nfor small pelagic fishes',values = msecols[c(2,1,3,4)]) +
  geom_text_repel(data = df2, aes(x=X,y=Y,label = LME_NAME),
                  size=2,
                  segment.color = "grey50",point.padding = 0.35) +
  theme(text = element_text(family = "IBM Plex Sans"),
        legend.text = element_text(family =  "IBM Plex Sans"),
        legend.title = element_text(size = 10),
        legend.position = 'bottom')

pie


png("FFMSE_pie.png",width = 8,height = 5,units = 'in',res=200)
pie
dev.off()
