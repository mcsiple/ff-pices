#Waffle map
library(tidyverse)
library(sf) #functions: st_read and st_centroid
library(here)
library(janitor) # for google sheet cleanup


# Basic world map ---------------------------------------------------------
world_map <- map_data("world")
map_plot <- ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group),
               fill = "grey30", colour = "grey45", size = 0.1) + 
  xlab('Longitude') +
  ylab('Latitute') +
  coord_fixed() +
  theme_void(base_size=14)

map_plot 


# LMEs --------------------------------------------------------------------
# Get centroids of each LME (may not need if waffles are cowplotted in)
load("LME_centroids.RData") #dataframe: sf_cent


# MSE information ---------------------------------------------------------
ffs <- read.csv(here('data','fishdata','ffs_063020.csv'))

fdat <- ffs %>% 
  clean_names() %>%
  select(i_common_name,species,stock, lme, mse_omp_built,mse_operational_in_management) %>%
  rename(operational = mse_operational_in_management,
         created = mse_omp_built) %>%
  mutate(mse_category = case_when(
    operational %in% c('Yes','Probably') ~ "MSE built and used for management",
    created == 'Yes' & operational %in% c('No','Not sure') ~ "MSE built, not operational",
    created %in% c('unknown','No') & operational %in% c('No','Not sure') ~ "MSE not yet built"
  ))


spatial_ffs <- sf_cent %>%
  right_join(ffs,by=c('LME_NUMBER'="LME"))


library(magrittr)
library(hrbrthemes)
library(waffle)
library(cowplot) #for insets

# Set up themes -----------------------------------------------------------

waffletheme <- theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

msecols <- c('#fee090','#91bfdb','#4575b4')


# Data for waffle plots
wafdat <- fdat %>%
  group_by(lme,mse_category) %>%
  count()
lme_list <- wafdat %>% 
  group_by(lme) %>% 
  group_split()

# Plot them all together
wafdat %>%
  ggplot(aes(fill = mse_category,values = n)) +
  geom_waffle(n_rows = 1,size=0.33,colour='grey') +
  scale_fill_manual(values = msecols) +
  facet_wrap(~lme) +
  waffletheme +
  coord_equal()


plotfun <- function(x){
  x %>%
    ggplot(aes(fill = mse_category,values = n)) +
    geom_waffle(n_rows = 1,size=0.2,colour='grey') +
    scale_fill_manual(values = msecols) +
    labs(subtitle= paste(x$lme[1])) +
    waffletheme +
    coord_equal() +
    theme(legend.position = 'none')
}

myplots <- lapply(lme_list,FUN = plotfun)

# Get standalone legend to place in bigger plot
xx <- ggplot(lme_list[[5]],
             aes(lme, fill = mse_category)) + 
  geom_bar(colour='white') +
  scale_fill_manual("MSE category",values = msecols) +
  waffletheme 
legend <- get_legend(xx)


# Save it! ----------------------------------------------------------------
wafscale <- 0.1

png('FFMSEMap.png',width = 12,height = 8,units = 'in',res = 200)
ggdraw() +
  draw_plot(map_plot) +
  draw_plot(myplots[[1]], x = -0.02, y = .6, width = .3, height = .3,scale = wafscale)  + # GoA (2)
  draw_plot(myplots[[2]], x = 0, y = .52, width = .3, height = .3,scale = wafscale)  + # CA Current (3)
  draw_plot(myplots[[3]], x = 0.1, y = .25, width = .3, height = .3,scale = wafscale)  +  # Humboldt Current (13)
  draw_plot(myplots[[4]], x = 0.26, y = .35, width = .3, height = .3,scale = wafscale)  + # East Brazil Shelf (16)
  draw_plot(myplots[[5]], x = 0.35, y = .7, width = .3, height = .3,scale = wafscale)  + # North Sea (22) ** MAY NEED TO ADD LINE SEGMENT
  draw_plot(myplots[[6]], x = 0.36, y = .65, width = .3, height = .3,scale = wafscale)  + # Baltic Sea (23)
  draw_plot(myplots[[7]], x = 0.29, y = .55, width = .3, height = .3,scale = wafscale)  + # Celtic-Biscay Shelf (24)
  draw_plot(myplots[[8]],  x = 0.29, y = .57, width = .3, height = .3,scale = wafscale)  + # Iberian Coast (25)
  draw_plot(myplots[[9]], x = .37, y = .51, width = .3, height = .3,scale = wafscale)  + # Mediterranean (26)
  draw_plot(myplots[[10]], x = .33, y = .31, width = .3, height = .3,scale = wafscale)  + # Benguela Current (29)
  draw_plot(myplots[[11]], x = 0.68, y = .32, width = .3, height = .3,scale = wafscale)  + # North Australian shelf (39)
  draw_plot(myplots[[12]], x = 0.64, y = .21, width = .3, height = .3,scale = wafscale)  + # Southwest Australian shelf (43)
  draw_plot(myplots[[13]], x = 0.66, y = .51, width = .3, height = .3,scale = wafscale)  + # East China Sea (47)
  draw_plot(myplots[[14]], x = 0.27, y = .1, width = .3, height = .3,scale = wafscale)  + # Antarctic (61)
  draw_plot(myplots[[15]], x = 0.43, y = .58, width = .3, height = .3,scale = wafscale)  + # Black Sea (62)
  draw_plot(myplots[[16]], x = 0.2, y = .72, width = .3, height = .3,scale = wafscale)  #  (64) Arctic
dev.off()
