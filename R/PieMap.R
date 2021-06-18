# Scatterpies attempt
# Code and basics here: https://guangchuangyu.github.io/2016/12/scatterpie-for-plotting-pies-on-ggplot/
library(tidyverse)
library(sf)
library(here)
library(janitor) # for google sheet cleanup
library(scatterpie)
library(ggrepel) # test

# Palettes etc ------------------------------------------------------------
msecols <- c("#fee090", "#91bfdb", "#4575b4", "#99d8c9", "grey45")
# Not yet built, Built, Operational, In progress, 



# World map ---------------------------------------------------------------
world_map <- map_data("world")
map_plot <- ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "lightgrey", colour = "white", size = 0.1
  ) +
  xlab("Longitude") +
  ylab("Latitute") +
  coord_fixed() +
  theme_void(base_size = 14)

map_plot


# LMEs --------------------------------------------------------------------
# Get centroids of each LME (may not need if waffles are cowplotted in)
load(here::here("data", "LME_centroids.RData")) # dataframe: sf_cent


# MSE information ---------------------------------------------------------
ffs <- read.csv(here("data", "fishdata", "ram_ffs_061721.csv"))
# google doc: https://docs.google.com/spreadsheets/d/1lcqabfbkKzwIjXRr0V_PH-GA9gCcy-aw-eFOlfKgXU4/edit#gid=0

fdat <- ffs %>%
  clean_names() %>%
  select(commonname, scientificname, lme, mse_omp_built, mse_operational_in_management) %>%
  rename(
    operational = mse_operational_in_management,
    created = mse_omp_built
  ) %>%
  mutate(mse_category = case_when(
    operational %in% c("Yes", "Probably") ~ "O",
    created == "Yes" & operational %in% c("No", "Not sure") ~ "BNO",
    created == "No" & operational %in% c("No", "Not sure") ~ "NB",
    created == "in progress" | operational == "in progress" | operational == "In progress" ~ "IP",
    created %in% c("unknown", "Not Sure") & operational %in% c("No", "Not sure") ~ "unknown"
  ))

# How many stocks have MSEs of some kind built for them?
table(fdat$mse_category)

spatial_ffs <- sf_cent %>%
  right_join(fdat, by = c("LME_NUMBER" = "lme"))

coords <- spatial_ffs %>% # just the coordinates of the LME centroids
  st_coordinates() %>%
  as.data.frame()

df <- spatial_ffs %>%
  add_column(coords) %>%
  as_tibble() %>%
  rename(lme = LME_NUMBER) %>%
  select(commonname, scientificname, lme, mse_category, X, Y)

# LME key for names/labels
lmekey <- sf_cent %>%
  select(LME_NUMBER, LME_NAME) %>%
  as_tibble()

coords_lmes <- df %>%
  select(lme, X, Y) %>%
  distinct(.keep_all = TRUE) %>%
  left_join(lmekey, by = c("lme" = "LME_NUMBER"))


df2 <- df %>%
  group_by(lme, mse_category) %>%
  count() %>%
  pivot_wider(
    names_from = mse_category,
    values_from = n, values_fill = 0
  ) %>%
  left_join(coords_lmes) %>%
  mutate(nstocks = sum(c(BNO, NB, O, IP, unknown), na.rm = TRUE)) %>%
  #select(-`NA`) %>%
  mutate(
    Built = BNO / nstocks,
    Not_built = NB / nstocks,
    Operational = O / nstocks,
    Unknown = unknown / nstocks,
    In_progress = IP / nstocks
  ) %>%
  as.data.frame()


# Modifications suggested by coauthors for clarity ------------------------
# Adjust Benguela point to be btwn South Africa and Namibia per Carryn de Moor
df3 <- df2 %>%
  mutate(Y = ifelse(LME_NAME == "Benguela Current", -29, Y)) %>%
  mutate(X = ifelse(LME_NAME == "East Bering Sea", -160, X))
# Move Bering Sea point to the left (not sure why it's currently in Canada)


# Consolidate Sea of Japan and East China Sea into "Tsushima Warm Current per Hiro Kurata
twc <- df3 %>%
  filter(LME_NAME %in% c("Sea of Japan", "East China Sea")) %>%
  summarize(lme = 999, NB = sum(NB), BNO = sum(BNO), unknown = sum(unknown), IP = sum(IP), O = sum(O), X = 125.3654, Y = 29.55634, LME_NAME = "Tsushima Warm Current", geometry = st_sfc(st_point(c(125.3654, 29.55634))), nstocks = sum(nstocks)) %>%
  mutate(
    Built = BNO / nstocks,
    Not_built = NB / nstocks,
    Operational = O / nstocks,
    Unknown = unknown / nstocks,
    In_progress = IP / nstocks
  ) %>%
  as.data.frame()

df3 <- df3 %>%
  bind_rows(twc) %>%
  filter(!LME_NAME %in% c("Sea of Japan", "East China Sea"))


pie <- map_plot +
  geom_scatterpie(
    data = df3,
    aes(x = X, y = Y, group = lme, r = nstocks * .7),
    cols = c("Built", "Not_built", "Operational", "In_progress", "Unknown"),
    color = NA, alpha = .8
  ) +
  scale_fill_manual("Management strategy evaluations \nfor small pelagic fishes",
    values = msecols[c(2, 1, 3, 4, 5)]
  ) +
  geom_text_repel(
    data = df3, aes(x = X, y = Y, label = LME_NAME),
    size = 2,
    color = "grey18",
    segment.color = "grey50", 
    point.padding = 0.35
  ) +
  theme(
    text = element_text(family = "sans"),
    legend.text = element_text(family = "sans"),
    legend.title = element_text(size = 10),
    legend.position = "bottom"
  )

pie


# Save figure -------------------------------------------------------------
png("FFMSE_pie_R1.png", width = 8, height = 5, units = "in", res = 200)
pie
dev.off()


# High-res version --------------------------------------------------------
pdf("FFMSE_pie_R1.pdf", width = 8, height = 5, useDingbats = FALSE)
pie
dev.off()
