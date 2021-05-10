# Check which big small pelagics fisheries are exploited by multiple countries
library(tidyverse)
library(janitor)

# Load FAO data (to 2019)
fao <- read.csv(here::here("data", "fishdata", "All_small_pelagics.csv"))

fao <- fao %>%
  rename_with(~ str_remove(., "X"))

top10 <- fao %>%
  select(Land.Area, Species, Scientific.name, `2019`) %>%
  group_by(Species) %>%
  summarize(tot_landings = sum(`2019`)) %>%
  arrange(desc(tot_landings)) %>%
  top_n(10) %>%
  pull(Species)

fao %>%
  select(Land.Area, Species, Scientific.name, `2019`) %>%
  filter(Species %in% top10) %>%
  filter(`2019` > 1) %>%
  group_by(Species) %>%
  summarize(length(unique(Land.Area)))

fao %>%
  select(Land.Area, Species, Scientific.name, `2019`) %>%
  filter(Species %in% top10) %>%
  group_by(Species) %>%
  filter(Species == "Anchoveta(=Peruvian anchovy)")


# Need to get marine area-species combos ----------------------------------
# pull all herrings, sardines, anchovies, plus: saurys, mackerel, smelts, flyingfish, scad
fao3 <- read.csv(here::here("data","fishdata", "just_spf_spps.csv"))
spf_spps <- fao3 %>%
  distinct(Species) %>%
  pull(Species)

statj <- read.csv(here::here("data", "fishdata", "fishstatj_export.csv"))

spfdat <- statj %>%
  select(-starts_with("S")) %>%
  clean_names() %>%
  rename_with(~ str_remove(., "x_")) %>%
  pivot_longer(cols = `1950`:`2019`, names_to = "year") %>%
  filter(asfis_species_name %in% spf_spps)


mjs <- spfdat %>%
  filter(year==2019 & value>0) %>%
  group_by(asfis_species_name, fao_major_fishing_area_name) %>%
  summarize(ncountries = length(unique(country_name))) %>%
  ungroup()

write.csv(mjs, "country_counts.csv",row.names = FALSE)

tb <- table(mjs$ncountries)
print("What proportion of stocks are exploited by a single country?")
tb[1] / sum(tb)

print("What proportion of stocks are exploited by more than one country?")
(sum(tb) - tb[1]) / sum(tb)
sum(tb) - tb[1]
