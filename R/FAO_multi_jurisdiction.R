# Check which big small pelagics fisheries are exploited by multiple countries
library(tidyverse)

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
fao2 <- read.csv(here::here("data", "fishdata", "All_small_pelagics_fishingaeas_b.csv"))
spf_spps <- fao2 %>%
  distinct(Species) %>%
  pull(Species)

statj <- read.csv(here::here("data", "fishdata", "fishstatj_export.csv"))
spfdat <- statj %>%
  select(-starts_with("S")) %>%
  clean_names() %>%
  rename_with(~ str_remove(., "x_")) %>%
  pivot_longer(cols = `1950`:`2019`, names_to = "year") %>%
  filter(asfis_species_name %in% spf_spps)

top50 <- spfdat %>%
  filter(year == "2019") %>%
  group_by(asfis_species_name, fao_major_fishing_area_name) %>%
  summarize(totalcatch = sum(value)) %>%
  ungroup() %>%
  arrange(desc(totalcatch)) %>%
  top_n(50)

dat <- spfdat %>%
  left_join(top50, by = c("fao_major_fishing_area_name", "asfis_species_name")) %>%
  filter(!is.na(totalcatch))


x <- dat %>%
  filter(value > 100) %>%
  # make it just landings>100 tonnes
  group_by(asfis_species_name, fao_major_fishing_area_name, year) %>%
  summarize(ncountries = length(unique(country_name))) %>%
  filter(year == 2019)
