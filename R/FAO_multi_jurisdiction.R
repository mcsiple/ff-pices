# Check which big small pelagics fisheries are exploited by multiple countries
library(tidyverse)

# Load FAO data (to 2019)
fao <- read.csv(here::here("data","fishdata","All_small_pelagics.csv"))

fao <- fao %>%
        rename_with(~str_remove(., 'X'))

top10 <- fao %>%
        select(Land.Area,Species,Scientific.name,`2019`) %>%
  group_by(Species) %>%
  summarize(tot_landings = sum(`2019`)) %>%
  arrange(desc(tot_landings)) %>%
  top_n(10) %>%
  pull(Species)

fao %>% 
  select(Land.Area,Species,Scientific.name,`2019`) %>%
  filter(Species %in% top10) %>%
  group_by(Species)  %>%
  summarize(length(unique(Land.Area)))
