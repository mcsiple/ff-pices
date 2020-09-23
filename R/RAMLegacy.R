# RAM legacy comparison
library(tidyverse)

load(here::here("..","RAMLDB v4.491","DB Files With Assessment Data","R Data","DBdata[asmt][v4.491].RData"))

head(taxonomy) #use to subset to forage fish
spf <- taxonomy %>% filter(FisheryType == "Forage Fish") #FisheryType == "Forage Fish"  gives 42 spps, taxgroup == "forage fish" gives 29 spps
spf <- metadata %>% filter(FisheryType == "Forage Fish")
nrow(spf)

# Korea and China have no stocks listed here - we have to add them after exporting!

write.csv(x = spf, file = "RAM_forage.csv")

