# What assessment method is used for Chilean stocks?
# What assessment methods are generally used for forage fish stocks?

library(tidyverse)

load(here::here("..","RAMLDB v4.491","DB Files With Assessment Data","R Data","DBdata[asmt][v4.491].RData"))
spf_stocks <- metadata %>% 
  filter(FisheryType == "Forage Fish") %>%
  select(assessid, stockid, assessyear,stocklong)
nrow(spf_stocks)

assess_df <- assessment %>% 
  right_join(spf_stocks, by = c("stockid","assessid","assessyear","stocklong") )
nrow(assess_df)

# What are the assesment methods for Peruvian anchoveta? according to RAM
filter(assess_df, stockid %in% c("PANCHCSCH","PANCHNCHSP"))
# Chilean method unknown according to RAM

method_key <- as_tibble(assessmethod) # df

assesscounts <- assess_df %>% 
  group_by(assessmethod) %>%
  count() %>%
  left_join(method_key, by = c("assessmethod" = "methodshort"))

assesscounts %>%
  group_by(category) %>%
  summarize(n_cat = sum(n)) %>%
  ggplot(aes(x=category,y = n_cat)) +
  geom_bar(stat = "identity") +
  ylab("Number of stocks") +
  xlab("Assessment method") +
  coord_flip()
