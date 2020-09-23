# Bibliometrix for EBFM / MSE paper
library(tidyverse)
library(reshape2)
library(bibliometrix)

figdir <- "C:/Users/siplem/Dropbox/PICES Meeting Workshop/Paper - SPF MSE/"

# All these searches were done on 12/4/19
AF <- read.csv("C:/Users/siplem/Dropbox/PICES Meeting Workshop/Paper - SPF MSE/Code/120419/all_fisheries.csv")
colnames(AF) <- c("PY","Npubs")

D2 <- readFiles("C:/Users/siplem/Dropbox/PICES Meeting Workshop/Paper - SPF MSE/Code/120419/ebfm.bib")
M2 <- convert2df(D2, dbsource = "isi", format = "bibtex")

D3 <- readFiles("C:/Users/siplem/Dropbox/PICES Meeting Workshop/Paper - SPF MSE/Code/120419/ebfm_mse.bib")
M3 <- convert2df(D3, dbsource = "isi", format = "bibtex")

D4 <- readFiles("C:/Users/siplem/Dropbox/PICES Meeting Workshop/Paper - SPF MSE/Code/120419/mse.bib")
M4 <- convert2df(D4, dbsource = "isi", format = "bibtex")

# Cumulative citations

get.cumulative <- function(df){
  tf <- df %>%
    filter(PY<2019) %>%
    group_by(PY) %>%
    summarize(cites = length(DT2)) %>%
    complete(PY=min(PY):max(PY)) %>%
    replace_na(list(cites = 0)) %>%
    mutate(cc = cumsum(cites))
  return(tf)
}

ebfm <-get.cumulative(M2)
mse <- get.cumulative(M4)
ebfm.mse <- get.cumulative(M3)

allfisheries <- AF %>%
  mutate(cc = cumsum(Npubs))


alldat2 <-  full_join(mse,ebfm,by= "PY") %>% 
  full_join(ebfm.mse, by="PY") %>%
  full_join(allfisheries,by="PY")

cu2 <- alldat2 %>%
  mutate(MSE=cc.x/cc.y.y,EBFM=cc.y/cc.y.y,EBFM_MSE=cc.x.x/cc.y.y) %>%
  select(PY,MSE,EBFM,EBFM_MSE) %>%
  melt(id.vars="PY") 

fig1b <- cu2 %>%
  filter(!is.na(PY)) %>%
        ggplot(aes(x=PY,y=value,colour=variable)) +
        geom_line(lwd=1.2) +
        theme_classic(base_size = 14) +
        scale_colour_brewer("Keywords") +
        xlim(c(1995,2018)) +
        xlab("Publication Year") +
        ylab("Number of publications \n(proportional to all fisheries publications)") +
        theme(legend.position = c(0.3,0.8))
fig1b

tiff(file.path(figdir,"Fig1_new.tiff"),width = 5,height=5,units = 'in',res = 200)
fig1b
dev.off()
