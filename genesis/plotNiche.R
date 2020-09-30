# set session ####

setwd("Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/5_all")

setwd("./1")

library(tidyverse)
library(gen3sis)

# load data ####

species <- readRDS("./species/species_t_0.rds")
landscape <- readRDS("./landscapes/landscape_t_0.rds")
coords <- landscape$coordinates

# per cell niche values ####
niche <- c()
for(sp in 1:length(species)){
  x     <- species[[sp]]$traits[,'niche']
  niche <- c(niche,x)
}

niche.df           <- data.frame(names(niche),niche)
colnames(niche.df) <- c("cell","niche")

niche.min <- niche.df %>% group_by(cell) %>% summarise(min = min(niche))
niche.max <- niche.df %>% group_by(cell) %>% summarise(max = max(niche))
niche.sd  <- niche.df %>% group_by(cell) %>% summarise(sd = sd(niche))
niche.count  <- niche.df %>% count(cell)

niche.df       <- niche.min
niche.df$max   <- niche.max$max
niche.df$range <- niche.df$max - niche.df$min
niche.df$sd    <- niche.sd$sd

coords.niche <- data.frame(rownames(coords), coords)
colnames(coords.niche) <- c("cell","x","y")

coords.niche <- left_join(coords.niche,niche.df)

# plot ####

ggplot(coords.niche, aes(x = x, y = y)) +
  geom_tile(aes(fill = range)) +
  scale_fill_viridis_c() +
  coord_fixed()
