# set session ####

library(tidyverse)
library(viridis)
library(gen3sis)

setwd("Y:/TKeggin/genesis/v0.9.9/output/1d_2000m_20c/2.3.6")

# load data ####
landscape <- readRDS("./landscapes/landscape_t_0.rds")
species   <- readRDS("./species/species_t_0.rds")

# plot goegraphic clusters

coords <- data.frame(landscape$coordinates)
clusts <- species[[2]]$divergence$clusters
clusts <- clusts[match(rownames(coords),names(clusts))]

coords$clusters <- as.factor(clusts)

# plot clusters ####

ggplot(coords, aes(x = x, y = y)) +
  geom_tile(aes(fill = clusters)) +
  coord_fixed() +
  theme_void()

View(get_divergence_matrix(species[[2]]))

species[[1]]$divergence$clusters












# plot temp
temp   <- data.frame(cbind(landscape$environment, landscape$coordinates))
ggplot(temp, aes(x = x, y = y, fill = temp)) +
  geom_tile() +
  scale_fill_viridis(option = "magma",
                     na.value = "lightgrey",
                     limits = c(-31,30)) +
  xlim(-180,180) +
  ylim(-90,90) +
  coord_fixed() +
  theme_void()

