# plot geo_sp_ti from gasm output
# Thomas Keggin
# 04/10/19

# set session ####

library(tidyverse)
library(raster)

setwd("C:/Users/keggint/Documents/bflueck_rgasm_package/output/sample_world_4d/default_config")

# load data ####
load("./geo/geo_sp_t_0.RData")

# wrangle data ####
# create richness column by summing presences per cell
richness <- rowSums(geo_sp_ti[,-(1:2)])
plotData <- data.frame(geo_sp_ti, richness)

# plot species richness ####
ggplot(plotData, aes(x = x, y = y, fill = richness)) +
  geom_tile() +
  coord_fixed() +
  xlim(c(-180,180)) +
  ylim(c(-90,90)) +
  theme_void()
