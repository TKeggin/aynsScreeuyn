# set session ####

library(tidyverse)
library(raster)
library(gen3sis)

setwd("Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/5_all/48")

# load data ####
summary <- readRDS("./sgen3sis.rds")
species <- readRDS("./species/species_t_0.rds")
land    <- readRDS("./landscapes/landscape_t_0.rds")

# count species
species.total <- length(species)

# count clusters per species
clusters.sp <- c()
for(sp in 1:species.total){
  x        <- species[[sp]]$divergence$index
  clusters.sp <- c(clusters.sp,length(unique(x)))
}

# identify surviving species
clusters.alive <- which(clusters.sp>0)

# get divergence matrix
clusters.div <- get_divergence_matrix(species[[15]])

# average divergence per species
species.mean <- mean(clusters.div)

# sum of the average divergence of each cluster ##
# average divergence of each cluster
clusters.mean <- colSums(clusters.div)/length(colSums(clusters.div))
# sum
clusters.sum  <- sum(clusters.mean)

