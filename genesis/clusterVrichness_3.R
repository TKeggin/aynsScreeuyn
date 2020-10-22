#
# calculate the sum diversity of the species inhabiting each cell
# compare the cluster diversity to the species richness
# Thomas Keggin (thomas.keggin@usys.ethz.ch)
#
# set session ####

library(tidyverse)
library(gridExtra)

# load functions ####
setwd("C:/Users/thoma/OneDrive/Documents/aynsScreeuyn/genesis/functions")
source("./createPADF.R")
source("./clustersTotal.R")
source("./clusterDiversity.R")
source("./speciesPresent.R")

# read and quantify timesteps ####
timesteps.file <- list.files("./richness/")
timesteps.seq  <- seq(min(parse_number(timesteps.file)),max(parse_number(timesteps.file)))

# comapare cluster richness to species richness for each timestep ####
setwd("Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/5_all/8")

for(timestep in timesteps.seq){
  
  richness  <- readRDS(paste0("./richness/richness_t_",timestep,".rds"))
  species   <- readRDS(paste0("./species/species_t_",timestep,".rds"))
  landscape <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))
  
  clusters_total    <- clustersTotal(species) # calculate total clusters per species
  pa_dataframe      <- createPADF(species,landscape) # create a presence/absence dataframe
  cluster_diversity <- clusterDiversity(pa_dataframe,clusters_total) # calculate cluster diversity
  
  # compare cluster diversity to species diversity
  cluster_normalised  <- cluster_diversity/max(cluster_diversity)
  richness_normalised <- richness/max(richness)
  continuity          <- sqrt((cluster_normalised-richness_normalised)^2)
  
  # plot ####
  summary <- data.frame(landscape$coordinates,richness,cluster_diversity, continuity)
  
  # richness vs cluster_diversity
  rVc <- ggplot(summary, aes(x = richness, y = cluster_diversity)) +
    scale_colour_viridis_c(direction = -1) +
    geom_point(aes(colour = continuity)) +
    theme_classic()
  
  # map continuity
  cont <- ggplot(summary, aes(x=x,y=y)) +
    geom_tile(aes(fill=continuity)) +
    scale_fill_viridis_c(direction=-1) +
    coord_fixed() +
    theme_void()

  #jpeg(file.path(paste0("./plots/",sprintf("%04i",t),".jpg")), width = 1000, height = 1000)
  #print(rich)
  #dev.off()
  
}