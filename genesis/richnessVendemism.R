#
# compare richness with weighted endemism
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
source("./endemismWeighted.R")
source("./speciesRange.R")

# read and quantify timesteps ####
setwd("Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/5_all/8")
timesteps.file <- list.files("./richness/")
timesteps.seq  <- seq(min(parse_number(timesteps.file)),max(parse_number(timesteps.file)))

# comapare cluster richness to species richness for each timestep ####

for(timestep in timesteps.seq){
  
  richness  <- readRDS(paste0("./richness/richness_t_",timestep,".rds"))
  species   <- readRDS(paste0("./species/species_t_",timestep,".rds"))
  landscape <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))
  
  pa_dataframe      <- createPADF(species,landscape) # create a presence/absence dataframe
  endemism_weighted <- endemismWeighted(species,landscape)
  
  # plot ####
  summary <- data.frame(landscape$coordinates,richness,endemism_weighted)
  summary <- filter(summary, endemism_weighted < 0.1)
  
  # richness vs endemism_weighted
  rVe <- ggplot(summary, aes(x = richness, y = endemism_weighted)) +
    scale_colour_viridis_c() +
    geom_point(aes(colour = endemism_weighted)) +
    theme_classic()
  
  # map endemism_weighted
  end <- ggplot(summary, aes(x=x,y=y)) +
    geom_tile(aes(fill=endemism_weighted)) +
    scale_fill_viridis_c() +
    coord_fixed() +
    theme_void()
  
  #jpeg(file.path(paste0("./plots/",sprintf("%04i",t),".jpg")), width = 1000, height = 1000)
  #print(rich)
  #dev.off()
  
}