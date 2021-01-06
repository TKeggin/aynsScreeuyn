#
# calculate the sum diversity of the species inhabiting each cell
# compare the cluster diversity to the species richness
# Thomas Keggin (thomas.keggin@usys.ethz.ch)
#
# set session ####

library(tidyverse)
library(gridExtra)

# load functions
path_function <- "C:/Users/thoma/OneDrive/Documents/aynsScreeuyn/genesis/functions"
functions     <- list.files(path = path_function,pattern = ".R")
invisible(lapply(paste(path_function,functions,sep = "/"),source))

# comapare cluster richness to species richness for each timestep ####
setwd("D:/genesis/output/5.5_all/108")

# read and quantify time steps ####
timesteps.file <- list.files("./richness/")
timesteps.seq  <- seq(min(parse_number(timesteps.file)),max(parse_number(timesteps.file)))

for(timestep in timesteps.seq){
  
  richness  <- readRDS(paste0("./richness/richness_t_",timestep,".rds"))
  species   <- readRDS(paste0("./species/species_t_",timestep,".rds"))
  landscape <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))
  
  clusters_total    <- clustersTotal(species) # calculate total clusters per species
  pa_dataframe      <- createPADF(species,landscape) # create a presence/absence dataframe
  cluster_structure <- clusterDiversity(pa_dataframe,clusters_total) # calculate cluster diversity
  Continuity        <- continuity(cluster_structure,richness)
  
  # plot ####
  summary <- data.frame(landscape$coordinates,richness,cluster_structure, Continuity)
  
  # richness vs cluster_diversity
  rVc <- ggplot(summary, aes(x = richness, y = cluster_structure)) +
    scale_colour_viridis_c(direction = -1) +
    geom_point(aes(colour = Continuity)) +
    theme_classic()
  
  # map continuity
  cont <- ggplot(summary, aes(x=x,y=y)) +
    geom_tile(aes(fill=Continuity)) +
    scale_fill_viridis_c(direction=-1) +
    coord_fixed() +
    theme_void()

  jpeg(file.path(paste0("./plots/",sprintf("%04i",timestep),".jpg")), width = 1000, height = 1000)
  print(grid.arrange(rVc,cont))
  dev.off()
  
}
