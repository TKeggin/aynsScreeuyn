#
# calculate the sum diversity of the species inhabiting each cell
# compare the cluster diversity to the species richness
# Thomas Keggin (thomas.keggin@usys.ethz.ch)
#
# set session ####

library(tidyverse)
library(gridExtra)
library(ape)
library(PhyloMeasures)

# load functions ####
setwd("C:/Users/thoma/OneDrive/Documents/aynsScreeuyn/genesis/functions")
functions <- list.files(pattern = ".R")
lapply(functions, source)

# read and quantify timesteps ####
timesteps.file <- list.files("./richness/")
timesteps.seq  <- seq(min(parse_number(timesteps.file)),max(parse_number(timesteps.file)))

# comapare cluster richness to species richness for each timestep ####
setwd("Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/5_all/8")

for(timestep in timesteps.seq){
  
  richness  <- readRDS(paste0("./richness/richness_t_",timestep,".rds"))
  species   <- readRDS(paste0("./species/species_t_",timestep,".rds"))
  landscape <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))
  
  # calculate faith's phylogenetic diversity
  phylo        <- read.nexus("./phy.nex") # load in phylogeny
  pa_dataframe <- createPADF(species,landscape) # create a presence/absence dataframe
  phy_faith    <- phyloFaith(pa_dataframe,phylo) # calculate faith's phylogenetic diversity
  
  # caculate cluster richness
  clusters_sp       <- clustersTotal(species) # calculate total clusters per species
  cluster_diversity <- clusterDiversity(pa_dataframe,clusters_sp) # calculate cluster diversity
  
  # normalise metrics for comparison
  richness_normalised          <- richness/max(richness)
  phy_faith_normalised         <- phy_faith/max(phy_faith)
  cluster_diversity_normalised <- cluster_diversity/max(cluster_diversity)
  
}

# plot ####

plot_data <- data.frame(landscape$coordinates,
                        phy_faith,
                        phy_faith_normalised,
                        richness,
                        richness_normalised,
                        cluster_diversity,
                        cluster_diversity_normalised)

# phy_faith vs richness
ggplot(plot_data, aes(x = richness_normalised, y = phy_faith_normalised)) +
  geom_point()

# phy_faith vs cluster_diversity
ggplot(plot_data, aes(x = phy_faith_normalised, y = cluster_diversity_normalised)) +
  geom_point()

# phy_faith
ggplot(plot_data, aes(x=x, y=y)) +
  geom_tile(aes(fill = phy_faith)) +
  scale_fill_viridis_c() +
  coord_fixed()