#
# This script generates summary metrics per cell in a run
# Thomas Keggin
#

# set session ####

library(tidyverse)
library(gridExtra)
library(ape)
library(PhyloMeasures)
library(gen3sis)
library(plotly)

setwd("Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/5_all/8")

# set timestep
timestep <- 0

# load functions
path_function <- "C:/Users/thoma/OneDrive/Documents/aynsScreeuyn/genesis/functions"
functions     <- list.files(path = path_function,pattern = ".R")
invisible(lapply(paste(path_function,functions,sep = "/"),source))

# load data
richness  <- readRDS(paste0("./richness/richness_t_",timestep,".rds"))
species   <- readRDS(paste0("./species/species_t_",timestep,".rds"))
landscape <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))
phylo     <- read.nexus("./phy.nex")

# calculate metrics ####

# cluster diversity
pa_dataframe      <- createPADF(species,landscape)
clusters_total    <- clustersTotal(species)
cluster_diversity <- clusterDiversity(pa_dataframe,clusters_total) # takes a while

# cluster divergence
cluster_divergence <- clusterDivergence(pa_dataframe,species,landscape) # also takes a while

# weighted endemism
endemism_weighted <- endemismWeighted(species,landscape)

# phylogenetic diversity (faith)
phylo_faith <- phyloFaith(pa_dataframe,phylo)

# phylogenetic diversity (mpd)
phylo_mean <- phyloMean(pa_dataframe,phylo)

# niche metrics
niche_metrics <- traitMetricsCell(species,landscape,"niche")

# t_opt diversity
t_opt_metrics <- traitMetricsCell(species,landscape,"t_opt")

# diversity vs richness
diversityVrich <- continuity(cluster_diversity,richness)

# divergence vs richness
divergenceVrich <- continuity(cluster_divergence,richness)

# plot ####

cell_summary <- data.frame(landscape$coordinates,
                           cluster_diversity,
                           cluster_divergence,
                           richness,
                           endemism_weighted,
                           phylo_faith,
                           phylo_mean,
                           niche_metrics[,-c(1,2)],
                           t_opt_metrics[,-c(1,2)],
                           diversityVrich,
                           divergenceVrich)

# geographic diversity
ggplot(cell_summary, aes(x=x,y=y)) +
  geom_tile(aes(fill = diversityVrich)) +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed()

# diversity vs richness
ggplot(cell_summary, aes(y=cluster_diversity, x=richness)) +
  scale_colour_viridis_c(direction = -1) +
  geom_point(aes(colour = diversityVrich))

# geographic divergence
ggplot(cell_summary, aes(x=x,y=y)) +
  geom_tile(aes(fill = divergenceVrich)) +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed()

# divergence vs richness
ggplot(cell_summary, aes(y=cluster_divergence, x=richness)) +
  scale_colour_viridis_c(direction = -1) +
  geom_point(aes(colour = diversityVrich))

# divergence vs richness
ggplot(cell_summary, aes(y=cluster_divergence, x=richness)) +
  scale_colour_viridis_c(direction = -1) +
  geom_point(aes(colour = divergenceVrich))

# phylo_faith vs richness
ggplot(cell_summary, aes(y=phylo_mean, x=richness)) +
  geom_point()

# diversity/divergence/richness
plot_ly(cell_summary,
        x = ~richness,
        y = ~cluster_diversity,
        z = ~cluster_divergence,
        opacity = 0.2,
        color = ~phylo_mean)














