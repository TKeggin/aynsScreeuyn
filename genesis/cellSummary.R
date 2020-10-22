#
# This script generates summary metrics per cell in a run
# Thomas Keggin
#

# set session ####

library(tidyverse)
library(gridExtra)
library(ape)
library(PhyloMeasures)

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

# species richness
#richness

# cluster diversity
pa_dataframe      <- createPADF(species,landscape)
clusters_total    <- clustersTotal(species)
cluster_diversity <- clusterDiversity(pa_dataframe,clusters_total) # takes a while

# weighted endemism
endemism_weighted <- endemismWeighted(species,landscape)

# phylogenetic diversity
phylo_faith <- phyloFaith(pa_dataframe,phylo)

# niche metrics
niche_metrics <- traitMetricsCell(species,landscape,"niche")

# t_opt diversity
t_opt_metrics <- traitMetricsCell(species,landscape,"t_opt")







