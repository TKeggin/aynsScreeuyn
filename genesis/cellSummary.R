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

setwd("D:/genesis/output/5.4_all/120")
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
# abundance per cell
abundance_cell <- abundanceCell(species)
abundance_cell <- data.frame(rownames(abundance_cell),abundance_cell)
colnames(abundance_cell) <- c("cell", "abundance")

coords <- data.frame(rownames(landscape$coordinates), landscape$coordinates)
colnames(coords) <- c("cell","x","y")

abundance_cell <- left_join(coords,abundance_cell)

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

# cell range (summed range of all species in a cell)
cell_range <- cellRange(pa_dataframe,landscape)

# niche metrics
niche_metrics <- traitMetricsCell(species,landscape,"niche")

# niche vector
niche_values <- traitValuesCell(species,landscape,"niche")

# within species niche variance per cell
niche_variance_cell <- traitVarianceCell(pa_dataframe,species,landscape,"niche")

# t_opt diversity
t_opt_metrics <- traitMetricsCell(species,landscape,"t_opt")

# diversity vs richness
diversityVrich <- continuity(cluster_diversity,richness)

# divergence vs richness
divergenceVrich <- continuity(cluster_divergence,richness)

# niche diversity vs richness
nicheVrich <- continuity(niche_metrics$niche.sd,richness)

# niche diversity between and within species
niche_cont <- continuity(sqrt(niche_variance_cell),niche_metrics$niche.sd)

# richness vs phylo_faith
faithVrich <- continuity(richness,phylo_faith)

# summarise ####

cell_summary <- data.frame(landscape$coordinates,
                           abundance = abundance_cell[,4],
                           cluster_diversity,
                           cluster_divergence,
                           richness,
                           cell_range,
                           endemism_weighted,
                           phylo_faith,
                           phylo_mean,
                           niche_metrics[,-c(1:3)],
                           niche_variance_cell,
                           t_opt_metrics[,-c(1:3)],
                           nicheVrich,
                           diversityVrich,
                           faithVrich,
                           niche_cont,
                           divergenceVrich) %>% 
                    filter(!is.na(cluster_diversity)) # remove empty cells

# export ####

saveRDS(cell_summary, "./cell_summary.rds")

# plot ####
# variables
colnames(cell_summary)

# filter
data <- cell_summary
data <- cell_summary %>% filter(niche.range != 0)


# versus plot
ggplot(data, aes(x=x, y=richness)) +
  geom_point() +
  ggtitle("population divergence vs. species richness") +
  #geom_point(shape = 21, aes(fill = x)) +
  #geom_hline(yintercept=0) +
  scale_fill_viridis_c()

# geographic plot
ggplot(cell_summary, aes(x=x, y=y)) +
  geom_tile(aes(fill = continuity2(cluster_diversity,cell_range))) +
  scale_fill_viridis_c() +
  coord_fixed()

# 3d plot
plot_ly(data,
        x = ~richness,
        y = ~cluster_diversity,
        z = ~cluster_divergence,
        opacity = 0.2,
        color = ~phylo_faith)

# abundance
# richness
# functional trait by longitude
# niche trait distribution
# geographic divergence
# geographic function-richness continuity
# geographic diversity-richness continuity
# geographic functional richness
# https://onlinelibrary.wiley.com/doi/full/10.1111/j.0030-1299.2005.13886.x#b17
# geographic species richness
# function vs richness
# diversity vs richness
# geographic divergence
# divergence vs richness
# phylo_faith vs richness
# diversity/divergence/richness
# geographic weighted endemism













