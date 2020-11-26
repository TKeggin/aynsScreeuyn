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

# niche metrics
niche_metrics <- traitMetricsCell(species,landscape,"niche")

# niche vector
niche_values <- traitValuesCell(species,landscape,"niche")

# t_opt diversity
t_opt_metrics <- traitMetricsCell(species,landscape,"t_opt")

# diversity vs richness
diversityVrich <- continuity(cluster_diversity,richness)

# divergence vs richness
divergenceVrich <- continuity(cluster_divergence,richness)

# niche diversity vs richness
nicheVrich <- continuity(niche_metrics$niche.sd,richness)

# plot ####

cell_summary <- data.frame(landscape$coordinates,
                           abundance = abundance_cell[,4],
                           cluster_diversity,
                           cluster_divergence,
                           richness,
                           endemism_weighted,
                           phylo_faith,
                           phylo_mean,
                           niche_metrics[,-c(1:3)],
                           t_opt_metrics[,-c(1:3)],
                           nicheVrich,
                           diversityVrich,
                           divergenceVrich) %>% 
                    filter(!is.na(cluster_diversity)) # remove empty cells

# abundance
ggplot(cell_summary, aes(x=x,y=y)) +
  geom_tile(aes(fill = abundance/richness)) +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed()

ggplot(cell_summary) +
  geom_point(aes(x=abundance, y=richness, colour=sqrt(y^2))) +
  scale_colour_viridis_c()

ggplot(cell_summary) +
  geom_point(aes(x=abundance, y=y))

ggplot(cell_summary) +
  geom_point(aes(x=abundance/richness, y=y))
  

# richness
ggplot(cell_summary, aes(x = y, y=richness)) +
  geom_point(aes(colour = abundance/richness)) +
  scale_colour_viridis_c()

# functional trait by longitude
test <- filter(cell_summary, !is.na(niche.sd))

ggplot(cell_summary, aes(x=x, y=niche.range)) +
  scale_colour_viridis_c() +
  geom_point(aes(colour=sqrt(y^2)))

# niche trait distribution

plot_me <- niche_values %>% filter(x > -180 & x < 0)

ggplot(plot_me, aes(x=niche, fill=cell)) +
  geom_density(alpha = 0.01) +
  theme(legend.position = "none")

ggplot(plot_me, aes(x=niche, fill=cell)) +
  geom_histogram(bins = 1000) +
  theme(legend.position = "none")

# geographic divergence
ggplot(cell_summary, aes(x=x,y=y)) +
  geom_tile(aes(fill = cluster_divergence)) +
  scale_fill_viridis_c() +
  coord_fixed()

# geographic function-richness continuity
ggplot(cell_summary, aes(x=x,y=y)) +
  geom_tile(aes(fill = nicheVrich)) +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed()

# geographic diversity-richness continuity
ggplot(cell_summary, aes(x=x,y=y)) +
  geom_tile(aes(fill = diversityVrich)) +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed()

# geographic functional richness
# https://onlinelibrary.wiley.com/doi/full/10.1111/j.0030-1299.2005.13886.x#b17
ggplot(cell_summary, aes(x=x,y=y)) +
  geom_tile(aes(fill = niche.range)) +
  scale_fill_viridis_c(direction = 1) +
  coord_fixed()

# geographic species richness
ggplot(cell_summary, aes(x=x,y=y)) +
  geom_tile(aes(fill = richness)) +
  scale_fill_viridis_c(direction = 1) +
  coord_fixed()

# function vs richness
ggplot(cell_summary, aes(y=niche.range, x=richness)) +
  scale_colour_viridis_c(direction = -1) +
  geom_point(aes(colour = nicheVrich))

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

# phylo_faith vs richness
ggplot(cell_summary, aes(y=phylo_mean, x=richness)) +
  geom_point()

# diversity/divergence/richness
plot_ly(cell_summary,
        x = ~richness,
        y = ~cluster_diversity,
        z = ~cluster_divergence,
        opacity = 0.2,
        color = ~phylo_faith)














