# 
# script to calculate the phylogenetic vs. cluster diversity per cell
#
# set session ####

library(tidyverse)
library(ape)
library(PhyloMeasures)

setwd("Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/5_all/8")
timestep <- 0

richness  <- readRDS(paste0("./richness/richness_t_",timestep,".rds"))
species   <- readRDS(paste0("./species/species_t_",timestep,".rds"))
summary   <- readRDS("sgen3sis.rds")
landscape <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))
coords    <- landscape$coordinates


# per species
clusters_sp <- c()
for(sp in 1:length(species)){
  x        <- species[[sp]]$divergence$index
  clusters_sp <- c(clusters_sp,length(unique(x)))
}

clusters_total <- sum(clusters_sp)

# create a presence/absence dataframe ####
pa_dataframe             <- data.frame(matrix(0,nrow=length(landscape$coordinates[,1]), ncol=(length(species)+2))) # create a dataframe
pa_dataframe[,1:2]       <- landscape$coordinates # add the coordinate information
rownames(pa_dataframe)   <- rownames(landscape$coordinates) # set rownames as cell IDs
names(pa_dataframe)[1:2] <- c("x", "y")
names(pa_dataframe)[3:length(pa_dataframe)] <- unlist(lapply(species, FUN=function(x){x$id})) # set column names as species IDs
# fill in the p/a data
for(i in 3:(length(pa_dataframe[1,]))){
  pa_dataframe[names(species[[i-2]]$abundance),i] <- 1
}


# determine which species inhabits a cell ####
no_cells <- dim(pa_dataframe)[1]
cells <- list()

for(i in 1:no_cells){
  present_species <- which(pa_dataframe[i,] == 1)-2
  cells[[i]]  <- present_species
}

# determine the number of clusters each species has that is present in a cell
cluster_diversity <- c()
for(i in 1:no_cells){
  
  x <- sum(clusters_sp[cells[[i]]]) # for all the species in a cell, sum up their numbers of clusters
  cluster_diversity <- c(cluster_diversity,x)
}

# calculate phylogenetic richness per cell ####
# load in phylogenetic tree
phy <- read.nexus("./phy.nex")

# create community matrix for pd calculation (cells x species)
pa_matrix <- as.matrix(pa_dataframe[,-c(1,2)])

# match the pa_matrix column names to the phy species naming format (1..n to species1...speciesn)
colnames(pa_matrix) <- paste("species",colnames(pa_matrix), sep = "")

# calculate pd
pd_faith <- pd.query(phy,pa_matrix)
pd_mpd   <- mpd.query(phy,pa_matrix)


# compare cluster diversity to species diversity ####

cluster_normalised <- cluster_diversity/max(cluster_diversity)
pd_normalised <- pd_mpd/max(pd_mpd)

continuity <- sqrt((cluster_normalised-pd_normalised)^2)


# plot ####
plotData <- data.frame(coords,pd_mpd,cluster_diversity, continuity)

# map pd_faith
ggplot(data = plotData, aes(x=x,y=y)) +
  geom_tile(aes(fill = pd_mpd)) +
  scale_fill_viridis_c() +
  coord_fixed()

# compare pd_faith to cluster_diversity
ggplot(data = plotData, aes(x=pd_mpd, y=cluster_diversity)) +
  scale_colour_viridis_c(direction = -1) +
  geom_point(aes(colour = continuity))

# map continuity
ggplot(data = plotData, aes(x=x,y=y)) +
  geom_tile(aes(fill = continuity)) +
  scale_fill_viridis_c(direction = -1) +
  coord_fixed()


