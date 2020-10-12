# set session ####

library(tidyverse)

setwd("Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/5_all/23")
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

# compare cluster diversity to species diversity

cluster_normalised <- cluster_diversity/max(cluster_diversity)
richness_normalised <- richness/max(richness)

continuity <- sqrt((cluster_normalised-richness_normalised)^2)


# summarise ####

summary <- data.frame(coords,richness,cluster_diversity, continuity)
  
ggplot(summary, aes(x = richness, y = cluster_diversity)) +
  scale_colour_viridis_c(direction = -1) +
  #scale_colour_gradient2(low  = "yellow",high = "green",mid = "blue") +
  geom_point(aes(colour = continuity))

ggplot(summary, aes(x=x,y=y)) +
  geom_tile(aes(colour=richness)) +
  scale_fill_viridis_c() +
  coord_fixed() +
  theme_void()

ggplot(summary, aes(x=x,y=y)) +
  geom_tile(aes(fill=cluster_diversity)) +
  scale_fill_viridis_c() +
  coord_fixed() +
  theme_void()

ggplot(summary, aes(x=x,y=y)) +
  geom_tile(aes(fill=continuity)) +
  scale_fill_viridis_c(direction=-1) +
  coord_fixed() +
  theme_void()

  











