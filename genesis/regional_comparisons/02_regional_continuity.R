# This script takes calculates the continuity between clusters and species
# in a series of simulations between ecoregions as defined by Spalding et
# al 2007

# set session ####
library(tidyverse)

# load data ####
# ecoregions
ecoregions <- read_csv("C:/Users/thoma/OneDrive/Documents/PhD/chapter_1/analysis/regional_continuity/01_sim_ecoregions.csv")


# loop for runs
setwd("D:/genesis/output/5.4_all/120/")
timestep <- 0

richness   <- readRDS(paste0("./richness/richness_t_",timestep,".rds"))
species    <- readRDS(paste0("./species/species_t_",timestep,".rds"))
summary    <- readRDS("sgen3sis.rds")
landscape  <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))

all_sim      <- data.frame(coords   = landscape$coordinates,
                           realm    = ecoregions$layer_REALM,
                           richness)
all_sim$cell <- rownames(all_sim) 


# create a data frame to contain all realm and species information ####
species_extract <- data.frame(cell = c(),
                              species_id = c(),
                              cluster_id = c())

for(i in 1:length(species)){
  if(length(species[[i]]$abundance) > 0){
    x <- data.frame(cell = names(species[[i]]$divergence$index),
                    species_id = species[[i]]$id,
                    cluster_id = paste0(species[[i]]$id,"_",species[[i]]$divergence$index))
    species_extract <- rbind(species_extract,x)
  }
}

all_sim <- inner_join(species_extract,all_sim)

# count number of species and clusters per realm

species_sim <- all_sim %>% select(species_id,realm)
species_sim <- species_sim %>% distinct()
species_count <- species_sim %>% count(realm)
colnames(species_count) <- c("realm","species")

cluster_sim <- all_sim %>% select(cluster_id,realm)
cluster_sim <- cluster_sim %>% distinct()
cluster_count <- cluster_sim %>% count(realm)
colnames(cluster_count) <- c("realm","cluster")

summary <- full_join(species_count,cluster_count)
summary$continuity <- summary$species/summary$cluster

tropical_summary <- summary %>% filter(realm %in% c("Central Indo-Pacific",
                                                    "Tropical Atlantic",
                                                    "Tropical Eastern Pacific",
                                                    "Western Indo-Pacific"))

# assign continuity values back into the spatial data

plot_me <- inner_join(all_sim,tropical_summary)


# plot ####

ggplot(all_sim, aes(x=x,y=y)) +
  geom_tile(aes(fill=ecoregions.layer_REALM)) +
  coord_fixed()


summary$realm <- factor(summary$realm, levels = summary$realm[order(summary$continuity)])
ggplot(summary, aes(x = realm, y = continuity)) +
  geom_col(aes(fill = realm)) +
  coord_flip() +
  theme(legend.position = "none")

cont <- ggplot(plot_me, aes(x = coords.x, y = coords.y)) +
  geom_tile(aes(fill = continuity)) +
  scale_fill_viridis_c() +
  coord_fixed()

rich <- ggplot(plot_me, aes(x = coords.x, y = coords.y)) +
  geom_tile(aes(fill = richness)) +
  scale_fill_viridis_c() +
  coord_fixed()

grid.arrange(cont, rich)
