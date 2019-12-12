# set session ####

setwd("C:/Users/thoma/Desktop/gasm/output/6d/my_sample_config")

library(ape)
library(tidyverse)

# load data ####

phy     <- read.nexus("./phy.nex")
species <- readRDS("./species/species_t_0.rds")
land    <- readRDS("./landscapes/landscape_t_0.rds")

# phylogeny ####
# plot
plot(phy)

# species ####
# explore
labels(species)
species[[1]]$id
species[[1]]$abundance
species[[1]]$traits

# landscapes ####
# explore
land$id
land$environment
land$coordinates

# plot ####

# merge coordinate and abundance data for all species
spp <- seq(1, length(species))

distribution <- rownames_to_column(data.frame(land$coordinates))

for(i in spp){
  sp <- species[[i]]$abundance
  sp[!is.na(sp)] <- 1
  sp <- rownames_to_column(data.frame(sp))
  
  distribution <- left_join(distribution,sp, by = "rowname")
}
colnames(distribution) <- c("cell","x","y",spp)

richness <- rowSums(!is.na(distribution[,-c(1:3)]))

distribution <- cbind(distribution,richness)



ggplot(data = distribution, aes(x=x,y=y, fill = richness)) +
  geom_tile()

