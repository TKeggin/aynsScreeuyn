# set session ####

library(tidyverse)

setwd("Y:/TKeggin/genesis/v0.9.9/output/6d_2000m_20c/test/")

# choose variables
timestep  <- 0
speciesNo <- 1

# load and wrangle data ####

landscape <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))
species   <- readRDS(paste0("./species/species_t_",timestep,".rds"))[[1]]

coords <- data.frame(landscape$coordinates)
clusts <- species$divergence$clusters
clusts <- clusts[match(rownames(coords),names(clusts))]

coords$clust <- as.factor(clusts)

data <- coords

# plot clusters ####

ggplot(data, aes(x = x, y = y)) +
  geom_tile(aes(fill = clust)) +
  coord_fixed() +
  theme_void()

# looped ####

timesteps <- seq(0,length(list.files("./landscapes/"))-1)
vid.order <- rev(timesteps)+1

for(step in timesteps){
  
  # load and wrangle data
  landscape <- readRDS(paste0("./landscapes/landscape_t_",step,".rds"))
  species   <- readRDS(paste0("./species/species_t_",timestep,".rds"))[[1]]
  
  coords <- data.frame(landscape$coordinates)
  clusts <- species$divergence$clusters
  clusts <- clusts[match(rownames(coords),names(clusts))]
  
  coords$clust <- as.factor(clusts)
  
  data <- coords
  
  # plot clusters
  
  plot <- ggplot(data, aes(x = x, y = y)) +
              geom_tile(aes(fill = clust)) +
              ggtitle(landscape$timestep) +
              coord_fixed() +
              theme_void()
  
  jpeg(file.path("C:/Users/thoma/OneDrive/Documents/Marrey/Output/test_plots/", paste0(sprintf("%04i",vid.order[step]),".jpg")), width = 1360, height = 960)
  print(plot)
  dev.off()
  
  print(paste(step,"/",length(timesteps)-1))
  
  
}
