# set session ####
library(tidyverse)
library(gridExtra)

# set directory
setwd("D:/genesis/output/5.4_all/120")

# set timestep
timestep <- 0

# load functions
path_function <- "C:/Users/thoma/OneDrive/Documents/aynsScreeuyn/genesis/functions"
functions     <- list.files(path = path_function,pattern = ".R")
invisible(lapply(paste(path_function,functions,sep = "/"),source))

# load data
richness     <- readRDS(paste0("./richness/richness_t_",timestep,".rds"))
species      <- readRDS(paste0("./species/species_t_",timestep,".rds"))
landscape    <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))
phylo        <- read.nexus("./phy.nex")
cell_summary <- readRDS("./cell_summary.rds")

# play ####
# niche diversity
cell_summary$niche.sd.rich <- cell_summary$niche.sd/cell_summary$richness
cont <- continuity(cell_summary$niche.sd, cell_summary$niche.sd.rich)

geog <- ggplot(cell_summary, aes(x=x, y=y)) +
  geom_tile(aes(fill = cont)) +
  scale_fill_viridis_c() +
  coord_fixed()

ggplot(cell_summary, aes(y = richness, x = niche.sd.rich, fill = y^2)) +
  geom_point(shape = 21) +
  scale_fill_viridis_c()

ggplot(cell_summary, aes(x = cont)) +
  geom_density()

# versus plot
ggplot(data, aes(x=x, y=niche.sd.rich)) +
  geom_point(shape = 21, aes(fill = richness)) +
  scale_fill_viridis_c()

# geographic plot
ggplot(data, aes(x=x, y=y)) +
  geom_tile(aes(fill = continuity(niche.sd,niche.sd/richness))) +
  scale_fill_viridis_c() +
  coord_fixed()

