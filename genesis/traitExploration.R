# set session ####

library(tidyverse)
library(gridExtra)

# load functions
path_function <- "C:/Users/thoma/OneDrive/Documents/aynsScreeuyn/genesis/functions"
functions     <- list.files(path = path_function,pattern = ".R")
invisible(lapply(paste(path_function,functions,sep = "/"),source))

# load data

setwd("D:/genesis/output/5.4_all/120")
timestep <- 0

species   <- readRDS(paste0("./species/species_t_",timestep,".rds"))
landscape <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))
richness  <- readRDS(paste0("./richness/richness_t_",timestep,".rds"))

# trait to look at (must be 0-1)
trait_id <- "niche"

# plot niche trait values only ####

# species niche values

niche_values_species <- traitValuesSpecies(species,trait_id)

plotme <- data.frame(niche_values_species,
                     spp = names(niche_values_species))

spp_filter <- unique(plotme$spp)[1:300]

plotme <- plotme %>% filter(spp %in% spp_filter)

colnames(plotme) <- c("x","y")

plot_species <- ggplot(plotme, aes(x = x)) +
  geom_histogram( aes(fill = y), bins = 141) +
  ggtitle("colour by species") +
  xlab(trait_id) +
  ylab("cell count") +
  theme(legend.position = "none")

# cell niche values

niche_values_cell <- traitValuesCell(species,trait_id)

plotme <- data.frame(niche_values_cell,
                     cell = names(niche_values_cell))

cell_filter <- unique(plotme$cell)[1:300]

plotme <- plotme %>% filter(cell %in% cell_filter)

colnames(plotme) <- c("x","y")

plot_cell <- ggplot(plotme, aes(x = x)) +
  geom_histogram( aes(fill = y), bins = 141) +
  ggtitle("colour by cell") +
  xlab(trait_id) +
  ylab("cell count") +
  theme(legend.position = "none")

# generate plots

grid.arrange(plot_species,plot_cell)

# plot niche trait values scaled by abundance ####

# species niche values

niche_values_species <- traitValuesAbdSpecies(species,landscape,trait_id)

plotme <- niche_values_species

spp_filter <- unique(plotme$sp_id)[1:300]

plotme <- plotme %>% filter(sp_id %in% spp_filter)

plotme$trait_group <- cut(plotme$trait_vector,
                          seq(-0.2,1.2, by = 0.01),
                          labels = FALSE)

plotme <- plotme %>% group_by(sp_id, trait_group) %>% summarise(abundance = sum(abd_vector))

plot_species <- ggplot(plotme, aes(x = trait_group, y = abundance)) +
  geom_col(aes(fill = as.factor(sp_id))) +
  ggtitle("colour by species") +
  theme(legend.position = "none")

# cell niche values


niche_values_cell <- traitValuesAbdCell(species,trait_id)

plotme <- niche_values_cell

cell_filter <- unique(plotme$cell_id)[1:300]

plotme <- plotme %>% filter(cell_id %in% cell_filter)

plotme$trait_group <- cut(plotme$trait_vector,
                          seq(-0.2,1.2, by = 0.01),
                          labels = FALSE)

plotme <- plotme %>% group_by(cell_id, trait_group) %>% summarise(abundance = sum(abd_vector))

plot_cell <- ggplot(plotme, aes(x = trait_group, y = abundance)) +
  geom_col(aes(fill = as.factor(cell_id))) +
  ggtitle("colour by cell") +
  theme(legend.position = "none")



# generate plots

grid.arrange(plot_species,plot_cell)


# trait variance ####




