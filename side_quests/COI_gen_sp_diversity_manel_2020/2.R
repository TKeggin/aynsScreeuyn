#### Setup ####
setwd('C:/Users/thoma/OneDrive/Documents/PhD/side_quests/COI_gen_sp_diversity_manel_2020/worldmap_fish_genetic_diversity/')

library(tidyverse)
library(phylotools)
library(pegas)
library(raster)

data_raw   <- read_tsv("./02-raw_data/seqbold_data.tsv")
spp_marine <- read_delim("./01-infos/marine_actinopterygii_species_tk.txt", delim = ",", col_names = FALSE)


#### Wrangle data ####
# remove unneeded columns
col_keep <- c(2,21,34,35,44,45)
data_wrangled <- data_raw[,col_keep]

# remove missing data
data_wrangled <- filter(data_wrangled,species_name != "_" & lat != "_" & lon != "_" )

# remove ambiguous species calls
data_wrangled <- filter(data_wrangled, !grepl("\\.",species_name))

# remove ambiguous base calls
for(base in c("B","D","H","V","R","Y","K","M","S","W","N")){
  data_wrangled <- filter(data_wrangled, !grepl(base,nucleotides))
}

# remove non-numeric coordinates
data_wrangled$lat <- as.numeric(data_wrangled$lat)
data_wrangled$lon <- as.numeric(data_wrangled$lon)
data_wrangled     <- filter(data_wrangled, !is.na(lat) & !is.na(lon))

# remove freshwater species
data_wrangled <- filter(data_wrangled, species_name %in% spp_marine$X1)


#### Compute nucleotide diversity per species ####
# split data by species into a list of dataframes
spp_names   <- unique(data_wrangled$species_name)
species_dfs <- list()
for(sp in spp_names){
  species_dfs[[sp]] <- filter(data_wrangled, species_name == sp)
}

# loop per species
divergence_species <- matrix(ncol=1,nrow=length(spp_names))
for(sp in 1:length(spp_names)){
  
  seq_species        <- strsplit(species_dfs[[sp]]$nucleotides, split="") # split into single character vectors
  # trim to same length
  trim_length     <- min(as.numeric(lapply(seq_species,length)))
  for(i in 1:length(seq_species)){
    seq_species[[i]] <- seq_species[[i]][1:trim_length]
  }
  names(seq_species) <- species_dfs[[sp]]$sampleid # assign sample names
  seq_species        <- as.DNAbin(seq_species) # convert to DNAbin
  divergence_species[sp,1] <- nuc.div(seq_species) # calculate nucletide diversity
  
}
rownames(divergence_species) <- spp_names
colnames(divergence_species) <- "nucleotide_diversity"

divergence_species[is.nan(divergence_species)] <- 0 # set diversity to 0 for singleton species


#### Count unique nucleotide sequences per species ####
spp_names   <- unique(data_wrangled$species_name)
species_dfs <- list()
for(sp in spp_names){
  species_dfs[[sp]] <- filter(data_wrangled, species_name == sp)
}

# loop per species
divergence_species <- matrix(ncol=1,nrow=length(spp_names))
for(sp in 1:length(spp_names)){
  
  divergence_species[sp,1] <- length(unique(species_dfs[[sp]]$nucleotides)) # calculate nucletide diversity
  
}
rownames(divergence_species) <- spp_names
colnames(divergence_species) <- "cluster_divergence"

divergence_species[is.nan(divergence_species)] <- 0 # set diversity to 0 for singleton species



#### Assign samples to cells ####
empty_world        <- raster()
data_wrangled$cell <- cellFromXY(empty_world, as.matrix(data_wrangled[,4:3])) # have to reverse the columns (read as lon/lat)


#### Sum whole-range within-species diversities per cell ####
cells <- unique(data_wrangled$cell)
diversity_cell <- c()

for(c in 1:length(cells)){
  cell_id <- cells[c] # find cell ID
  species <- filter(data_wrangled, cell == cells[c])$species_name # find species present in cell
  species <- unique(species) # remove duplicate species
  diversity_cell  <- c(diversity_cell,
                       mean(divergence_species[species,])) # replace species names with diversity values and average them
}
names(diversity_cell) <- cells

#### create data frame of long/lat and cell diversity ####
diversity <- data.frame(cells, diversity_cell, row.names = NULL)
colnames(diversity) <- c("cell", "diversity")

cell_coords    <- distinct(data.frame(xyFromCell(empty_world,data_wrangled$cell),data_wrangled$cell))
colnames(cell_coords) <- c("lon","lat","cell")

diversity_summary <- merge(diversity, cell_coords)


### Some nice plots ####
#plot_data <- filter(diversity_summary, diversity < 20 & diversity != 0)
plot_data <- diversity_summary

# diversity histogram
ggplot(plot_data, aes(x=diversity)) +
  geom_histogram(bins = 100)

# geographic 
ggplot(plot_data, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = diversity)) +
  scale_fill_viridis_c() +
  theme_void() +
  coord_fixed()
