# set session ####

setwd("C:/Users/thoma/OneDrive/Documents/PhD/side_quests/COI_gen_sp_diversity_manel_2020/worldmap_fish_genetic_diversity")

library(tidyverse)
library(phylotools)
library(pegas)
library(raster)

# load data ####
data_raw   <- read_tsv("./02-raw_data/seqbold_data.tsv")
spp_marine <- read_delim("./01-infos/marine_actinopterygii_species_tk.txt", delim = ",", col_names = FALSE)

# wrangle data ####

# remove unneeded columns
col_keep <- c(2,21,34,35,44,45)
data_filtered <- data_raw[,col_keep]

# remove missing data
data_filtered <- filter(data_filtered,species_name != "_" & lat != "_" & lon != "_" )

# remove ambiguous species calls
data_filtered <- filter(data_filtered, !grepl("\\.",species_name))

# remove ambiguous base calls
for(base in c("B","D","H","V","R","Y","K","M","S","W","N")){
  data_filtered <- filter(data_filtered, !grepl(base,nucleotides))
}

# remove non-numeric coordinates
data_filtered$lat <- as.numeric(data_filtered$lat)
data_filtered$lon <- as.numeric(data_filtered$lon)
data_filtered     <- filter(data_filtered, !is.na(lat) & !is.na(lon))

# remove freshwater species
data_filtered <- filter(data_filtered, species_name %in% spp_marine$X1)

# compute nucleotide diversity per species ####

# split data by species into a list of dataframes
spp_names   <- unique(data_filtered$species_name)
species_dfs <- list()
for(sp in spp_names){
  species_dfs[[sp]] <- filter(data_filtered, species_name == sp)
}

# loop per species
diversity_species <- matrix(ncol=1,nrow=length(spp_names))
for(sp in 1:length(spp_names)){
  
  seq_species        <- strsplit(species_dfs[[sp]]$nucleotides, split="") # split into single character vectors
  # trim to same length
  trim_length     <- min(as.numeric(lapply(seq_species,length)))
  for(i in 1:length(seq_species)){
    seq_species[[i]] <- seq_species[[i]][1:trim_length]
  }
  names(seq_species) <- species_dfs[[sp]]$sampleid # assign sample names
  seq_species        <- as.DNAbin(seq_species) # convert to DNAbin
  diversity_species[sp,1] <- nuc.div(seq_species) # calculate nucletide diversity
  
}
rownames(diversity_species) <- spp_names
colnames(diversity_species) <- "nucleotide_diversity"

diversity_species[is.nan(diversity_species)] <- 0 # set diversity to 0 for singleton species

# assign samples to cells ####
empty_world        <- raster()
data_filtered$cell <- cellFromXY(empty_world, as.matrix(data_filtered[,4:3])) # have to reverse the columns (read as lon/lat)

# sum whole-range within-species diversities per cell ####
cells <- unique(data_filtered$cell)
diversity_cell <- c()

for(c in 1:length(cells)){
  cell_id <- cells[c] # find cell ID
  species <- filter(data_filtered, cell == cells[c])$species_name # find species present in cell
  species <- unique(species) # remove duplicate species
  diversity_cell  <- c(diversity_cell,
                       sum(diversity_species[species,])) # replace species names with diversity values and sum them
}
names(diversity_cell) <- cells

# create data frame of long/lat and cell diversity ####
diversity <- data.frame(cells, diversity_cell, row.names = NULL)
colnames(diversity) <- c("cell", "diversity")

cell_coords    <- distinct(data.frame(xyFromCell(empty_world,data_filtered$cell),data_filtered$cell))
colnames(cell_coords) <- c("lon","lat","cell")

diversity_summary <- merge(diversity, cell_coords)

# species richness ####

# plot this puppy ####

plot_data <- filter(diversity_summary, diversity < 1)

# diversity histogram
ggplot(plot_data, aes(x=diversity)) +
  geom_histogram()


# geographic 
ggplot(plot_data, aes(x = lon, y = lat)) +
  geom_tile(aes(fill = diversity)) +
  scale_fill_viridis_c() +
  theme_void() +
  coord_fixed()

# output data in fasta format ####
data_convert <- data_filtered[,c(1,6)]
colnames(data_convert) <- c("seq.name","seq.text")
dat2fasta(data_convert,"../data_convert.fasta")

# output ####
write_csv(diversity_cell, "../diversity_cell.csv")
