# set session ####

library(ape)
library(readxl)

# load data ####

gene.tree <- read.tree(file = "M:/data/processed_phylogenetic_trees/All Phylo Sequences.newick")
species   <- read_excel("C:/Users/keggint/Desktop/species.xlsx")

# wrangle data ####

sub("*_","",gene.tree$tip.label)

spp  <- c(spp$caribbean,spp$wio)
test <- keep.tip(data,spp)

# plot data ####

plot(data)
