# set session ####

library(ape)
library(readxl)

# load data ####

camille.tree   <- readRDS("C:/Users/keggint/Desktop/Eilish.Fishes.Trees.rds")
molecular.tree <- camille.tree$Molecular.Tree.1063sp
#gene.tree     <- read.tree(file = "M:/data/processed_phylogenetic_trees/All Phylo Sequences.newick")
species        <- read_excel("C:/Users/keggint/Desktop/species.xlsx")

# wrangle data ####

species  <- c(species$caribbean,species$wio)
species <- gsub(" ","_",species)

camille.names <- molecular.tree$tip.label

test <- keep.tip(molecular.tree,species)

# plot data ####

plot(data)
