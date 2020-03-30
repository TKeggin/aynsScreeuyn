# set session ####

library(SimRAD)
library(seqinr)

setwd("C:/Users/keggint/polybox/Zurich/data/megafauna/genomes/Myripristis jacobus/")

# load data ####

data <- ref.DNAseq("./GCA_900302555.1_ASM90030255v1_genomic.fna")

# set enzymes ####

#Restriction Enzyme 1
#EcoRI#
E5 <- "G"
E3 <- "AATTC"

#Restriction Enzyme 2
#TaqI #
T5 <- "T"
T3 <- "CGA"

# perform digest ####

digest <- insilico.digest(data, E5, E3, T5, T3, verbose = TRUE)

# sandbox ####

test <- as.character("NNNNNGAATTCNNGAATTCNNNNNTCGANNNN")

test.digest <- insilico.digest(test, E5, E3, T5, T3)
