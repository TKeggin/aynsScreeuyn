# set session ####

setwd("Z:/LE_projects/megafauna/data")

library("tidyverse")
library("readxl")
library("taxise")

#load data ####

samples <- read_excel("./sandbox/Browse samples.xlsx", sheet = 1)
samples$gen_sp <- paste(samples$Genus, samples$species, sep = " ")
sampleSpp <- unique(samples$gen_sp)

species <- read_excel("./sandbox/species.xlsx")
species$gen_sp <- paste(species$Genus, species$species, sep = " ")
speciesSpp <- unique(species$gen_sp)

tol_resolve(speciesSpp)
gnr_resolve(speciesSpp[1:2])

classification("Homo sapiens", db = "itis")
