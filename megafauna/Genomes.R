# set session ####

setwd("Z:/LE_projects/megafauna/data/sandbox")

library(tidyverse)
library(readxl)


# load data ####

samples <- read_excel("./Browse samples.xlsx")

genomes <- read_excel("./data_genomes.xlsx")

# create gen_sp column ####

samples$gen_sp <- paste(samples$Genus,samples$species, sep = " ")

genomes$gen_sp <- paste(genomes$genus,genomes$species, sep = " ")

# find sampled species with a genome ####

refGenome <- filter(samples, samples$gen_sp %in% genomedSpecies)

genomedSpecies <- unique(refGenome$gen_sp)

# find sampled species with a genus genome ####

refGenusGenome <- filter(samples, samples$Genus %in% genomes$genus) %>%
  group_by(gen_sp) %>%
  count()


write_csv(refGenusGenome, path = "./genomelist.csv")
