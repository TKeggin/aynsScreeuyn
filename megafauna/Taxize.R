# Script to use the taxize package to return taxonomic categories for species of interest
# for inputting into the Megafauna data_species table.
# Author: Thomas Keggin, thomas.keggin@usys.ethz.ch

# set session ####

library(taxize)
library(tidyverse)
library(readxl)

setwd("M:/data/raw_megafauna_metadata/raw_atlantic")

# pick taxanomic categories you want returned
cats <- c("phylum","class","order","family","genus")

# set search function
do <- function(sp){
  data <- classification(sp, db = "itis")
  data <- data[[1]]
  data <- filter(data, rank %in% cats)
  c(data$name, sp)
}

# loop for chosen genera ####

input <- read_excel("./Samples_Atlantic12S_Met_zurich.xlsx", sheet = 2)
species <- unique(input$species)

taxa <- as.character()
t <- 1

for(i in species[t:length(species)]){
  t <- t + 1
  x <- do(i)
  taxa <- rbind(taxa,x)
  print(t)
}
taxa.df <- data.frame(taxa)
colnames(taxa.df) <- c(cats,"species")
