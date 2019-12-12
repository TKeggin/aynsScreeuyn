# set session ####

library(taxize)
library(tidyverse)
library(readxl)

setwd("C:/Users/keggint/Desktop")

# pick taxanomic categories you want returned
cats <- c("phylum","class","order","family")

# set search function
do <- function(gen){
  data <- classification(gen, db = "itis")
  data <- data[[1]]
  data <- filter(data, rank %in% cats)
  c(data$name, gen)
}

# loop for chosen genera ####

genera <- read_excel("./data_species.xlsx")
genera <- unique(genera$genus)

taxa <- as.character()
t <- 2
for(i in genera[t:50]){
  t <- t + 1
  x <- do(i)
  taxa <- rbind(taxa,x)
  print(t)
}
data.frame(taxa)
colnames(taxa) <- c(cats,"genus")

