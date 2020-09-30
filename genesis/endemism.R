# set session ####

library(gen3sis)
library(tidyverse)
library(raster)
library(data.table)

setwd("C:/Users/thoma/OneDrive/Documents/PhD/genesis/test_environment/output/from_sim/5_all_12")

# load data ####

landscape <- readRDS("./landscape_t_0.rds")
species   <- readRDS("./species_t_0.rds")

# set functions ####

SpecRich <- function(x){
  mm1 <- data.frame(table(x$grids))
  names(mm1) <- c("grids", "SR")
  mm1
}

weighted.endemism <- function(x){
  tmp <- SpecRich(x)
  index <- match(x$grids, tmp$grids)
  SR <- tmp$SR[index]
  ff <- table(x$species)
  x$WE <- SR/ff[x$species]
  tmp <- as.data.table(x)
  res <- tmp[, sum(WE), by=grids]
  res
}

# create a presence/absence matrix ####

pa_matrix             <- data.frame(matrix(0,nrow=length(landscape$coordinates[,1]), ncol=(length(species)+2))) # create a matrix
pa_matrix[,1:2]       <- landscape$coordinates # add the coordinate information
rownames(pa_matrix)   <- rownames(landscape$coordinates) # set rownames as cell IDs
names(pa_matrix)[1:2] <- c("x", "y")
names(pa_matrix)[3:length(pa_matrix)] <- unlist(lapply(species, FUN=function(x){x$id})) # set column names as species IDs
# fill in the p/a data
for(i in 3:(length(pa_matrix[1,]))){
  pa_matrix[names(species[[i-2]]$abundance),i] <- 1
}



# calculate endemism ####
#transpose pa_matrix
data <- t(pa_matrix)


# calculate species richness per cell
richness <- colSums(data[-c(1,2),])

# calculate species range per species
ranges   <- rowSums(data[-c(1,2),])

# calculate total ranges for all species in a cell
# who is present in the cell?
cell <- pa_matrix[101,]
cell[cell == 1]

filter(data = data[-c(1,2,)], data[,'19258'] == 1)


# divide cell species richness by cell species range








