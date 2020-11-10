#
# calculate cluster divergence per species
# returns a vector of the mean divergence of each species
# Thomas Keggin
#


speciesDivergence <- function(species){
  
  # calculate average divergence per species (~ global Fst)
  species_divergence <- c()
  for(sp in 1:length(species)){
    
    divergence_matrix <- get_divergence_matrix(species[[sp]])
    mean_divergence <- sum(divergence_matrix)/(length(divergence_matrix)-dim(divergence_matrix)[1]) # mean divergence (remove self comparisons)
    species_divergence <- c(species_divergence,mean_divergence)
  }
  names(species_divergence) <- 1:length(species)
  
}
