#
# calculate trait standard deviation
# requires createPADF.R
# outputs a vector of trait standard deviations per cell
# Thomas Keggin
#

traitSD <- function(species,landscape,trait){
  
  pa_dataframe    <- createPADF(species,landscape) # generate p/a dataframe
  species_present <- speciesPresent(pa_dataframe) # find which species is in each cell
  
  # replace species IDs with trait values (each cluster is different) in species_present dataframe
  cells <- 1:length(species_present)
  
  traits_present <- list()
  for(c in cells){
    
    trait_values <- c()
    for(s in species_present[[c]]$speciesID){
      trait_value <- species[[s]]$traits[species_present[[c]]$cellID,trait]
      trait_values <- c(trait_values,trait_value)
    }
    
    traits_present[[c]] <- list(cellID      = species_present[[c]]$cellID,
                                traitValues = trait_values)
  }
  
  
  
}