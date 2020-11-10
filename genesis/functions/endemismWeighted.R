#
# calculate weighted endemism from a species and a landscape object
# requires createPADF(), speciesRange(), speciesPresent()
# returns a vector of weighted endemism (richness/total species ranges)
# Thomas Keggin
#

endemismWeighted <- function(species, landscape){
  
  pa_dataframe    <- createPADF(species,landscape) # find species presence for each cell
  species_range   <- speciesRange(pa_dataframe) # find range size for each species
  species_present <- speciesPresent(pa_dataframe) # find which species are present in each cell
  
  # find total range size for each cell
  range_total <- c()
  for(cell in 1:dim(landscape$coordinates)[1]){
    
    range_total <- c(range_total,sum(species_range[species_present[[cell]]$speciesID]))
  }
  names(range_total) <- rownames(landscape$coordinates)
  
  endemism_weighted <- richness/range_total # species richness / total range
  return(endemism_weighted)
}