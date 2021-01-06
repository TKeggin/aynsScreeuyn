#
# calculate the summed ranges of all species in a cell
# requires speciesRange()
# outputs a vector of cell ranges
# Thomas keggin
#

cellRange <- function(pa_dataframe,landscape){
  
  # find species ranges
  species_range <- speciesRange(pa_dataframe)
  
  # find species present in each cell
  species_present <- speciesPresent(pa_dataframe)
  
  no_cells <- dim(landscape$coordinates)[1]
  cell_range <- c()
  
  for(i in 1:no_cells){
    
    # for all the species in a cell, sum their ranges
    x <- mean(species_range[species_present[[i]]$speciesID])
    cell_range <- c(cell_range,x)
  }
  names(cell_range) <- rownames(landscape$coordinates)
  
  return(cell_range)
  
}
