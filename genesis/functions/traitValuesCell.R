#
# calculate trait vector
# outputs a vector of trait values
# Thomas Keggin
#


traitValuesCell <- function(species,landscape,trait){
  
  # create vector of all trait values
  trait_vector <- c()
  for(sp in 1:length(species)){
    x            <- species[[sp]]$traits[,paste0(trait)]
    trait_vector <- c(trait_vector,x)
  }
  
  trait_df <- data.frame(names(trait_vector), trait_vector)
  colnames(trait_df) <- c("cell",paste0(trait))
  
  # add coordinate data to each cell
  coords_trait <- data.frame(rownames(landscape$coordinates), landscape$coordinates)
  colnames(coords_trait) <- c("cell","x","y")
  
  trait_values <- left_join(coords_trait,trait_df)
  
  return(trait_values)

}
