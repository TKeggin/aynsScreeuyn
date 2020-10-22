#
# calculate the total number of clusters for each species in a species object
# returns a vector of summed clusters per species
# Thomas Keggin
#

clustersTotal <- function(species){
  clusters_total <- c()
  for(sp in 1:length(species)){
    x        <- species[[sp]]$divergence$index
    clusters_total <- c(clusters_total,length(unique(x)))
  }
  names(clusters_total) <- unlist(lapply(species, FUN=function(x){x$id})) # assign species IDs
  return(clusters_total)
}
