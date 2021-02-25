#
# calculate mean cluster divergence of all surviving species
# returns a single mean value
# Thomas Keggin
#

clusterDivergenceSim <- function(species){
  
  cluster_divergences <- c()
  
  for(i in 1:length(species)){
    
    div_mat       <- species[[i]]$divergence$compressed_matrix # extract divergence values
    diag(div_mat) <- NA                                        # set self comparisons to NA
    
    div_vec       <- as.vector(div_mat)                        # vectorise all divergence values
    div_vec       <- div_vec[which(!is.na(div_vec))]           # remove self comparisons
    
    cluster_divergences <- c(cluster_divergences,div_vec)
    
  }
  
  cluster_divergence_sim <- mean(cluster_divergences)
  
  return(cluster_divergence_sim)
}
