#
# calculates niche metrics per cell
# requires tidyverse
# outputs a data frame of niche metrics
# Thomas Keggin
#


nicheMetricsCell <- function(species,landscape){
  
  # create vector of all niche values
  niche <- c()
  for(sp in 1:length(species)){
    x     <- species[[sp]]$traits[,'niche']
    niche <- c(niche,x)
  }
  
  # create a data frame of niche values and their associated cell ID
  niche_df           <- data.frame(names(niche),niche)
  colnames(niche_df) <- c("cell","niche")
  
  # calculate niche metrics per cell
  niche_min   <- niche_df %>% group_by(cell) %>% summarise(min = min(niche))
  niche_max   <- niche_df %>% group_by(cell) %>% summarise(max = max(niche))
  niche_sd    <- niche_df %>% group_by(cell) %>% summarise(sd  = sd(niche))
  
  # amalgamate into a data frame
  niche_df       <- niche_min
  niche_df$max   <- niche_max$max
  niche_df$range <- niche_df$max - niche_df$min
  niche_df$sd    <- niche_sd$sd
  
  # add coordinate data to each cell
  coords_niche <- data.frame(rownames(landscape$coordinates), landscape$coordinates)
  colnames(coords_niche) <- c("cell","x","y")
  
  niche_metrics <- left_join(coords_niche,niche_df)
  
  return(niche_metrics)

}
