#
# function to create a presence/absence dataframe from a species object and a landscape object
# rows are cells, columns are species
# Thomas Keggin
#

createPADF <- function(species, landscape){
  pa_dataframe             <- data.frame(matrix(0,nrow=length(landscape$coordinates[,1]), ncol=(length(species)+2))) # create a dataframe
  pa_dataframe[,1:2]       <- landscape$coordinates # add the coordinate information
  rownames(pa_dataframe)   <- rownames(landscape$coordinates) # set rownames as cell IDs
  names(pa_dataframe)[1:2] <- c("x", "y")
  names(pa_dataframe)[3:length(pa_dataframe)] <- unlist(lapply(species, FUN=function(x){x$id})) # set column names as species IDs
  # fill in the p/a data
  for(i in 3:(length(pa_dataframe[1,]))){
    pa_dataframe[names(species[[i-2]]$abundance),i] <- 1 # revalue all abundance values to 1 (there or not)
  }
  return(pa_dataframe)
}