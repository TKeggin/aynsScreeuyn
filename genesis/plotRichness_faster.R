# set session ####

library(tidyverse)

# plot species richness ####

# create plots directory
dir.create("./plots")

# read and quantify timesteps
timesteps.file <- list.files("./species/")
timesteps.seq  <- seq(0,length(timesteps.file)-1)

for(t in timesteps.seq){

  # load data
  species <- readRDS(paste0("./species/species_t_",t,".rds", sep = ""))
  land    <- readRDS(paste0("./landscapes/landscape_t_",t,".rds", sep = ""))
  
  # find number of species present
  spp <- seq(1, length(species))
  
  # create a dataframe in which to put species distributions
  distribution <- data.frame(land$coordinates)
  
  # put all species abundances into distribution dataframe and convert to binary presence/absence
  for(i in spp){
    
    sp <- data.frame(species[[i]]$abundance)
    
    test <- merge(distribution,sp, by = 0, all.x = TRUE)
    
    distribution <- merge(distribution,sp, by = 0)
    print(paste("calculating species ",i))
  }
  colnames(distribution) <- c("cell","x","y",spp)
  
  # calculate species richness
  if(dim(distribution)[2]>4){
    richness <- rowSums(!is.na(distribution[,-c(1:3)]))
    richness[richness == 0] <- NA
  } else {
    richness <- distribution[,4]
  }
  distribution <- cbind(distribution,richness)
  
  # plot and print species richness
  jpeg(file.path("./plots", paste0(sprintf("%04i",length(timesteps.seq)-t) ,".jpg")), width = 1360, height = 960)
    print(ggplot(data = distribution, aes(x=x,y=y)) +
            geom_tile(aes(fill = richness)) +
            scale_fill_gradient(low  = "#fee6ce",
                                high = "#e6550d",
                                na.value = "grey50") +
            xlim(c(-180,180)) +
            ylim(c(-90,90)) +
            ggtitle(paste0(sprintf("%04i",t))) +
            coord_fixed() +
            theme_light()
    )
  dev.off()
  
  print(paste(t, "complete"))
  
}
