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
  distribution <- rownames_to_column(data.frame(land$coordinates))
  
  # put all species abundances into distribution dataframe and convert to binary presence/absence
  for(i in spp){
    sp <- species[[i]]$abundance
    sp[!is.na(sp)] <- 1
    sp <- rownames_to_column(data.frame(sp))
    
    distribution <- left_join(distribution,sp, by = "rowname")
    print(paste("calculating species ",i))
  }
  colnames(distribution) <- c("cell","x","y",spp)
  
  # calculate species richness
  if(dim(distribution)[2]>4){
    richness <- rowSums(!is.na(distribution[,-c(1:3)]))
  } else {
    richness <- distribution[,4]
  }
  distribution <- cbind(distribution,richness)
  
  # plot and print species richness
  jpeg(file.path("./plots", paste0(sprintf("%04i",t) ,".jpg")), width = 680, height = 480)
    print(ggplot(data = distribution, aes(x=x,y=y)) +
            geom_raster(aes(fill = richness)) +
            scale_fill_gradientn(colours = rev(terrain.colors(10))) +
            xlim(c(-180,180)) +
            ylim(c(-90,90)) +
            ggtitle(paste0(sprintf("%04i",t))) +
            coord_fixed() +
            theme_light()
    )
  dev.off()
  
  print(paste(t, "complete"))
  
}
