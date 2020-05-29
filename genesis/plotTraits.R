# set session ####
library(tidyverse)

# create plots directory
dir.create("./plots/traits")

# read and quantify timesteps
timesteps.file <- list.files("./species/")
timesteps.seq  <- seq(802,1200)

for(t in timesteps.seq){
  
  # load species data
  species <- readRDS(paste0("./species/species_t_",t,".rds", sep = ""))
  land    <- readRDS(paste0("./landscapes/landscape_t_",t,".rds", sep = ""))
  
  t_opt <- c() # vector for all the t_opt values
  for(sp in seq(1,length(species))){
    
    # add all the unique t_opt values of a species to the global t_opt vector
    t_opt <- c(t_opt,unique(species[[sp]]$traits[,"t_opt"])) 
  }
  
  # plot a nice histogram
  t_opt <- data.frame(t_opt)
  trait <- ggplot(t_opt, aes(x=t_opt)) +
              geom_histogram(bins = 100, aes(fill = "red")) +
              ggtitle(land$timestep) +
              xlim(c(20,30)) +
              scale_y_continuous(expand = c(0,0)) +
              theme_classic() +
              theme(legend.position = "none")
  
  jpeg(file.path(paste0("./plots/traits/",paste0(sprintf("%04i",t)),".jpg")), width = 1360, height = 960)
  print(trait)
  dev.off()
  
  print(paste("done with ",t))
  
}


