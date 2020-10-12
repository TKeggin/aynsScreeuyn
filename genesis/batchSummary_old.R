main.dir <- "Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/2_tolerance"
setwd(main.dir)

library(tidyverse)
library(gen3sis)

# list all the runs
runs.file <- list.files("./")

total_species <- c()
timestep      <- c()

for(i in 1:length(runs.file)){
  
  # specify run
  run <- runs.file[i] 
  setwd(paste0("./",run))

  # read and quantify timesteps
  timesteps.file  <- list.files("./species/")
  timestep.latest <- min(parse_number(timesteps.file))
  
  species <- readRDS(paste0("./species/species_t_",timestep.latest,".rds", sep = ""))
  
  total_species <- c(total_species,length(species))
  timestep      <- c(timestep,timestep.latest)
  
  print(paste("run ",run,": ",length(species)))
  
  setwd(main.dir)
}

summary <- data.frame(runs.file,total_species,timestep)

summary <- readRDS("./sgen3sis.rds")
plot_summary(summary)
