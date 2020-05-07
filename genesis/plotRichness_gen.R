setwd("Y:/TKeggin/genesis/v0.6.1/output/6d/1.2.1")

library(gen3sis)

# read and quantify timesteps
timesteps.file <- list.files("./species/")
timesteps.seq  <- seq(1,length(timesteps.file))
stepnames      <- substring(timesteps.file, 11)
stepnames      <- gsub('.{4}$', '', stepnames)
stepnames      <- sprintf("%04i",as.numeric(stepnames))

for(t in timesteps.seq){
  
  # load data
  species <- readRDS(paste0("./species/species_t_",t,".rds", sep = ""))
  land    <- readRDS(paste0("./landscapes/landscape_t_",t,".rds", sep = ""))
  
  # plot data
  jpeg(file.path("./plots", paste0(stepnames[t] ,".jpg")), width = 680, height = 480)
  plot_richness(species,land)
  dev.off()
  
  print(paste("step ",stepnames[t]," done."))
}
