
# Plots all the richnesses in a directory of a batch of run outputs.
# You can choose to either plot all steps, or just the last one.
plot.option <- "last" # "last" or "all"


# set session ####
#main.dir <- "Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/5.2_all"
main.dir <- "C:/Users/thoma/OneDrive/Documents/PhD/genesis/test_environment/output/1d_2000m_17c/"
setwd(main.dir)

library(tidyverse)
library(ggnewscale)

# load all sea
land_all <- readRDS("D:/genesis/input/1d_all/landscapes.rds")

# list all the runs
runs.file <- list.files("./")

for(run in runs.file){
  
  setwd(paste0("./",run))
  
  # read and quantify timesteps
  timesteps.file <- list.files("./richness/")
  
  # make a sequence of timesteps to plot, choosing either all of them, or just the latest one.
  if(plot.option == "all"){
    timesteps.seq  <- seq(min(parse_number(timesteps.file)),max(parse_number(timesteps.file)))
  } else {
    timesteps.seq <- min(parse_number(timesteps.file))
  }
  
  for(t in timesteps.seq){
    
    # load and wrangle data
    richness <- readRDS(paste0("./richness/richness_t_",t,".rds", sep = ""))
    land     <- readRDS(paste0("./landscapes/landscape_t_",t,".rds", sep = ""))
    
    coords   <- data.frame(land$coordinates)
    richness <- richness[match(rownames(coords),names(richness))]
    
    data <- cbind(coords,richness)
    data$richness[data$richness == 0] <- NA
    
    bathy <- land_all$depth[,c(1,2,t+3)]
    colnames(bathy) <- c("x","y","depth")
    
    # plot time
    rich <- ggplot() +
      # plot depth
      #geom_tile(data = bathy, aes(x=x,y=y,fill = depth)) +
      #scale_fill_gradient("depth",
      #                    low  = "#617190",
      #                    high = "#baccf0",
      #                    na.value = "white") +
      #new_scale_fill() +
      # plot richness
      geom_tile(data = data, aes(x=x,y=y,fill = richness), size = 0.2) + #colour = "#2e2e2e", 
      scale_fill_viridis_c() +
      xlim(c(-180,180)) +
      ylim(c(-90,90)) +
      ggtitle(land$timestep) +
      coord_fixed() +
      theme_void()
    
    jpeg(file.path(paste0("./plots/",sprintf("%04i",t),".jpg")), width = 1360, height = 960)
    print(rich)
    dev.off()
    
    print(paste("run ",run,", step",t))
    
  }
  
  setwd(main.dir)
}

