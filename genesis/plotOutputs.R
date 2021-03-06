# set session ####
library(tidyverse)
library(ggnewscale)
library(ggsci)
library(RColorBrewer)
library(viridis)

# create plots directory
dir.create("./plots/")

# load all sea
land_all <- readRDS("D:/genesis/input/1d_all/landscapes.rds")

# read and quantify timesteps
timesteps.file <- list.files("./richness/")
timesteps.seq  <- seq(802,1200)

for(t in timesteps.seq){
  
  # load and wrangle data
  richness <- readRDS(paste0("./richness/richness_t_",t,".rds", sep = ""))
  land     <- readRDS(paste0("./landscapes/landscape_t_",t,".rds", sep = ""))
  
  coords <- data.frame(land$coordinates)
  richness <- richness[match(rownames(coords),names(richness))]
  
  data <- cbind(coords,richness)
  data$richness[data$richness == 0] <- NA
  
  bathy <- land_all$temp[,c(1,2,t+3)]
  colnames(bathy) <- c("x","y","depth")
  
  # plot time
  rich <- ggplot() +
    # plot depth
    geom_tile(data = bathy, aes(x=x,y=y,fill = depth)) +
    scale_fill_viridis(option = "cividis") +
    new_scale_fill() +
    # plot richness
    geom_tile(data = data, aes(x=x,y=y,fill = richness), colour = "#2e2e2e", size = 0.2) +
    scale_fill_gradient("richness",
                        low  = "#ff9b8b",
                        high = "#ff2a00",
                        na.value = "transparent") +#,
    #limits = c(0,2)) +
    xlim(c(-180,180)) +
    ylim(c(-90,90)) +
    ggtitle(land$timestep) +
    coord_fixed() +
    theme_void()
  
  jpeg(file.path(paste0("./plots/",paste0(sprintf("%04i",t)),".jpg")), width = 1360, height = 960)
  print(rich)
  dev.off()
  
  print(t)
  
}


