# set session ####
setwd("D:/genesis/input/1d_2000m_17c")

library(tidyverse)
library(gridExtra)
library(viridis)
library(ggnewscale)

# load data ####

landscapes <- readRDS("./landscapes.rds")
land_all   <- readRDS("D:/genesis/input/1d_all/landscapes.rds")

# produce plots ####
setwd("./plot")

year <- seq(1200,0)

for(step in seq(1203,3)){
  
  temperature  <- landscapes$temp[,step]
  temp_all     <- land_all$temp[,step]
  depth        <- landscapes$depth[,step]
  depth_all    <- land_all$depth_all[,step]
  
  plotTemp <- ggplot() +
    ggtitle(paste(colnames(landscapes$temp[step]),"mya")) +
    # background temperature
    geom_tile(data = land_all$temp, aes(x = x, y = y, fill = temp_all), alpha = 0.4) +
    scale_fill_viridis(option = "plasma",
                       limits = c(-41,30),
                       na.value = "black") +
    # target temperature
    new_scale_fill() +
    geom_tile(data = landscapes$temp, aes(x = x, y = y,
                                          fill = temperature)) +
    scale_fill_viridis(option = "plasma",
                       limits = c(-41,30),
                       na.value = "transparent") +
    xlim(-180,180) +
    ylim(-90,90) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none")
  
  
  plotDepth <- ggplot(landscapes$depth, aes(x = x, y = y, fill = depth)) +
    geom_tile() +
    scale_fill_gradient(low = "darkblue",
                        high = "white",
                        na.value = "grey30",
                        limits = c(-7610.05,0)) +
    xlim(-180,180) +
    ylim(-90,90) +
    coord_fixed() +
    theme_void()
  
  #png(filename=paste0(step-3,".png"))
  png(filename=paste0(sprintf("%04i",year[step-2]),".png"))
  grid.arrange(plotTemp, plotDepth, nrow = 2)
  dev.off()
  
  print(paste(step-3," done"))
}



