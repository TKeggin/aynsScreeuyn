setwd("D:/genesis/input/1d_2000m_20c")

library(tidyverse)
library(gridExtra)
library(viridis)

landscapes <- readRDS("./landscapes.rds")

plot_names <- seq(dim(landscapes$temp)[2]-3,0)

setwd("./plot")

year <- seq(1200,0)

for(step in seq(1203,3)){
  
  tempFill  <- landscapes$temp[,step]
  depthFill <- landscapes$depth[,step]
  
  plotTemp <- ggplot(landscapes$temp, aes(x = x, y = y, fill = tempFill)) +
    geom_tile() +
    ggtitle(paste(colnames(landscapes$temp[step]),"mya")) +
    scale_fill_viridis(option = "magma",
                       na.value = "lightgrey",
                       limits = c(20,30)) +
    xlim(-180,180) +
    ylim(-90,90) +
    coord_fixed() +
    theme_void()
  
  
  plotDepth <- ggplot(landscapes$depth, aes(x = x, y = y, fill = depthFill)) +
    geom_tile() +
    scale_fill_gradient(low = "darkblue",
                        high = "white",
                        na.value = "lightgrey",
                        limits = c(-2000,0)) +
    xlim(-180,180) +
    ylim(-90,90) +
    coord_fixed() +
    theme_void()
  
  #png(filename=paste0(step-3,".png"))
  png(filename=paste0(sprintf("%04i",year[step-2]),".png"))
  grid.arrange(plotTemp, plotDepth, nrow = 2)
  dev.off()
  
  print(paste(plot_names[step-2]," done"))
}



