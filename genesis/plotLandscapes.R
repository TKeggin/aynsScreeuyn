setwd("D:/genesis/input/1d_all")

library(tidyverse)
library(gridExtra)

landscapes <- readRDS("./landscapes.rds")

setwd("./plot")

for(step in seq(3,1203)){
  
  tempFill  <- landscapes$temp[,step]
  depthFill <- landscapes$depth[,step]
  
  plotTemp <- ggplot(landscapes$temp, aes(x = x, y = y, fill = tempFill)) +
    geom_tile() +
    ggtitle(paste(colnames(landscapes$temp[step]),"mya")) +
    scale_fill_gradient2(low = "darkblue",
                         #mid = "grey",
                         high = "darkred",
                         na.value = "grey30",
                         midpoint = 0,
                         limits = c(-41,30)) +
    xlim(-180,180) +
    ylim(-90,90) +
    coord_fixed() +
    theme_void()
  
  
  plotDepth <- ggplot(landscapes$depth, aes(x = x, y = y, fill = depthFill)) +
    geom_tile() +
    scale_fill_gradient(low = "darkblue",
                        high = "white",
                        na.value = "grey30",
                        limits = c(-7610.05,0)) +
    xlim(-180,180) +
    ylim(-90,90) +
    coord_fixed() +
    theme_void()
  
  png(filename=paste0(step-3,".png"))
  grid.arrange(plotTemp, plotDepth, nrow = 2)
  dev.off()
  
  print(paste(step-3," done"))
}
