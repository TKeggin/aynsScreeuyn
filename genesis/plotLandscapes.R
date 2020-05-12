setwd("D:/genesis/input/1d_2000m")

library(tidyverse)
library(gridExtra)

landscapes <- readRDS("./landscapes.rds")

# setwd("./plots")

for(i in seq(3,6)){
  
  step <- i
  
  tempFill  <- landscapes$temp[,step]
  depthFill <- landscapes$depth[,step]
  
  plotTemp <- ggplot(temp, aes(x = x, y = y, fill = tempFill)) +
    geom_tile() +
    ggtitle(paste(colnames(temp[step]),"mya")) +
    scale_fill_gradient2(low = "blue",
                         mid = "yellow",
                         high = "red",
                         na.value = "white",
                         limits = c(20,40)) +
    xlim(-180,180) +
    ylim(-90,90) +
    coord_fixed() +
    theme_void()
  
  
  plotDepth <- ggplot(depth, aes(x = x, y = y, fill = depthFill)) +
    geom_tile() +
    scale_fill_gradient(low = "black",
                          high = "lightblue",
                          na.value = "white") +
    xlim(-180,180) +
    ylim(-90,90) +
    coord_fixed() +
    theme_void()
  
  png(filename=paste0(i,".png"))
  grid.arrange(plotTemp, plotDepth, nrow = 2)
  dev.off()
}
