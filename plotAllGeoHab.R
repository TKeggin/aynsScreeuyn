setwd("C:/Users/keggint/polybox/Zurich/images/gasm")


library(tidyverse)
library(gridExtra)

temp <- all_geo_hab$temp
depth <- all_geo_hab$depth


tempFill <- temp[,1277]
depthFill <- depth[,1277]

plotTemp <- ggplot(temp, aes(x = x, y = y, fill = tempFill)) +
              geom_tile() +
              scale_fill_continuous(low = "black",
                                    high = "lightgrey",
                                    na.value = "white",
                                    limits = c(-40,60)) +
              xlim(-180,180) +
              ylim(-90,90) +
              coord_fixed() +
              theme_void()


plotDepth <- ggplot(depth, aes(x = x, y = y, fill = depthFill)) +
              geom_tile() +
              scale_fill_continuous(low = "black",
                                    high = "lightgrey",
                                    na.value = "white",
                                    limits = c(-200,0)) +
              xlim(-180,180) +
              ylim(-90,90) +
              coord_fixed() +
              theme_void()

grid.arrange(plotTemp, plotDepth, nrow = 2)


# sandbox


for(i in seq(3:6)){
  
  depthFill <- depth[,i]
  
  png(filename=paste(i,".png"))
  ggplot(depth, aes(x = x, y = y, fill = depthFill)) +
    geom_tile() +
    scale_fill_continuous(low = "black",
                          high = "lightgrey",
                          na.value = "white",
                          limits = c(-200,0)) +
    xlim(-180,180) +
    ylim(-90,90) +
    coord_fixed() +
    theme_void()
  dev.off()
  
}