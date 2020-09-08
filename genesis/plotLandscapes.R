# set session ####

setwd("D:/genesis/input/1d_all/")

library(tidyverse)
library(gridExtra)
library(viridis)

# load data ####

landscapes <- readRDS("./landscapes.rds")

# plot ####

setwd("./plot")

year <- seq(1200,0)

for(step in 3:1203){
  
  temperature  <- landscapes$temp[,step]
  depth        <- landscapes$depth[,step]
  
  plotTemp <- ggplot(landscapes$temp, aes(x = x, y = y, fill = temperature)) +
    geom_tile() +
    # ggtitle(paste(colnames(landscapes$temp[step]),"mya")) +
    scale_fill_viridis(option = "magma",
                       na.value = "white",
                       limits = c(-30,30)) +
    xlim(-180,180) +
    ylim(-90,90) +
    coord_fixed() +
    theme_void()
  
  
  plotDepth <- ggplot(landscapes$depth, aes(x = x, y = y, fill = depth)) +
    geom_tile() +
    scale_fill_viridis(option = "magma",
                       na.value = "white",
                       begin = 0,
                       end = 0.5,
                        limits = c(-8000,0)) +
    xlim(-180,180) +
    ylim(-90,90) +
    coord_fixed() +
    theme_void()
  
  png(filename=paste0(sprintf("%04i",year[step-2]),".png"))
  grid.arrange(plotTemp, plotDepth, nrow = 2, top = paste(colnames(landscapes$temp[step]),"mya"))
  dev.off()
  
  print(paste(colnames(landscapes$temp)[step]," done"))
}

# latitudinal / longitudinal slices ####

#temperature <- landscapes$temp

#tempSlice <- filter(temperature, x == 0.5)

#tempPlot <- pivot_longer(tempSlice, cols = colnames(tempSlice)[-c(1,2)], names_to = "timestep", values_to = "temperature")

#timesteps <- unique(tempPlot$timestep)

#tempPlot <- filter(tempPlot, timestep %in% sample(timesteps, 50, replace = FALSE))

#ggplot(tempPlot, aes(x = y, y = temperature)) +
 # geom_line(aes(colour = timestep)) +
  #theme_classic()




























