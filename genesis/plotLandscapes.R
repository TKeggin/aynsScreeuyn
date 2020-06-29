# set session ####

setwd("D:/genesis/input/interpolation_bugged/1d_2000m_15c/")

library(tidyverse)
library(gridExtra)
library(viridis)

# load data ####

landscapes <- readRDS("./landscapes.rds")

# plot ####

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
                       limits = c(15,30)) +
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
  
  print(paste(step-3," done"))
}

# latitudinal / longitudinal slices ####

temperature <- landscapes$temp

tempSlice <- filter(temperature, x == 0.5)

tempPlot <- pivot_longer(tempSlice, cols = colnames(tempSlice)[-c(1,2)], names_to = "timestep", values_to = "temperature")

timesteps <- unique(tempPlot$timestep)

tempPlot <- filter(tempPlot, timestep %in% sample(timesteps, 50, replace = FALSE))

ggplot(tempPlot, aes(x = y, y = temperature)) +
  geom_line(aes(colour = timestep)) +
  theme_classic()




























