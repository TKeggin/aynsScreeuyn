# set the session ####

library(tidyverse)
library(reshape2)
library(magick)

setwd ("C:/Users/Thomas/OneDrive/Documents/Zurich/gasm/GaSM/Output/WorldMap200-0Ma_multiple_2d/keggin_001/config_00722/geo")

timeSteps <- list.files(pattern = "*.RData") # make a list of all the timesteps

for(i in timeSteps){
  
  load(i)
  
  data <- as.data.frame(geo_sp_ti)                  # convert to dataframe
  
  spCount <- rowSums(data[,-c(1:2), drop = FALSE])  # count number of spp per cell
  spCount <- as.factor(spCount)                     # change count to factor
  data["spCount"] <- spCount                        # append spp number to rows
  
  plotID <- substr(i,10,13)                         # extract timestep from file name
  plotDest <- paste("./geoPlots/",plotID,".png", sep = "")
  
  png(filename = plotDest,
      width=3200, height=1800, res=300)
  
  plot <- ggplot(data, aes(x = x, y = y, fill = spCount)) +
    geom_tile() +
    scale_fill_brewer(type = "seq", palette = "Reds") +
    xlim(-180,180) +
    ylim(-90,90) +
    ggtitle(plotID) +
    coord_fixed() +
    theme_void() +
    theme(plot.title = element_text(hjust=0.5))
  
  print(plot)
  
  dev.off()
  
}

# make the animation (backwards and long)

setwd ("./geoPlots")

images <- list.files(pattern = "*.png")

images <- map(images,image_read)
images <- image_join(images)

anime <- image_animate(images, fps = 1, dispose = "previous")

image_write(anime, "animation.gif")







