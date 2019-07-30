# set session ####

library("rgl")
library("raster")
library("tidyverse")

# raster to elevation (stolen from Oskar's compilation script)
convert.grey.to.elev <- function(grey_elev_raster){
  elevation <- grey_elev_raster
  values(elevation) <- (values(grey_elev_raster)-155)*40
  values(elevation)[values(grey_elev_raster) > 230] <- 3000 + ((values(grey_elev_raster)[values(grey_elev_raster) > 230]-230)*300)
  values(elevation)[values(grey_elev_raster) < 5] <- -6000 - ((5-values(grey_elev_raster)[values(grey_elev_raster) < 5])*1000)
  return(elevation)
}

# load data ####

setwd("C:/Users/keggint/polybox/sandbox/elevation/JurrassicTriassic/")

mapNames <- list.files(pattern = "*.tif")

i <- 1
mapList <- c()                           
for(n in mapNames){                       
  print(paste("loading map",i))
  map <- raster(n)
  mapList <- c(mapList,map)
  i <- i+1
  
}

# convert elevation to real values ####

for(m in seq(1:length(mapList))){
  
  print(paste("converting",m,"of",length(mapList)))
  mapList[[m]] <- convert.grey.to.elev(mapList[[m]])
  
  
}

# create and output plots####

setwd("C:/Users/keggint/polybox/sandbox/elevation/3dplots/trijur/")

par3d(windowRect = c(15,30,1900,1100))

mapNames <- substr(mapNames, 1, nchar(mapNames)-4) 

i <- 1
for(l in mapList){
  
  data <- l
  
  data <- as.matrix(data)
  
  z <- 0.0008*data
  x <- -(1:nrow(data))
  y <- (1:ncol(data))
  
  zlim <- c(-7.5,4.6) # values are min/max of z after transformation for the present day
  zrange <- zlim[2] - zlim[1]
  
  colFunc <- colorRampPalette(c("darkblue","lightblue"))
  colSea <- colFunc((-zlim[1]/zrange)*10)
  
  colLand <- terrain.colors(((zlim[2]/zrange)*10)+1)
  
  colLut <- c(colSea,colLand)
  
  col <- colLut[z-zlim[1]+1]
  
  rgl.viewpoint(theta = -90, phi = 75, zoom = 0.5)
  
  rgl.clear()
  
  rgl.surface(x,y,z, back = "lines", color = col)
  
  rgl.snapshot(paste("./",mapNames[i],".png", sep =""), fmt = "png", top = TRUE)
  
  print(paste(i,"of",length(mapList)))
  
  i <- i+1

}


# sandbox ####















