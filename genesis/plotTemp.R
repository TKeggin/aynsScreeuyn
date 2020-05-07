setwd("D:/genesis/input/1d_1000m")
library(rgl)
library(raster)
library(animation)

data <- readRDS("landscapes.rds")

depth <- rasterFromXYZ(data$depth)
temp  <- rasterFromXYZ(data$temp)

# create and output plots####



depth.mat <- as.matrix(depth[[1]])
temp.mat  <- as.matrix(temp[[1]])

z <- depth.mat*0.01
x <- -(1:nrow(depth.mat))
y <- (1:ncol(depth.mat))

col <- temp.mat + 30

colFunc <- colorRampPalette(c("darkblue","lightblue"))
colSea <- colFunc(col)

rgl.open()

rgl.surface(x,y,z, color = col)
rgl.surface(x,y,z, back = "lines", color = colFunc)


