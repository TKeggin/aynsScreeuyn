# set session ####

library(tidyverse)
library(raster)
library(readxl)

# set functions ####
convert.grey.to.elev <- function(grey_elev_raster){
  elevation <- grey_elev_raster
  values(elevation) <- (values(grey_elev_raster)-155)*40
  values(elevation)[values(grey_elev_raster) > 230] <- 3000 + ((values(grey_elev_raster)[values(grey_elev_raster) > 230]-230)*300)
  values(elevation)[values(grey_elev_raster) < 5] <- -6000 - ((5-values(grey_elev_raster)[values(grey_elev_raster) < 5])*1000)
  return(elevation)
}

# load output data ####

landscape <- readRDS("D:/genesis/input/1d_all/landscapes.rds")
depthOutput     <- landscape$temp

# load raw data ####

setwd("C:/Users/thoma/OneDrive/Documents/Marrey/utilities/input_compilation")
geoDepthListRaw <- c()                               # set a blank list in which to put the imported data files
depthExt <- extent(-180, 180, -90, 90)               # set the extent to match temp rasters
depthTemplate <- raster(nrow=180, ncol=360, crs=NA)  # set a blank raster to resample into

depthNames <- list.files("./data/elevation/all/", pattern = "*.tif")     # names of all the data files

for(d in depthNames){                           # import all the data files and store them in geoDepthListRaw
  
  depth <- raster(paste("./data/elevation/all/",d, sep = ""))
  extent(depth) <- depthExt
  depth <- resample(depth, depthTemplate, method = "bilinear") # how does this resampling affect the accuracy of the data? # check before and after reef zones
  depth <- aggregate(depth, fact=1)                   # set the resolution to target resolution
  geoDepthListRaw <- c(geoDepthListRaw,depth)
  print(d)
}

# filter raw depths for target time period and assign dates to timesteps
elevationTimes <- read_excel("./data/elevation/elevation_times.xlsx")   # adapted from Cris Scotese's "Animation FrameAge_v8 .xls" file (headers changed)
elevationStart <- filter(elevationTimes, cum_frame == 1)               # save the present day raster
elevationTimes <- filter(elevationTimes, age != 0 & age <= 200)        # filter out present day and rasters beyond 200 million years
elevationTimes <- rbind(elevationStart, elevationTimes)

targetElevations <- c()
for(raster in c(elevationTimes$cum_frame)){
  
  rasteri <- geoDepthListRaw[[raster]]
  targetElevations <- c(targetElevations, rasteri)
}

targetElevations <- stack(targetElevations)

# convert raw data to landscape dataframe

depthRaw <- as.data.frame(targetElevations[[1]], xy=T)
depthRaw <- depthRaw[,-3]

for(raster in seq(1,dim(targetElevations)[3])){
  
  raster.df <- as.data.frame(targetElevations[[raster]], xy=T)
  depthRaw <- cbind(depthRaw,raster.df[,3])
  
}
colnames(depthRaw) <- c("x","y",format(round(elevationTimes$age, 2), nsmall = 2))

# convert raw depth to elevation values ####

convertedElevations <- convert.grey.to.elev(targetElevations)

for(t in targetElevations){
  
  geoDepthList[[t]] <- convert.grey.to.elev(geoDepthListRaw[[t]])
  
}

# convert raw data to landscape dataframe

depthRawConverted <- as.data.frame(convertedElevations[[1]], xy=T)
depthRawConverted <- depthRawConverted[,-3]

for(raster in seq(1,dim(convertedElevations)[3])){
  
  raster.df <- as.data.frame(convertedElevations[[raster]], xy=T)
  depthRawConverted <- cbind(depthRawConverted,raster.df[,3])
  
}
colnames(depthRawConverted) <- c("x","y",format(round(elevationTimes$age, 2), nsmall = 2))

# create time series of one cell ####

# output
depth <- as.numeric(depthOutput[32400,])
depth <- depth[-c(1,2)]
time         <- as.numeric(colnames(depthOutput)[-c(1,2)])
seriesOutput <- data.frame(time,depth)

# raw
depth <- as.numeric(depthRaw[32400,])
depth <- depth[-c(1,2)]
time      <- as.numeric(colnames(depthRaw)[-c(1,2)])
seriesRaw <- data.frame(time,depth)

# converted raw

depth <- as.numeric(depthRawConverted[32400,])
depth <- depth[-c(1,2)]
time      <- as.numeric(colnames(depthRawConverted)[-c(1,2)])
seriesRawConverted <- data.frame(time,depth)

# plot data ####

# separately
ggplot(seriesOutput, aes(x = time, y = depth, group=1)) +
  geom_line() +
  theme_classic()

ggplot(seriesRaw, aes(x = time, y = depth, group=1)) +
  geom_line() +
  theme_classic()

ggplot(seriesRawConverted, aes(x = time, y = depth, group=1)) +
  geom_line() +
  theme_classic()

# together

seriesTogether <- rbind(seriesOutput,seriesRawConverted)

ggplot() +
  geom_line(data = seriesOutput, aes(x = time, y = depth, group=1), colour = "red") +
  geom_line(data = seriesRawConverted, aes(x = time, y = depth, group=2))


# sandbox ####

timeseriesRaw <- as.numeric()

for(step in seq(1:length(targetElevations))){
  
  timeseriesRaw <- c(timeseriesRaw,targetElevations[[step]][180,90])
}

timeseriesInter <- as.numeric()

for(step in seq(1:length(geoDepthList))){
  
  timeseriesInter <- c(timeseriesInter,geoDepthList[[step]][180,90])
}

plot(timeseriesRaw, type = "l", lty = 1)
lines(timeseriesInter, type = "l", lty = 1, col = "red")

notInterpolated <- data.frame(elevationTimes$age,timeseriesRaw)
colnames(notInterpolated) <- c("time","depth")
interpolated <- data.frame(geoTimes,timeseriesInter)
colnames(interpolated) <- c("time","depth")


ggplot() +
  geom_line(data = interpolated, aes(x = time, y = depth), colour = "red") +
  geom_line(data = notInterpolated, aes(x = time, y = depth))


ggplot(depthOutput, aes(x = x, y = y, fill = depthOutput$`100.00`)) +
  geom_tile()

mean(depthOutput)








