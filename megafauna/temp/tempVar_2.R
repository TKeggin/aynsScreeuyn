# average sst from the 1985 to 2019 based on NOAA remote sensing.
# set session ####

setwd("D:/ocean temperature/crw/daily/coraltemp/v1.0/nc/")

library(raster)
library(tidyverse)
library(readxl)
library(gridExtra)

# load and wrangle data ####

# get list of all years
years   <- list.files("./")[-36]

# calculate average ####

# create blank raster for cumulative average for all the years calculated so far
cumyearAvg <- setValues(raster(res = c(0.05,0.05)), 0)

# start loop to calculate the average for each year
for(y in years){

  # list all the raster files for the year
  days   <- list.files(y)
  ncOnly <- !grepl(".md5", days)
  files  <- data.frame(days, ncOnly) %>% filter(ncOnly == TRUE)
  
  # create a blank raster to add the daily averages in with
  cumAdd <- setValues(raster(res = c(0.05,0.05)), 0)
  
  # start loop to add together all the rasters for the year
  i <- 1
  for(f in files$days){
    cumAdd <- raster(paste0("./",y,"/",f)) + cumAdd
    print(paste(f, " added Raster ", i, " of ", length(files$days), " complete for year ", y))
    i <- i+1
  }
  
  # update the cumulative average of the years added so far
  cumyearAvg <- cumAdd/length(files$days) + cumyearAvg

}

totalAvg <- cumyearAvg/length(years)

# calculate average squared differences ####

# create blank raster for cumulative average for all the years calculated so far
cumsqrAvg <- setValues(raster(res = c(0.05,0.05)), 0)

# start loop to calculate the average for each year
for(y in years){
  
  # list all the raster files for the year
  days   <- list.files(y)
  ncOnly <- !grepl(".md5", days)
  files  <- data.frame(days, ncOnly) %>% filter(ncOnly == TRUE)
  
  # create a blank raster to add the daily averages in with
  cumDiff <- setValues(raster(res = c(0.05,0.05)), 0)
  
  # start loop to add together all the sqr. diff. for the year
  i <- 1
  for(f in files$days){
    
    sqrDiff <- (totalAvg - raster(paste0("./",y,"/",f)))^2
    cumDiff <- sqrDiff + cumDiff
    print(paste(f, " added Raster ", i, " of ", length(files$days), " complete for year ", y))
    i <- i+1
  }
  
  # update the cumulative average of the years added so far
  cumsqrAvg <- cumDiff/length(files$days) + cumsqrAvg
  
}

variance <- cumsqrAvg/length(years)

# calculate variance ####






















saveRDS(totalAvg, file = "./totalAvg")















# alternate load each year ####

setwd("./1985/")

# list all the raster files for the year
days   <- list.files("./")
ncOnly <- !grepl(".md5", days)
files  <- data.frame(days, ncOnly) %>% filter(ncOnly == TRUE)

year <- stack()

for(d in files$days){
  year <- stack(year,raster(d))
  print(d)
}






