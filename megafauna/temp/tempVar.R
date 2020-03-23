# average sst from the 1985 to 2019 based on NOAA remote sensing.
# set session ####

setwd("D:/ocean temperature/crw/aggregated_MonthToYear_0.25")

library(raster)
library(tidyverse)
library(readxl)
library(gridExtra)

# load and wrangle data ####

# mean temp
meanFiles <- list.files("./SST_mean/")
meanYears <- stack()

for(f in meanFiles){
  
  meanYears <- stack(meanYears, raster(paste0("./SST_mean/",f)))
}

# variance in temp

meanTemp <- mean(meanYears)
sqDiff <- stack()

for(year in seq(1,dim(meanYears)[3])){

  sqDiff  <- stack(sqDiff, (meanTemp-meanYears[[year]])^2)
  print(paste(year," complete"))
}

varTemp <- mean(sqDiff)



# For just 2019

setwd("D:/ocean temperature/crw/monthly/5km/v3.1/nc/v1.0/monthly/2019")


files  <- data.frame(list.files("./"))
colnames(files) <- "file"

files <- filter(files, !grepl("md5",file))
files <- filter(files, grepl("sst-mean",file))

monthTemp <- stack()

for(f in files$file){
  
  monthTemp <- stack(monthTemp, raster(f))
}

meanTemp <- mean(monthTemp)
sqDiff <- stack()

for(year in seq(1,dim(monthTemp)[3])){
  
  sqDiff  <- stack(sqDiff, (meanTemp-monthTemp[[year]])^2)
  print(paste(year," complete"))
}

varTemp <- mean(sqDiff)



