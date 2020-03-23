# average sst from the 1985 to 2019 based on NOAA remote sensing.
# set session ####

setwd("C:/Users/thoma/Documents/sandbox/sst")

library(raster)
library(tidyverse)
library(readxl)
library(gridExtra)

# load and wrangle data ####

# load data
mean       <- raster("sst_mean.nc")
min        <- raster("sst_min.nc")
max        <- raster("sst_max.nc")

samples.df <- read_excel("Z:/LE_projects/megafauna/data/sandbox/browse samples.xlsx")
samples.df <- filter(samples.df, grepl("Chromis",species))

# put data in list and calculate temperature range
data <- list(mean  = mean,
             max   = max,
             min   = min,
             range = max-min)

stat <- names(data)

# set temperature range of interest
minTemp <- 22
maxTemp <- 32

# convert to dataframe and apply temperature range
for(s in stat[1:3]){
  
  data[[s]] <- as.data.frame(data[[s]], xy = TRUE)
  colnames(data[[s]]) <- c("x","y","temperature")
  data[[s]]$temperature[data[[s]]$temperature<minTemp] <- minTemp
}

# apply separate temperature range to range values
data$range <- as.data.frame(data$range, xy = TRUE)
colnames(data$range) <- c("x","y","temperature")
data$range$temperature[data$range$temperature>8] <- 0

# plot data ####

plotList <- list()

for(s in stat[1:3]){
  
  plotList[[s]] <-   ggplot() +
                          geom_raster(data = data[[s]], aes(x=x, y=y, fill = temperature)) +
                          geom_point(data = samples.df, aes(x=longitude, y=latitude)) +
                          scale_fill_gradientn(colours = rev(terrain.colors(10)), limits = c(minTemp,maxTemp)) +
                          coord_fixed() +
                          ggtitle(s) +
                          theme_void()
}

plotList$range <-   ggplot() +
  geom_raster(data = data$range, aes(x=x, y=y, fill = temperature)) +
  geom_point(data = samples.df, aes(x=longitude, y=latitude)) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  coord_fixed() +
  ggtitle("range") +
  theme_void()

grid.arrange(plotList$mean, plotList$max, plotList$min, plotList$range, nrow = 2)









































  