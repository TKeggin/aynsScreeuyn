# set session ####
library(tidyverse)
library(raster)
library(viridis)

setwd("R:/data/raw_reefish_fish.range.global")

# load data ####

# this is already at 1d resolution :)
load("./mat_pa_cam_vs_gaspar.Rdata")
data <- ugly_mat_pa
rm(ugly_mat_pa)

# calculate richness ####

data$richness <- rowSums(data[,-c(1,2)])

data <- data[,c("Longitude","Latitude","richness")]
rich <- filter(data, richness > 1)

# mask data with input generation ####

land <- readRDS("D:/genesis/input/1d_2000m_17c/landscapes.rds")$temp[,c(1,2,3)]
land <- filter(land, land[,3] > 1)


land.raster <- rasterFromXYZ(land) # input raster
rich.raster <- rasterFromXYZ(rich) # richness raster
rich.raster <- resample(rich.raster, land.raster) # match extents
#land.raster <- mask(land.raster, rich.raster)
rich.masked <- mask(rich.raster, land.raster) # mask richness by the input

rich.true <- as.data.frame(rich.masked, xy = TRUE)

# load in output ####

rich.out <- readRDS("D:/genesis/output/11/richness/richness_t_0.rds")
land.out <- readRDS("D:/genesis/output/11/landscapes/landscape_t_0.rds")
coords   <- land.out$coordinates
rich.out <- data.frame(coords,rich.out)

# merge everything ####
rich.all <- merge(rich.out,rich.true, by = "row.names")
rich.all <- rich.all[,c(1,2,3,4,7)]
colnames(rich.all) <- c("cell","x","y","rich.out","rich.true")
rich.all <- filter(rich.all, !is.na(rich.true))

# normalise to 0-1 to make comparable ####
rich.all$rich.true <- rich.all$rich.true/max(rich.all$rich.true)
rich.all$rich.out  <- rich.all$rich.out/max(rich.all$rich.out)
rich.all$discrepency <- sqrt((rich.all$rich.true-rich.all$rich.out)^2)

discrepency <- mean(rich.all$discrepency)


# plot ####

ggplot(data = rich.all, aes(x = x, y = y)) +
  geom_tile(aes(fill = discrepency)) + 
  scale_fill_viridis_c(direction = -1) +
  coord_fixed()
