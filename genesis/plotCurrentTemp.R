# set session ####

library(tidyverse)
library(sdmpredictors)
library(viridis)

# load input data ####

landscapes <- readRDS("D:/genesis/input/1d_all/landscapes.rds")

temp_input_df     <- landscapes$temp[,c(1:3)]
colnames(temp_input_df) <- c("x","y","temp")
temp_input_raster <- rasterFromXYZ(temp_input_df, res = c(1,1), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# world ocean atlas ####

# Bio_ORACLE ####
# explore and load sst data
BO_layers <- list_layers(datasets="Bio-ORACLE")
View(BO_layers)

# load in sst layers
BO_sst <- load_layers(layercodes = c("BO_sstmean","BO_sstmax","BO_sstmin","BO_sstrange"))

# set resolution
BO_sst_agg <- aggregate(BO_sst, fact = 1/res(BO_sst)[1])

# mask land from each to make them more comparable
BO_sst_agg[is.na(temp_input_raster)]      <- NA
temp_input_raster[is.na(BO_sst_agg[[1]])] <- NA

# explore differences
diff <- BO_sst_agg[[1]] - temp_input_raster
diff_sq <- sqrt(diff^2)

# convert to df to lot with ggplot
plotDiff <- as.data.frame(diff_sq, xy = TRUE)

ggplot(plotDiff, aes(x=x, y=y)) +
  geom_tile(aes(fill = layer)) +
  scale_fill_viridis(option = "magma",
                     na.value = "lightgrey") +
  coord_fixed() +
  theme_void()

# find average at each latitude
# create a latitudinal curve for the input
mean_input_temp <- filter(temp_input_df, !is.na(temp))
plot_input_lat <- aggregate.data.frame(mean_input_temp[, 3], list(mean_input_temp$y), mean)
plot_input_lat <- cbind(plot_input_lat, "regular_input")
colnames(plot_input_lat) <- c("lat","temp","source")

mean_BO_temp <- as.data.frame(BO_sst_agg, xy = T)[,c(1:3)]
colnames(mean_BO_temp) <- c("x","y","temp")
mean_BO_temp <- filter(mean_BO_temp, !is.na(temp))
plot_BO_Lat <- aggregate.data.frame(mean_BO_temp[, 3], list(mean_BO_temp$y), mean)
plot_BO_Lat <- cbind(plot_BO_Lat, "BO")
colnames(plot_BO_Lat) <- c("lat","temp","source")

plot_lat <- rbind(plot_lat, plot_input_lat)

ggplot(plot_lat, aes(x = lat, y = temp)) +
  geom_line(aes(colour = source)) + 
  theme_classic()

# compare with input data

# MARSPEC ####

# CoralReefWatch v3.1 ####