# set session ####
library("rgdal")
library("raster")
library("sf")
library("tidyverse")
library("gridExtra")
library("fasterize")

setwd("C:/Users/thoma/OneDrive/Documents/PhD/data/")

# load data ----
# contemporary reef distribution
dist <- st_read("./coral_dist/14_001_WCMC008_CoralReefs2018_v4/01_Data/WCMC008_CoralReef2018_Py_v4.shp")
dist <- st_read("./coral_dist/BufferedPolygons/BufferedPolygons/Reef_AsBufferedPolygon.shp")
# genesis input
land <- readRDS("D:/genesis/input/1d_2000m_20c/landscapes.rds")
land <- land$depth[,c(1,2,3)]
colnames(land)[3] <- "depth"
land.raster <- rasterFromXYZ(land ,crs = st_crs(dist))

st_geometry_type(dist)  # check data type
st_crs(dist)            # check crs
st_bbox(dist)           # check extent

# output a multi-plot ----
mapWorld <- borders("world", colour="lightgrey", fill="lightgrey")

plots <- list()
d_cuts <- seq(from = -3000, to =-300, by = 300)

for(d_cut in seq(1:length(d_cuts))){

land_plot <- filter(land, depth >= d_cuts[d_cut])

x <- ggplot() +
  mapWorld +
  geom_tile(data = land_plot, aes(x=x, y=y, fill =depth)) +
  geom_sf(data = dist, aes(alpha = 0.5), lwd = 0) +
  ggtitle(d_cuts[d_cut]) +
  theme_void() +
  theme(legend.position="none")
plots[[d_cut]] <- x
}

jpeg(file = "C:/Users/thoma/Desktop/depth_compare.jpeg", width = 3000, height = 6000)
do.call("grid.arrange", c(plots, ncol=2))
dev.off()

# find depths of coral distribution overlay ----
# convert sf dist object to raster
template <- raster(crs = st_crs(dist), ext = extent(land.raster))
reef_mask <- fasterize(dist, template)

# filter depth raster by reef presence
reef_depth <- mask(land.raster, reef_mask)

reef_depth.df <- as.data.frame(reef_depth, xy = T)
reef_depth.df <- filter(reef_depth.df, !is.na(depth))

plot(reef_depth)

# histogram of cell depth distribution
ggplot(reef_depth.df)+
    geom_histogram(aes(x=depth), color="black", fill="lightblue", bins = 150) +
  geom_vline(xintercept=-200, colour = "red") +
  ylab("# cells") +
  theme_classic()

# explore different depth cuts and habitat coverage
d_cut <- -2000

reef_cut <- reef_depth
values(reef_cut)[values(reef_cut) > d_cut] = NA

plot(reef_depth)
plot(reef_cut, col = "red", add = T, legend = F)
title(d_cut)

# plot full depth habitat with set cut off
land_plot <- filter(land, depth >= d_cut)
ggplot() +
 # mapWorld +
  geom_tile(data = land_plot, aes(x=x, y=y, fill=depth)) +
  geom_sf(data = dist, aes(alpha=0.5), color = "darkred", lwd = 0) +
  ggtitle(d_cut) +
  theme_void()# +
  #theme(legend.position="none")











