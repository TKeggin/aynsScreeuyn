# set session ####
library("rgdal")
library("raster")
library("sf")
library("tidyverse")
library("gridExtra")

setwd("C:/Users/thoma/OneDrive/Documents/PhD/data/")

# load data ####
# contemporary reef distribution
dist <- st_read("./coral_dist/14_001_WCMC008_CoralReefs2018_v4/01_Data/WCMC008_CoralReef2018_Py_v4.shp")
dist <- st_read("./coral_dist/BufferedPolygons/BufferedPolygons/Reef_AsBufferedPolygon.shp")
# genesis input
land <- readRDS("./gasm/landscapes.rds")
land <- land$depth[,1:3]
colnames(land)[3] <- "depth"
land.raster <- rasterFromXYZ(land ,crs = st_crs(dist))

st_geometry_type(dist)  # check data type
st_crs(dist)            # check crs
st_bbox(dist)           # check extent

mapWorld <- borders("world", colour="lightgrey", fill="lightgrey")

# output a multi-plot
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

#
