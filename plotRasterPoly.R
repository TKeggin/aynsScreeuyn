
library(rgdal)
library(maptools)
library(raster)
library(sf)
library(tidyverse)

# whole dataset
#dist <- readOGR("C:/Users/keggint/polybox/Zurich/data/megafauna/distribution/14_001_WCMC008_CoralReefs2018_v4/01_Data/WCMC008_CoralReef2018_Py_v4.shp")
# Conor's buffered data
dist <- readOGR("C:/Users/keggint/polybox/Zurich/data/megafauna/distribution/BufferedPolygons/BufferedPolygons/Reef_AsBufferedPolygon.shp")
# convert dist to sf object
dist.sf <- st_as_sf(dist)
# buffer the polygons
data <- st_buffer(dist.sf, dist=1)

# present day elevation raster
load("C:/Users/keggint/Documents/Marrey/utilities/input_compilation/present.Rdata")
crs(present) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# convert present to dataframe
# change land to NA
present.df <- geoDepthList[[1]]
present.df <- as.data.frame(present, xy =TRUE)
present.df[present.df$Ceno0001 > 0, "Ceno0001"] <- NA

ggplot() +
  geom_tile(data = present.df, aes(x=x, y=y, fill=Ceno0001)) +
  scale_fill_gradient2(low = "lightgrey", mid = "white",
                       high = "yellow", midpoint = 0) +
  geom_sf(data = dist.sf, aes(colour = "red", alpha = 0.2), fill = "red", alpha = 0.2) +
  coord_sf() +
  theme_void()










