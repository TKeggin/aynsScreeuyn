
library("rgdal")
library("raster")
library("sf")
library("tidyverse")

setwd("Z:/LE_projects/megafauna/data/distribution/14_001_WCMC008_CoralReefs2018_v4/01_Data")

data <- st_read("./WCMC008_CoralReef2018_Py_v4.shp")

st_geometry_type(data)  # check data type
st_crs(data)            # check crs
st_bbox(data)           # check extent

mapWorld <- borders("world", colour="lightgrey", fill="lightgrey")

ggplot() +
  geom_sf(data = data) 
  #xlim(-120,-80) +
  #ylim(13,40)
