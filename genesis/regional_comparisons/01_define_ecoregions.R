# This script takes the ecoregions as defined by Spalding et al. 2007 and
# uses them to assign ecoregions to each cell in the genesis simulation
# outputs.
# Thomas Keggin
# thomaskeggin@hotmail.com


# set session ####

library(tidyverse)
library(raster)
library(sf)

# load and wrangle data ####

# ecoregions from spalding et al 2007
data <- st_read("C:/Users/thoma/OneDrive/Documents/PhD/data/ecoregions/Marine_Ecoregions_Of_the_World_(MEOW)-shp/Marine_Ecoregions_Of_the_World__MEOW_.shp")

# convert crs 
data <- st_transform(data, crs = "+proj=longlat +datum=WGS84")

# aggregate to realms
data_realms <- aggregate(data, by = list(data$REALM), mean)
data_realms <- data[,c("REALM","geometry")]

# filter out non-tropical provinces
# data_tropical <- data %>% filter(Lat_Zone == "Tropical")
tropical_realms <- c("Tropical Atlantic",
                     "Western Indo-Pacific",
                     "Central Indo-Pacific",
                     "Tropical Eastern Pacific",
                     "Eastern Indo-Pacific")

data_tropical <- data %>% filter(REALM %in% tropical_realms)

# convert to raster and match genesis output format
blank_raster         <- raster()
data_tropical_raster <- rasterize(data_tropical,blank_raster)
data_raster          <- rasterize(data,blank_raster)

# get simulation coordinate data
landscape        <- readRDS("D:/genesis/output/5.4_all/120/landscapes/landscape_t_0.rds")
coords           <- data.frame(landscape$coordinates,rownames(landscape$coordinates))
colnames(coords) <- c("x","y","cell")
coords_raster    <- rasterFromXYZ(coords)

# crop ecoregion raster to match extent
data_raster      <- crop(data_raster, coords_raster)

# mask ecoregion raster to give ecoregions of simulation cells
sim_ecoregions   <- mask(data_raster,coords_raster)

# convert sim_ecoregions and coords into a data frame
sim_ecoregions <- as.data.frame(sim_ecoregions, xy = T)
coords_new     <- as.data.frame(coords_raster, xy = T)

# add cell values to sim_ecoregions and remove NAs
sim_ecoregions$cell <- coords_new$cell
sim_ecoregions      <- sim_ecoregions %>% filter(!is.na(cell))

# output

write_csv(sim_ecoregions, "C:/Users/thoma/OneDrive/Documents/PhD/chapter_1/analysis/regional_continuity/01_sim_ecoregions.csv")





