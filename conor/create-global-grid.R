### script to create a standardised global grid 

# libraries ----
lib_vect <- c('raster', 'rgdal', 'gdalUtils', 'rgeos')
install.lib<-lib_vect[!lib_vect %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(lib_vect,require,character=TRUE)

# empty raster ----

# create empty extent object
global_grid <- raster(extent(c(-180, 180, -90, 90)), 
                      crs = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0', 
                      res = 0.25)

# set values to 1
global_grid[] <- 1

# test plot
plot(global_grid)


# depth ----
depth <- raster('/Volumes/RF-env-data/reef-futures/env-data/GEBCO_2019/GEBCO_2019.nc')
plot(depth)

# tuncate values
depth_clamp <- clamp(depth, lower = -200, upper = 50, useValues = FALSE)
# save the depth clamp
writeRaster(depth_clamp, filename = '/Volumes/RF-env-data/reef-futures/env-data/GEBCO_2019/GEBCO_2019_depth_-200_50.tif')

# apply resampling from gdal as faster than raster
src_dataset <- system.file("/Volumes/RF-env-data/reef-futures/env-data/GEBCO_2019/GEBCO_2019_depth_-200_50.tif", package="gdalUtils")

gdalwarp(srcfile = '/Volumes/RF-env-data/reef-futures/env-data/GEBCO_2019/GEBCO_2019_depth_-200_50.tif',
         dstfile = '/Volumes/RF-env-data/reef-futures/env-data/GEBCO_2019/GEBCO_2019_depth_-200_50_resampled.tif',
         t_srs ='+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0',
         tr    = c(0.25, 0.25), # resampling resolution
         r     = 'near', # resampling method
         output_Raster=TRUE,
         overwrite=TRUE,
         verbose=TRUE)

plot(raster('/Volumes/RF-env-data/reef-futures/env-data/GEBCO_2019/GEBCO_2019_depth_-200_50_resampled.tif'))

# coastlines ----

# read in the coastlines from world hires GSHHS_l_L1_disolve.shp
coastlines <- readOGR(dsn = '/Volumes/RF-env-data/reef-futures/env-data/coastal-shape-files/GSHHS_l_L1_disolve.shp')

coastlines_buffered <- gBuffer(coastlines, width = 1)

# coral ----
coral <- readOGR(dsn = '/Volumes/RF-env-data/reef-futures/env-data/WCMC_ReefData/BufferedPolygons/Reef_AsBufferedPolygon.shp')

# created masked global raster ----

# global coastlines are buffered to 1°
open_ocean   <- mask(global_grid, coastlines, inverse = T)
buffered_ocean <- mask(global_grid, coastlines_buffered, inverse = F)
plot(open_ocean)
plot(buffered_ocean)
plot(open_ocean + buffered_ocean)

# create coast mask through summing
coast_mask <- open_ocean + buffered_ocean

# global reefs are buffered to 0.5°
coral_mask <- mask(global_grid, coral, inverse = F)
coral_mask[][is.na(coral_mask[])] <- 0 # convert to 0 so that sums work
plot(coral_mask)

# turn coast mask with coral to 1s
coast_mask[][coral_mask[] == 1] <- 1
coast_mask[][coast_mask[] > 0] <- 1

# remove land from the oceans
full_coastal_mask <- mask(coast_mask, coastlines, inverse = T)
full_coastal_mask[][is.na(full_coastal_mask[])] <- 0

# create final mask where land=NA, open ocean = 1 and coastal ocean = 2
final_global_mask <- open_ocean + full_coastal_mask
plot(final_global_mask)

# ensure all RLS sites are inside oceanic grid ----

# load in site level data
RLS_Sites <- read.csv('/Users/cwaldock/Dropbox/ETH_REEF_FUTURES/reef-futures-data-processing/processed-data/RLS-spatial-fish-data-extract-siteSummary.csv')

final_global_mask[cellFromXY(final_global_mask, unique(RLS_Sites[,c("SiteLongitude", "SiteLatitude")]))] <- 2

# edit mask errors manually ----

# gibraltar strait 
plot(final_global_mask, xlim = c(-10, -0), ylim = c(30, 40))
final_global_mask[][cellFromXY(final_global_mask, rbind(c(-5.75, 36.25)))] <- 2
plot(final_global_mask, xlim = c(-8, -4), ylim = c(35, 38))

# suez canal
plot(final_global_mask, xlim = c(30, 40), ylim = c(22, 33))
points(c(32.25, 32.25, 32.25, 32.25, 32.25, 32.25), c(29.75, 30, 30.25, 30.5, 30.75, 31))
final_global_mask[][cellFromXY(final_global_mask, rbind(c(32.5, 29.25),
                                                        c(32.25, 29.75), 
                                                        c(32.25, 30), 
                                                        c(32.25, 30.25), 
                                                        c(32.25, 30.5),
                                                        c(32.25, 30.75), 
                                                        c(32.25, 31),
                                                        c(32.25, 31.25)))] <- 2
plot(final_global_mask, xlim = c(30, 35), ylim = c(28, 33))

# double check panama canal is shut
plot(final_global_mask, xlim = c(-90, -70), ylim = c(5, 15))

plot(final_global_mask)

# save the environmental mask layers ---- 
writeRaster(final_global_mask, 'processed-data/environmental-data/environmental_grid/global_mask.nc', overwrite = T)
final_global_mask <- raster('processed-data/environmental-data/environmental_grid/global_mask.nc')

