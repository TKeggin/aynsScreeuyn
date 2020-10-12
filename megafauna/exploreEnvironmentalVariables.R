#
# Explore environmental variables associated with a sampling scheme
# look at overall environmental conditions
# compare to species distributions/sampling scheme
# Thomas Keggin (thomas.keggin@usys.ethz.ch)
#
# set session ####

setwd("C:/Users/thoma/OneDrive/Documents/PhD/data/bio-oracle/All.Present.Surface.BOv2_1.tif")
variables <- list.files()

library(tidyverse)
library(readxl)
library(raster)
library(rnaturalearth)
library(ggrepel)
library(robis)

# load in sample metadata ####

samples <- read_excel("M:/data/temp_megafauna_exports/Browse samples.xlsx") # import data

#chromis <- filter(samples, str_detect(species,"Chromis")) # filter for chromis
chromis <- filter(samples, species %in% c("Chromis_weberi","Chromis_multilineata")) %>%  # filter for sample species
  filter(sample_site != "Lengguru") # remove lengguru

# salinity ####
# load data
variables_salinity <- variables[grep("Salinity",variables)]

salinity_mean  <- raster(variables_salinity[4]) # mean
salinity_range <- raster(variables_salinity[6]) # range
salinity_min <- raster(variables_salinity[5]) # min
salinity_max <- raster(variables_salinity[3]) # max

# extract values
chromis$salinity_mean  <- extract(salinity_mean, chromis[,5:4])
chromis$salinity_range <- extract(salinity_range, chromis[,5:4])
chromis$salinity_min  <- extract(salinity_min, chromis[,5:4])
chromis$salinity_max <- extract(salinity_max, chromis[,5:4])

# temperature ####
# load data
variables_temperature <- variables[grep("Temperature",variables)]

temperature_mean  <- raster(variables_temperature[4]) # mean
temperature_range <- raster(variables_temperature[6]) # range
temperature_min <- raster(variables_temperature[5]) # min
temperature_max <- raster(variables_temperature[3]) # max

# extract values
chromis$temperature_mean  <- extract(temperature_mean, chromis[,5:4])
chromis$temperature_range <- extract(temperature_range, chromis[,5:4])
chromis$temperature_min   <- extract(temperature_min, chromis[,5:4])
chromis$temperature_max   <- extract(temperature_max, chromis[,5:4])

# oxygen ####
# load data
variables_oxygen <- variables[grep("oxygen",variables)]

oxygen_mean  <- raster(variables_oxygen[4]) # mean
oxygen_range <- raster(variables_oxygen[6]) # range
oxygen_min <- raster(variables_oxygen[5]) # min
oxygen_max <- raster(variables_oxygen[3]) # max

# extract values
chromis$oxygen_mean  <- extract(oxygen_mean, chromis[,5:4])
chromis$oxygen_range <- extract(oxygen_range, chromis[,5:4])
chromis$oxygen_min  <- extract(oxygen_min, chromis[,5:4])
chromis$oxygen_max <- extract(oxygen_max, chromis[,5:4])


salinity_range <- aggregate(salinity_range,3) # aggregate to a 1x1 resolution
salinity_range <- as.data.frame(salinity_range, xy = TRUE) # convert to dataframe
colnames(salinity_range) <- c("x","y","salinity")
salinity_range <- filter(salinity_range, salinity>30)

# summarise data ####

chr_sum <- chromis %>%
  group_by(species,sample_site) %>% 
  summarise_at(vars("longitude","latitude",
                    "temperature_mean", "temperature_min", "temperature_max", "temperature_range",
                    "salinity_mean", "salinity_min", "salinity_max", "salinity_range",
                    "oxygen_mean", "oxygen_min", "oxygen_max", "oxygen_range",
                    ), mean)

write_csv(chr_sum, "C:/Users/thoma/OneDrive/Documents/PhD/screeuyn/chapter 3/variable_summary.csv")

# sampled proportion of total range size ####
# weberi
# load spp distribution data
weberi_dist <- occurrence("Chromis weberi")
weberi_dist <- weberi_dist[,c("decimalLongitude","decimalLatitude")]
# extract environmental variables
weberi_dist$temp_mean <- extract(temperature_mean, weberi_dist)
weberi_dist <- filter(weberi_dist, !is.na(temp_mean)) # remove na values
web_sp_range <- range(weberi_dist$temp_mean) # find range of mean values
weberi <- filter(chromis, species == "Chromis_weberi") # filter for weberi only
(weberi$temperature_mean-web_sp_range[1])/(web_sp_range[2]-web_sp_range[1]) # convert to proportion of species total range values

ggplot() +
  geom_point(data = weberi_dist, aes(x=decimalLongitude, y=decimalLatitude, colour=temp_mean)) +
  geom_sf(data = world, colour = "darkgrey") +
  scale_colour_viridis_c() +
  geom_label_repel(data = filter(chr_sum, species == "Chromis_weberi"),
                   box.padding = 0.7,
                   aes(x = longitude,
                       y = latitude,
                       label= paste(round(temperature_range,1),"C")),
                   size=4)

# multilineata
# load spp distribution data
multilineata_dist <- occurrence("Chromis multilineata")
multilineata_dist <- multilineata_dist[,c("decimalLongitude","decimalLatitude")]
# extract environmental variables
multilineata_dist$temp_mean <- extract(temperature_mean, multilineata_dist)
multilineata_dist <- filter(multilineata_dist, !is.na(temp_mean)) # remove na values
multilineata_dist <- filter(multilineata_dist, temp_mean > 22) # remove outliers

ggplot(data = multilineata_dist, aes(x=temp_mean, y = decimalLatitude)) +
  geom_point()

web_sp_range <- range(multilineata_dist$temp_mean) # find range of mean values
multilineata <- filter(chromis, species == "Chromis_multilineata")# filter for multilineata only
(multilineata$temperature_mean-web_sp_range[1])/(web_sp_range[2]-web_sp_range[1]) # convert to proportion of species total range values


ggplot() +
  geom_point(data = multilineata_dist, aes(x=decimalLongitude, y=decimalLatitude, colour=temp_mean)) +
  geom_sf(data = world, colour = "darkgrey") +
  scale_colour_viridis_c() +
  geom_label_repel(data = filter(chr_sum, species == "Chromis_multilineata"),
                   box.padding = 0.7,
                   aes(x = longitude,
                       y = latitude,
                       label= paste(round(temperature_range,1),"C")),
                   size=4)
  

# plot data ####
world <- ne_coastline(scale = "medium", returnclass = "sf")

# temperature
temp <- as.data.frame(aggregate(temperature_mean,fact = 3), xy = TRUE)

ggplot() +
  geom_tile(data = temp, aes(x=x,y=y, fill=Present.Surface.Temperature.Mean)) +
  coord_fixed() +
  scale_fill_viridis_c() +
  geom_label_repel(data = chr_sum,
                   box.padding = 0.7,
                   aes(x = longitude,
                       y = latitude,
                       label= paste(round(temperature_mean,1),"C")),
                   size=4) +
  geom_point(data=multilineata_dist, aes(x = decimalLongitude, y = decimalLatitude)) +
  theme_void()

ggplot(data = world) +
  geom_sf(colour = "darkgrey") +
  geom_point(data = chr_sum,
             aes(x = longitude,
                 y = latitude,
                 colour = sample_site),
             size = 3) +
  geom_label_repel(data = chr_sum,
                  box.padding = 0.7,
                  aes(x = longitude,
                      y = latitude,
                      label= paste(round(temperature_range,1))),
                  size=4) +
  theme_void()

# plot ranges
# range
ggplot(chr_sum, aes(x=temperature_range, y=salinity_range)) +
  geom_label(aes(label=sample_site, fill=species)) +
  ggtitle("range")
# mean
ggplot(chr_sum, aes(x=temperature_mean, y=salinity_mean)) +
  geom_label(aes(label=sample_site, fill=species)) +
  ggtitle("mean")
# min
ggplot(chr_sum, aes(x=temperature_min, y=salinity_min)) +
  geom_label(aes(label=sample_site, fill=species)) +
  ggtitle("min")































