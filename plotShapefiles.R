# set session ####

setwd("R:/data/raw_reefish_seascape/coral_reefs_2010/")

library("sf")
library("tidyverse")

# load data ####

data <- st_read("WCMC-008-CoralReefs2010-ver1-3.shp")
data_sub <- sample_frac(data, size = 0.1)

world <- ne_coastline(scale = "small", returnclass = "sf")

# pot ####

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = data_sub, colour = "red", fill ="red") +
  theme_minimal() +
  theme(panel.grid = element_line(colour = "transparent"))
