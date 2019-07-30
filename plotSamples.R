# set session ####

library("readxl")
library("ggmap")
library("maptools")
library("maps")
library("ggrepel")

setwd("Z:/LE_projects/megafauna/data/sandbox")

# load and wrangle the data ####

data <- read_excel("Browse samples.xlsx", sheet = 1)

agg.data <- aggregate(cbind(Longitude,Latitude) ~ Sample_site, data = data, mean)

# plot all samples ####

mapWorld <- borders("world", colour="lightgrey", fill="lightgrey")

ggplot(data) +
  mapWorld +
  geom_point(aes(x = Longitude, y = Latitude, colour = Sample_site)) +
  geom_text_repel(data = agg.data, box.padding = 0.7, aes(x = Longitude, y = Latitude, label=Sample_site), size=4) +
  #geom_text_repel(data = data, aes(x = longitude, y = latitude, label=Sample_ID), size=1) +
  #xlim(72,74) +
  #ylim(-90,90) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")

