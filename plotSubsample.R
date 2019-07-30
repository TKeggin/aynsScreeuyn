# set session ####

library("readxl")
library("ggmap")
library("maptools")
library("maps")
library("ggrepel")

setwd("C:/Users/keggint/polybox/sandbox/L_piscatorius")

# load and wrangle the data ####

data <- read_excel("90_coord.xlsx", sheet = 1)

# plot ####

mapWorld <- borders("world", colour="lightgrey", fill="lightgrey")

ggplot(data) +
  mapWorld +
  geom_point(aes(x = longitude, y = latitude)) +
  #geom_text_repel(data = agg.data, box.padding = 0.7, aes(x = longitude, y = latitude, label=sample_site), size=4) +
  geom_text_repel(data = data, aes(x = longitude, y = latitude, label=individual)) +
  xlim(-15,5) +
  ylim(40,70) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")
