setwd("M:/admin/NW Australia/Sampling")

library(readxl)
library(tidyverse)


data <- read_excel("./Coordinates_Sampling_WA.xlsx", sheet = 1)

lat <- data$lat_dd[1:6]
long <- data$long_dd[1:6]

data <- as.data.frame(cbind(lat,long))


mapWorld <- borders("world", regions = "australia", colour="lightgrey", fill="lightgrey")

ggplot(data) +
  mapWorld +
  geom_point(aes(x = long, y = lat)) +
  #geom_text_repel(data = agg.data, box.padding = 0.7, aes(x = long, y = lat), size=4) +
  #geom_text_repel(data = data, aes(x = longitude, y = latitude, label=Sample_ID), size=1) +
  xlim(110,170) +
  ylim(-50,0) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")
