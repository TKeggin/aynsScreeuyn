library(tidyverse)
library(reshape2)
library(magick)

setwd ("C:/Users/Thomas/OneDrive/Documents/Zurich/gasm/GaSM/Input/WorldMap200-0Ma_multiple_2d/all_geo_hab")

load("all_geo_hab.RData")

temp <- all_geo_hab$temp
arid <- all_geo_hab$aridity

Temperature <- temp[3]
Aridity <- arid[1201]

# plot temperature
ggplot(temp, aes(x = x, y = y, fill = Temperature)) +
  geom_tile() +
  scale_fill_continuous(low = "black",
                        high = "lightgrey",
                        na.value = "white",
                        limits = c(-40,60)) +
  xlim(-180,180) +
  ylim(-90,90) +
  coord_fixed() +
  theme_void()


# plot aridity
ggplot(arid, aes(x = x, y = y, fill = Aridity)) +
  geom_tile() +
  scale_fill_continuous(low = "lightgrey",
                        high = "black",
                        na.value = "white",
                        limits = c(0,1)) +
  xlim(-180,180) +
  ylim(-90,90) +
  coord_fixed() +
  theme_void()
