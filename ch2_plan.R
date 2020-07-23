library(readxl)
library(tidyverse)

setwd("C:/Users/keggint/Desktop")

# relative contribution to diversity ####

rel.div <- read_excel("./data.xlsx", sheet = 1)
rel.div$level <- factor(rel.div$level, levels = c("genome",
                                            "individual",
                                            "population",
                                            "species",
                                            "community"))
rel.div <- filter(rel.div, region == "WIO")
  

ggplot(data = rel.div, aes(x = level, y = diversity)) +
  geom_col(aes(fill = environment), width = 0.5, colour = "black") +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("made up relative contribution to diversity") +
  theme_classic()

# dispersal v. diversity ####

dis.div <- read_excel("./data.xlsx", sheet = 3)
dis.div$`dispersal/diversity_R2` <- runif(length(dis.div$`dispersal/diversity_R2`), min = -1, max = 1)

wio <- filter(dis.div, region == "WIO")
car <- filter(dis.div, region == "caribbean")

ggplot() +
  geom_path(data = wio, aes(x = spatial, y = `dispersal/diversity_R2`, group = level, colour = level), size = 1, linetype = "dashed") +
  geom_path(data = car, aes(x = spatial, y = `dispersal/diversity_R2`, group = level, colour = level), size = 1, linetype = "solid") +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
  ylim(c(-1,1)) +
  ylab("dispersal/diversity") +
  xlab("spatial scale") +
  theme_classic()
