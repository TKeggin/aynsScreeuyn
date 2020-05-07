
library(tidyverse)
library(readxl)
library(ggrepel)


data <- read_excel("C:/Users/thoma/OneDrive/Documents/PhD/data/sandbox/fossil_calibrations.xlsx")
data$height <- 0

data$fifty <- data$min_age > 60

ggplot(data, aes(x = min_age, y = height)) +
  geom_point(aes(colour = fifty)) +
  #geom_text_repel(aes(label = clade)) +
  scale_x_reverse() +
  theme(#axis.line=element_blank(),
        #axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
       
       
