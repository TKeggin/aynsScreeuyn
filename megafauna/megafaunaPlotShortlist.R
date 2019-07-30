# set session ####

setwd("Z:/LE_projects/megafauna/data/sandbox")

library("readxl")
library("tidyverse")

# load data ####

data <- read_excel("./shortlist.xlsx", sheet = 1)

# plot this puppy ####

data <- data %>% filter(popgen == 1)

ggplot(data, aes(x = reorder(data$gen_sp, -data$rad))) +
  geom_col(aes(y=n)) +
  geom_col(aes(y=extract), colour="red") +
  geom_col(aes(y=rad), fill="red") +
  geom_text
  ylim(c(0,160)) +
  ylab(element_blank()) +
  xlab(element_blank()) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.2),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour="grey", linetype = "solid"),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank())

