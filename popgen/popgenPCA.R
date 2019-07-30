# set session ####

setwd("C:/Users/keggint/polybox/L_piscatorius")

library("adegenet")
library("tidyverse")

# load and clean data ####

# read structure
data <- read.structure("./1_4.str")

# Replace missing with mean
data_na <- scaleGen(data, NA.method="mean")

# analysis ####

# perform PCA
pca1 <- dudi.pca(data_na,cent=FALSE,scale=FALSE,scannf=FALSE,nf=3)

barplot(pca1$eig[1:50],main="PCA eigenvalues", col=heat.colors(50))

# plot data ####

plotData <- pca1$li

plotData$population <- data$pop

aggData <- aggregate(cbind(Axis1,Axis2,Axis3) ~ population, data = plotData, mean)

ggplot() +
  geom_point(data = plotData, aes(x = Axis1, y = Axis2, colour = population)) +
  geom_text(data = aggData, aes(x = Axis1 ,y = Axis2, label = population)) +
  theme_void() +
  theme(legend.position = "none")
