# set session ####

library("adegenet")
library("tidyverse")
#library("vcfR")

setwd("C:/Users/keggint/polybox/shark/4_output/Cc/7.5")

# load data ####

data <- read.structure("./populations.stru")

# DAPC ####

# Replace missing with mean
data_na <- scaleGen(data, NA.method="mean")

# find optimum PCs with a-score
dapcA <- dapc(data_na, grp=data$pop, n.da=100, n.pca=150)
temp <- optim.a.score(dapcA)

# find optimum PCs with cross-validation
grp <- pop(data)
xval <- xvalDapc(data_na, grp=data$pop, n.pca.max = 100, training.set = 0.9,
                 result = "groupMean", center = TRUE, scale = FALSE,
                 n.pca = NULL, n.rep = 30, xval.plot = TRUE)

# run dapc
dapc <- dapc(data_na, grp=data$pop)

# plot ####

scatter(dapc)

myPal <- colorRampPalette(c("red","black"))

scatter(dapc, 
        xax=1, 
        #yax=2,
        col=transp(myPal(5)), 
        scree.da=TRUE, 
        pch=20,
        cell=1.25, 
        cex=2,
        bg="white",
        clab=0,
        cstar=0)

assignplot(dapc)

data <- as.data.frame(dapc$ind.coord)
data$population <- dapc$grp

aggData <- aggregate(cbind(LD1,LD2) ~ population, data = data, mean)

ggplot() +
  geom_point(data = data, aes(x = LD1, y = LD2, colour = population)) +
  geom_text(data = aggData, aes(x = LD1 ,y = LD2, label = population)) +
  theme_void()
 # theme(legend.position = "none")

plot3d(data$LD1,data$LD2,data$LD3)
