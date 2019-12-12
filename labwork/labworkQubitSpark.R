# set session ####

setwd("Z:/LE_projects/megafauna/labwork/extractions/Extraction Plates/Lengguru")

library(tidyverse)
library(reshape2)
library(readxl)

# load data ####

key    <- as.matrix(read_excel("./2. setup.xlsx", sheet = 4, col_names = FALSE))
values <- as.matrix(read_excel("./2. setup.xlsx", sheet = 5, col_names = FALSE))

dimnames(key)    <- list(LETTERS[1:16], 1:24)
dimnames(values) <- list(LETTERS[1:16], 1:24)

key       <- melt(key, varnames = c("row","column"))
key$value <- as.character(key$value)
values    <- melt(values, varnames = c("row","column"))

data <- cbind(key, values$value)
names(data) <- c("row","column","ID","value")
data$ID <- gsub(" ", "", data$ID)

# create standard curve to convert to ngul ####

ngul <- c(0,100)
stds <- aggregate(data$value, by=list(data$ID), FUN = mean) %>% 
  filter(Group.1 %in% c("Blank","S1"))

#ngul <- c(0,100,50,25,10,1,0.3,0.1)
#stds <- aggregate(data$value, by=list(data$ID), FUN = mean) %>% 
#  filter(Group.1 %in% c("Blank","S1","S2","S3","S4","S5","S6","S7"))

stds <- data.frame(stds, ngul)

ggplot(stds, aes(x=x, y=ngul)) +
  geom_point()

fit <- lm(ngul ~ x, data = stds)

eq <- function(x){
  ngul <- fit$coefficients[1] + fit$coefficients[2]*x
  print(ngul)
}


# take mean of duplicates, correct for blanks, and convert to ngul using lm ####

mean      <- aggregate(data$value, by=list(data$ID), FUN=mean)
base      <- filter(mean, Group.1 == "Blank")$x[1]
mean$x    <- mean$x-base
mean$ngul <- eq(mean$x)
mean$one  <- mean$ngul>4

sd   <- aggregate(data$value, by=list(data$ID), FUN=sd)
min  <- aggregate(data$value, by=list(data$ID), FUN=min)
max  <- aggregate(data$value, by=list(data$ID), FUN=max)

summary <- data.frame(mean, sd$x, min$x, max$x)

# plot ####

plotData <- filter(summary, !(Group.1 %in% stds$Group.1))

ggplot(plotData, aes(x=x, y=ngul)) +
  geom_point(aes(colour = one))

ggplot(summary, aes(x=reorder(Group.1, -ngul))) +
  geom_col(aes(y=ngul, fill = one))

ggplot(summary, aes(x=reorder(Group.1, -x))) +
  geom_col(aes(y=x, fill = one)) +
  #geom_point(aes(y=min.x)) +
  #geom_point(aes(y=max.x)) +
  geom_errorbar(aes(ymin=x-(sd.x/2), ymax=x+(sd.x/2)), width=.5, colour = "red") +
  theme(axis.text.x = element_text(angle=90))

# look at high devation subset (probable pipetting errors) ####

subset <- filter(data, ID %in% c(1775,734,1692,62))

ggplot(subset) +
  geom_point(aes( x = ID, y = value), pch = 4)



