# set session ####

setwd("C:/Users/keggint/polybox/sandbox/L_piscatorius/outlier detection/")

library("tidyverse")
library("readxl")
library("data.table")

# load and wrangle data ####

# this uses the Arlequin fdist_ObsOut.txt as input
data <- read_excel("./fdist2_ObsOut.xlsx", sheet = 1)

# this uses quantile data found in the populations.xml file
data_quantiles <-  read_excel("./fdist2_ObsOut.xlsx", sheet = 2)

# replace arlequin marker names with stacks ones using structure file
structure <- fread("../populations.str")
loci <- colnames(structure)[-(1:2)]

data$Locus <- loci





# filter data ####

cutoff <- 0.001

outlierCc <- filter(Cc, FST.P.value <= cutoff, Obs.FST > 0)
neutralCc <- filter(Cc, FST.P.value > cutoff)
balancingCc <- filter(Cc, FST.P.value <= cutoff, Obs.FST < 0)

# plot data ####

size <- 1

ggplot(Cc, aes(x = Obs.Het.BP, y = Obs.FST)) +
  geom_point(data = neutralCc, size = size) +
  geom_point(data = outlierCc, color = "red", size = size) +
  geom_point(data = balancingCc, color = "grey", size = size) +
  geom_text(data = outlierCc, size = 1, aes(label = Name)) +
  geom_line(data = Cc_quantiles, aes(x = Het.BP., y = q0.99), color = "red") +
  geom_line(data = Cc_quantiles, aes(x = Het.BP., y = q0.95)) +
  geom_line(data = Cc_quantiles, aes(x = Het.BP., y = q0.05)) +
  geom_line(data = Cc_quantiles, aes(x = Het.BP., y = q0.01), color = "red") +
  ggtitle("Centroselachus crepidater") +
  xlab("Heterozygosity") +
  ylab("FST") +
  guides(col = FALSE) +
  theme_classic() +
  theme(plot.title = element_text(face="bold.italic", hjust = 0.5))


# write loci lists to file ####
oCc <- data.frame(gsub("_.*", "", outlierCc$Name))
nCc <- data.frame(gsub("_.*", "", neutralCc$Name))


write_csv(oCc, "./Cc selection.csv", col_names = FALSE)

write_csv(oDc, "./Dc selection.csv", col_names = FALSE)

# write list for all loci ####
allCc <- data.frame(gsub("_.*", "", Cc$Name))

write_csv(allCc, "./Cc.csv", col_names = FALSE)
