# set session ####
main.dir <- "Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/2_tolerance/1/"
setwd(main.dir)


library(ape)


data <- read.nexus("./phy.nex")

plot(data)


species <- readRDS("./species/")

summary <- readRDS("./sgen3sis.rds")
