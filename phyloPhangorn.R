# set session ####

setwd("C:/Users/keggint/polybox/Eilish/Phylogenies")

library("phangorn")
library("ape")

# load data ####

sp.fasta <- read.dna("./Abalistes stellaris.fasta", format = "fasta")

data <- as.phyDat(sp.fasta)

dm <- dist.ml(data)

treeUPGMA <- upgma(dm)

treeNJ <- NJ(dm)


layout(matrix(c(1,2), 2, 1), height=c(1,2))
par(mar = c(0,0,2,0)+ 0.1)
plot(treeUPGMA, main="UPGMA")
plot(treeNJ, "unrooted", main ="NJ")










# tutorial ####

fdir <- system.file("extdata/trees", package = "phangorn")

primates <- read.phyDat(file.path(fdir, "primates.dna"), format = "interleaved")

dm <- dist.ml(primates)

treeUPGMA <- upgma(dm)

treeNJ <- NJ(dm)


layout