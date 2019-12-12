# set the session ####

library(pcadapt)
library(vcfR)

setwd("C:/Users/keggint/polybox/shark/4_output/Cc/7.3/test/")

data <- read.pcadapt("./test.recode.vcf", type = "vcf")

x <- pcadapt(input = data, K = 20) 

popmap <- scan("./Cc_popmap.txt", character())

plot(x, option = "screeplot")

plot(x, option = "scores", pop = popmap)


