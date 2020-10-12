# set session #
home <- "R:/data/reefish_WIO_SNPs/"
setwd(home)

library(adegenet)
library(vcfR)
library(tidyverse)
library(pegas)

# pca loop ####
species    <- list.files()[-1]
search.vcf <- "HARDY.haplo.vcf"
search.pop <- "pop_map"

for(i in 18:length(species)){
  setwd(species[i])
  print(paste0("Loading in ",species[i]))
  # load SNPs
  file.vcf  <- grep(search.vcf, list.files())
  data.vcfR <- read.vcfR(list.files()[file.vcf])
  # load popmap
  file.pop         <- grep(search.pop, list.files())
  popmap           <- read_delim(list.files()[file.pop], delim = " ")
  colnames(popmap) <- c("Sample_ID","pop")
  
  # convert to genind
  data.genind     <- vcfR2genind(data.vcfR)
  data.genind@pop <- as.factor(popmap$pop)
  
  # convert to loci
  data.loci <- as.loci(data.genind)
  
  # calculate pairwise fst
  Fst(data.loci)
  
  setwd(home)
}




