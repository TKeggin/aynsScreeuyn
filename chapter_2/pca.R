# set session #
home <- "R:/data/reefish_WIO_SNPs/"
setwd(home)

library(adegenet)
library(vcfR)
library(tidyverse)

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
  data.genind <- vcfR2genind(data.vcfR)
  
  # Replace missing with mean
  data_na <- scaleGen(data.genind, NA.method="mean")
  
  # perform PCA
  pca1 <- dudi.pca(data_na,cent=FALSE,scale=FALSE,scannf=FALSE,nf=3)
  
  # plot data
  data.plot           <- pca1$li
  data.plot$Sample_ID <- row.names(data.plot)
  data.plot           <- merge(data.plot,popmap)
  
  plot <- ggplot() +
    geom_point(data = data.plot, aes(x = Axis1, y = Axis2, colour = pop)) +
    ggtitle(species[i])
  
  jpeg(file.path(paste0("C:/Users/thoma/OneDrive/Documents/PhD/chapter_2/pca/", species[i],".jpeg")), width = 540, height = 480)
  print(plot)
  dev.off()
  
  setwd(home)
}




