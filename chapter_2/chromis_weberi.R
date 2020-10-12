# set session #

library(adegenet)
library(vcfR)

# load data ####
# vcf
data.vcfR <- read.vcfR("R:/data/reefish_WIO_SNPs/Chr_w/Chr_w_TotalSNPs_Q20_mac3_maf0.05_minDP3_meanDP10_NA10.5_reduced_NA20.2_QUAL3_maxDP50_SNP2_NA30.95_HARDY.haplo.vcf.gz")
# popmap
popmap <- read_delim("R:/data/reefish_WIO_SNPs/Chr_w/pop_map_Chr_w.txt", delim = " ")
popmap <- popmap[-14,]

# convert to genind
data.genind <- vcfR2genind(data.vcfR)

# PCA ####
# Replace missing with mean
data_na <- scaleGen(data.genind, NA.method="mean")

# perform PCA
pca1 <- dudi.pca(data_na,cent=FALSE,scale=FALSE,scannf=FALSE,nf=3)

barplot(pca1$eig[1:50],main="PCA eigenvalues", col=heat.colors(50))

# plot data ####

plotData <- pca1$li
plotData$Sample_ID <- row.names(plotData)

plotData <- merge(plotData,popmap)

aggData <- aggregate(cbind(Axis1,Axis2,Axis3) ~ population, data = plotData, mean)

ggplot() +
  geom_point(data = plotData, aes(x = Axis1, y = Axis2, colour = pop)) +
  geom_text(data = aggData, aes(x = Axis1 ,y = Axis2, label = pop)) +
  theme_void() +
  theme(legend.position = "none")

