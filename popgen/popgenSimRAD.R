# set session ####

library(SimRAD)

simseq <- sim.DNAseq(size=1000000, GCfreq=0.433)

#TaqI
cs_5p1 <- "T"
cs_3p1 <- "CGA"

#MseI
cs_5p2 <- "T"
cs_3p2 <- "TAA"

simseq.dig <- insilico.digest(simseq, cs_5p1, cs_3p1, cs_5p2, cs_3p2, verbose = TRUE)
