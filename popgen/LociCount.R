
setwd("C:/Users/keggint/polybox/L_piscatorius")

library("tidyverse")
library("data.table")

#subData <- fread("./batch_3_subset.geneR")
allData <- fread("./1mil_all.str")

loci <- rowSums(allData!=999)
missing <- rowSums(allData==0)
perMissing <- (100/loci)*missing
perLoci <- 100-perMissing
kept <- (perMissing<=90)

summary <- data.frame(allData$V1,allData$V2,perLoci,kept)
summary <- distinct(summary)

colnames(summary) <- c("individual","pop","percentage","kept")
#View(summary)

ggplot(summary, aes(x = reorder(summary$individual, -summary$percentage), y = percentage, fill = kept)) +
  geom_col() +
  xlab("") +
  ylab("Loci Present (%)") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0))
  
  