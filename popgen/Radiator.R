setwd("C:/Users/keggint/polybox/Zurich/data/reefish/Amph_akal_vcf")

library("adegenet")
library("vcfR")
library("radiator")


data <- radiator::filter_rad(
  data = "test.vcf"
  #strata = "strata.tsv",
  #output = "structure"
)
