# set session ####

library("BiocManager")
library("Biostrings")
library("tidyverse")

# craft data ####

ref <- DNAString("AAATTTCCC")

seq <- c("a","b","c","d","e")

a <- DNAString("AAATTTTCCC")
b <- DNAString("AAATTTTCC")
c <- DNAString("AAATTTTC")
d <- DNAString("ATCCC")
e <- DNAString("AAATTTCCC")

# run an alignment query ####

query <- list(a,b,c,d,e)

scores <- c()

for(q in query){
  
  holdMe <- pairwiseAlignment(ref,q)
  scores <- c(scores,holdMe@score)
  
}

results <- data.frame(seq,scores)


top_n(results, 1)
