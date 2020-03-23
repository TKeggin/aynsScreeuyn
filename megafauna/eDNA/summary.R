# set session ####

setwd("Z:/LE_projects/megafauna/data/processed_megafauna_edna/virginie_unsorted/P9_global_AI/P9_global_AI/Rdata")

library(tidyverse)
library(reshape)

# load data ####

load("01_liste_all_read_edna.Rdata")

# count the number of species in each family for each site ####

sites <- seq(1,length(liste_read_edna))
site_names <- names(liste_read_edna)

families <- list()

for(site in sites){
  
  data <- liste_read_edna[[site]]
  data <- data %>%
    select(4,6,7,16,17,18,19,28,29,33,34,54,55,56,57,58) %>% 
    filter(!is.na(new_family_name)) %>% 
    group_by(new_family_name) %>% count()
  
  data <- data.frame(data)
  
  families[[site]] <- data
  
}

# merge into a single dataframe
data <- families[[1]]

for(i in sites){
  data <- merge(data, families[[i+1]], all = TRUE, by = "new_family_name")
}

names(data) <- c("family", site_names)

# count the family richness of each site

family_richness <- colSums(!is.na(data[,-1]))


# count the number of species in each genus for each site ####

sites <- seq(1,length(liste_read_edna))
site_names <- names(liste_read_edna)

genera <- list()

for(site in sites){
  
  data <- liste_read_edna[[site]]
  data <- data %>%
    select(4,6,7,16,17,18,19,28,29,33,34,54,55,56,57,58) %>% 
    filter(!is.na(new_genus_name)) %>% 
    group_by(new_genus_name) %>% count()
  
  data <- data.frame(data)
  
  genera[[site]] <- data
  
}

# merge into a single dataframe
data <- genera[[1]]

for(i in sites){
  data <- merge(data, genera[[i+1]], all = TRUE, by = "new_genus_name")
}

names(data) <- c("genus", site_names)

# count the genus richness of each site

genus_richness <- colSums(!is.na(data[,-1]))

richness <- data.frame(t(rbind(family_richness, genus_richness)))
richness$site <- site_names
richness <- melt(richness)

# plot ####

ggplot(richness, aes(x = site, y = value)) +
  geom_point(aes(colour = variable))

# check to see if sequences are duplicated within filters ####


library(tidyverse)
library(reshape)

load("01_liste_all_read_edna.Rdata")

sites <- names(liste_read_edna)

summaries <- list()

for(site in sites){
  
  data    <- liste_read_edna[[site]]
  samples <- unique(data$sample_name)
  
  allUnique <- c()
  
  for(sample in samples){
    
    subset <- filter(data, data$sample_name == sample)
    
    allSeq <- length(subset$sequence)
    uniSeq <- length(unique(subset$sequence))
    
    unique <- allSeq == uniSeq
    
    allUnique <- c(allUnique,unique)
  }
  
  sum <- data.frame(samples,allUnique)
  
  summaries[[site]] <- sum
  
}


















