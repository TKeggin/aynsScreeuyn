# This script takes calculates the continuity between clusters and species
# in a series of simulations between ecoregions as defined by Spalding et
# al 2007

# set session ####
library(tidyverse)
library(gridExtra)
library(gtools)

# load ecoregions ####
ecoregions <- read_csv("C:/Users/thoma/OneDrive/Documents/PhD/chapter_1/analysis/regional_continuity/01_sim_ecoregions.csv")

# loop for runs ####
# set main working directory to main output folder
home <- "Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c"
setwd(home)

# list the folders for a series of batches (5.x only)
batches <- list.files()[grepl(5,list.files())]

# set the timestep you want to look at
timestep <- 0

# define tropical realms
tropical_realms <- c("Central Indo-Pacific",
                     "Tropical Atlantic",
                     "Tropical Eastern Pacific",
                     "Western Indo-Pacific")
abbrs <- c("CIP",
           "TA",
           "TEP",
           "WIP")

# create summary table to enter results into
regional_continuity <- data.frame(batch      = c(),
                                  run        = c(),
                                  comparison = c(),
                                  continuity = c())

# start batch loop
for(batch in 1:length(batches)){
  
  setwd(home)
  
  # list runs in batch (filter out non-integer file names as they are not runs)
  runs <- list.files(paste0("./",batches[batch]))
  runs <- runs[!is.na(as.integer(runs))] # "NAs introduced by coercion" is fine.
  
  for(run in 1:length(runs)){
    
    setwd(home)
    setwd(paste0("./",batches[batch],"/",runs[run])) # enter run directory
    
    species    <- readRDS(paste0("./species/species_t_",timestep,".rds"))
    landscape  <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))
    
    # create df of all required data to be added to
    all_sim      <- data.frame(coords   = landscape$coordinates,
                               realm    = ecoregions$layer_REALM)
    all_sim$cell <- rownames(all_sim) 
    
    # create a data frame to contain all realm and species information ####
    species_extract <- data.frame(cell = c(),
                                  species_id = c(),
                                  cluster_id = c())
    
    for(i in 1:length(species)){
      if(length(species[[i]]$abundance) > 0){
        x <- data.frame(cell = names(species[[i]]$divergence$index),
                        species_id = species[[i]]$id,
                        cluster_id = paste0(species[[i]]$id,"_",species[[i]]$divergence$index))
        species_extract <- rbind(species_extract,x)
      }
    }
    
    # skip if all species are extinct
    skip_condition <- length(species_extract) == 0
    if(!skip_condition){ # continue if the skip condition is false
      
      all_sim <- inner_join(species_extract,all_sim)
      
      # count number of species and clusters per realm
      
      species_sim <- all_sim %>% select(species_id,realm)
      species_sim <- species_sim %>% distinct()
      species_count <- species_sim %>% count(realm)
      colnames(species_count) <- c("realm","species")
      
      cluster_sim <- all_sim %>% select(cluster_id,realm)
      cluster_sim <- cluster_sim %>% distinct()
      cluster_count <- cluster_sim %>% count(realm)
      colnames(cluster_count) <- c("realm","cluster")
      
      summary <- full_join(species_count,cluster_count)
      summary$continuity <- summary$species/summary$cluster
      
      tropical_summary <- summary %>% filter(realm %in% tropical_realms)
      tropical_summary <- data.frame(batch = batches[batch],
                                     run = runs[run],
                                     realm = tropical_summary$realm,
                                     continuity = tropical_summary$continuity)
      
      
      # calculate the comparisons (CIP,TA,TEP,WIP)
      pairs     <- as.data.frame(combinations(n = length(tropical_summary$realm), r = 2, v = tropical_summary$realm, repeats.allowed = FALSE))
      pairs_num <- pairs
      
      # replace realm names with continuity values
      for(i in 1:length(tropical_realms)){
        pairs_num[pairs_num == tropical_realms[i]] <- tropical_summary$continuity[which(tropical_summary$realm == tropical_realms[i])]
      }
      
      # calculate the realm comparisons
      pairs_num$continuity <- as.numeric(pairs_num$V1) - as.numeric(pairs_num$V2)
      pairs_num$continuity <- sqrt(pairs_num$continuity^2)
      
      # bind to comparison data frame
      pairs$continuity <- pairs_num$continuity
      
      # abbreviate realm names
      for(i in 1:length(tropical_realms)){
        pairs[pairs == tropical_realms[i]] <- abbrs[which(tropical_realms == tropical_realms[i])]
      }
      
      # create comparison column
      comparison <- paste0(pairs$V1,"-",pairs$V2)
      
      pairs <- data.frame(comparison,pairs$continuity)
      
      # bind results to summary data frame
      tropical_summary <- data.frame(batch = batches[batch],
                                     run = runs[run],
                                     pairs)
      colnames(tropical_summary)[4] <- "continuity"
      
      regional_continuity <- rbind(regional_continuity, tropical_summary)
      
      # there needs be another output table for just regional continuity values #############
      
    }
    print(paste0(batches[batch],"/",runs[run]," done."))
  }
  
}

# output the summary ####

write_csv(regional_continuity, "C:/Users/thoma/OneDrive/Documents/PhD/chapter_1/analysis/regional_continuity/02_regional_continuity_comparisons.csv")














