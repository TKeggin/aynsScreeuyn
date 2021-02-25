#
# This script generates the per run summary metrics of a batch of simulations in an output folder
# Thomas Keggin (thomas.keggin@usys.ethz.ch)
#
# set session ----

run <- "6.2_all"

setwd(paste0("Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/",run))

library(tidyverse)
library(gen3sis)
library(ape)
library(raster)
library(PhyloMeasures)

# load functions
path_function <- "C:/Users/thoma/OneDrive/Documents/aynsScreeuyn/genesis/functions"
functions     <- list.files(path = path_function,pattern = ".R")
invisible(lapply(paste(path_function,functions,sep = "/"),source))


# set up true richness df for comparison ----
load("R:/data/raw_reefish_fish.range.global/mat_pa_cam_vs_gaspar.Rdata") # load gaspar richness data
ugly_mat_pa$richness <- rowSums(ugly_mat_pa[,-c(1,2)])

ugly_mat_pa <- ugly_mat_pa[,c("Longitude","Latitude","richness")]
rich <- filter(ugly_mat_pa, richness > 1)
rm(ugly_mat_pa)

# mask with input landscape.
land <- readRDS("D:/genesis/v1.0/input/1d_2000m_17c/landscapes.rds")$temp[,c(1,2,3)]
#land <- filter(land, land[,3] > 1)
land.raster <- rasterFromXYZ(land) # input raster
rich.raster <- rasterFromXYZ(rich) # richness raster
rich.raster <- resample(rich.raster,land.raster) # match extents
rich.masked <- mask(rich.raster, land.raster) # mask richness by the input

rich.true <- as.data.frame(rich.masked, xy = TRUE)

# create vectors ----
timestep_final <- c()

species.total <- c()
species.surviving <- c()
species.divergence <- c()
species.range <- c()

t_opt.mean <- c()
t_opt.max <- c()
t_opt.min <- c()
t_opt.range <- c()
t_opt.evenness <- c()
t_opt.diversity <- c()

niche.mean <- c()
niche.max <- c()
niche.min <- c()
niche.range <- c()
niche.sd <- c()
niche.diversity <- c()
niche.evenness <- c()

richness.avg <- c()
richness.max <- c()
richness.min <- c()
richness.range <- c()
richness.sd <- c()
richness.discrepancy <- c()

clusters.total <- c()
clusters.divergence.sim <- c()

# loop time ----
runs     <- list.files()
run_id   <- runs[!is.na(as.integer(runs))]

for(i in 1:length(run_id)){
  
  setwd(paste0(run_id[i]))
  
  # determine last time step
  timesteps      <- list.files("./richness")
  timestep       <- min(as.numeric(gsub(".*?([0-9]+).*", "\\1", timesteps)))  # this is bugged
  timestep_final <- c(timestep_final,timestep)
  
  if(timestep == 0){ # need to skip failed runs.
    
    # load data ----
    richness  <- readRDS(paste0("./richness/richness_t_",timestep,".rds"))
    species   <- readRDS(paste0("./species/species_t_",timestep,".rds"))
    summary   <- readRDS("sgen3sis.rds")
    landscape <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))
    coords    <- landscape$coordinates
    
    # species  ----
    species.total     <- c(species.total,summary$summary$phylo_summary['0',][1])
    species.surviving <- c(species.surviving,summary$summary$phylo_summary['0',][2])
    
    if(species.surviving[i] > 0){

      
      # species ----
      if(species.surviving[i] != 1){
        phylo              <- read.nexus(paste0("./phylogeny/phylogeny_t_",timestep,".nex"))      
        pa_dataframe       <- createPADF(species,landscape)
        species.divergence <- c(species.divergence,phyloDiversityMeanSim(pa_dataframe,phylo))
      } else {
        species.divergence <- c(species.divergence,NA)
      }
      
      species.range <- c(species.range,mean(speciesRange(pa_dataframe)))
      
      # t_opt ----
      t_opt <- c()
      for(sp in 1:species.total[i]){
        x     <- species[[sp]]$traits[,'t_opt']
        t_opt <- c(t_opt,x)
      }
      t_opt.mean  <- c(t_opt.mean,mean(t_opt))
      t_opt.max   <- c(t_opt.max,range(t_opt)[2])
      t_opt.min   <- c(t_opt.min,range(t_opt)[1])
      t_opt.range <- c(t_opt.range,t_opt.max[i]-t_opt.min[i])
      t_opt_evenness <- traitEvennessCell(species,"t_opt")
      t_opt.evenness <- c(t_opt.evenness, mean(t_opt_evenness[which(is.finite(t_opt_evenness))]))
      t_opt_diversity <- traitDiversity(species,"t_opt")
      t_opt.diversity <- c(t_opt.diversity, mean(t_opt_diversity[which(is.finite(t_opt_diversity))]))
      
      # niche ----
      niche <- c()
      for(sp in 1:species.total[i]){
        x     <- species[[sp]]$traits[,'niche']
        niche <- c(niche,x)
      }
      
      niche.mean   <- c(niche.mean,mean(niche))
      niche.max   <- c(niche.max,range(niche)[2])
      niche.min   <- c(niche.min,range(niche)[1])
      niche.range <- c(niche.range,niche.max[i]-niche.min[i])
      niche.sd    <- c(niche.sd,sd(niche))
      niche_evenness <- traitEvennessCell(species,"niche")
      niche.evenness <- c(niche.evenness, mean(niche_evenness[which(is.finite(niche_evenness))]))
      niche_diversity <- traitDiversity(species,"niche")
      niche.diversity <- c(niche.diversity, mean(niche_diversity[which(is.finite(niche_diversity))]))
      
      # richness ----
      richness.avg   <- c(richness.avg,mean(richness))
      richness.max   <- c(richness.max,max(richness))
      richness.min   <- c(richness.min,min(richness))
      richness.range <- c(richness.range,richness.max[i]-richness.min[i])
      richness.sd    <- c(richness.sd,sd(richness))
      
      # richness discrepancy ----
      # load in output
      rich.out <- data.frame(coords,richness)
      
      # merge everything
      rich.all <- merge(rich.out,rich.true, by = "row.names", all.x = TRUE)
      rich.all <- rich.all[,c(1,2,3,4,7)]
      colnames(rich.all) <- c("cell","x","y","rich.out","rich.true")
      rich.all <- filter(rich.all, !is.na(rich.true))
      
      # normalise to 0-1 to make comparable
      rich.all$rich.true <- rich.all$rich.true/max(rich.all$rich.true)
      rich.all$rich.out  <- rich.all$rich.out/max(rich.all$rich.out)
      rich.all$discrepency <- sqrt((rich.all$rich.true-rich.all$rich.out)^2)
      
      richness.discrepancy <- c(richness.discrepancy,mean(rich.all$discrepency))
      
      # cluster ----
      # per species
      clusters.sp <- c()
      for(sp in 1:species.total[i]){
        x        <- species[[sp]]$divergence$index
        clusters.sp <- c(clusters.sp,length(unique(x)))
      }
      #names(clusters.sp) <- 1:species.total[1]
      clusters.total <- c(clusters.total,sum(clusters.sp))
      
      # cluster divergence
      clusters.divergence.sim <- c(clusters.divergence.sim,clusterDivergenceSim(species))
      
      
      
    } else {
      species.divergence <- c(species.divergence,NA)
      species.range      <- c(species.range,NA)
      
      t_opt.mean  <- c(t_opt.mean,NA)
      t_opt.max <- c(t_opt.max,NA)
      t_opt.min <- c(t_opt.min,NA)
      t_opt.range <- c(t_opt.range,NA)
      t_opt.evenness <- c(t_opt.evenness,NA)
      t_opt.diversity <- c(t_opt.diversity,NA)
      
      niche.mean <- c(niche.mean,NA)
      niche.max <- c(niche.max,NA)
      niche.min <-  c(niche.min,NA)
      niche.range <-  c(niche.range,NA)
      niche.sd <-  c(niche.sd,NA)
      niche.evenness <- c(niche.evenness,NA)
      niche.diversity <- c(niche.diversity,NA)
      
      richness.avg <-  c(richness.avg,NA)
      richness.max <-  c(richness.max,NA)
      richness.min <-  c(richness.min,NA)
      richness.range <-  c(richness.range,NA)
      richness.sd <-  c(richness.sd,NA)
      richness.discrepancy <- c(richness.discrepancy, NA)
      
      clusters.total <-  c(clusters.total,NA)
      clusters.divergence.sim <-  c(clusters.divergence.sim,NA)
      
    }
  } else {
    species.total     <- c(species.total,NA)
    species.surviving <- c(species.surviving,NA)
    species.divergence <- c(species.divergence,NA)
    species.range      <- c(species.range,NA)
    
    t_opt.mean  <- c(t_opt.mean,NA)
    t_opt.max <- c(t_opt.max,NA)
    t_opt.min <- c(t_opt.min,NA)
    t_opt.range <- c(t_opt.range,NA)
    t_opt.evenness <- c(t_opt.evenness,NA)
    t_opt.diversity <- c(t_opt.diversity,NA)
    
    niche.mean <- c(niche.mean,NA)
    niche.max <- c(niche.max,NA)
    niche.min <-  c(niche.min,NA)
    niche.range <-  c(niche.range,NA)
    niche.sd <-  c(niche.sd,NA)
    niche.evenness <- c(niche.evenness,NA)
    niche.diversity <- c(niche.diversity,NA)
    
    richness.avg <-  c(richness.avg,NA)
    richness.max <-  c(richness.max,NA)
    richness.min <-  c(richness.min,NA)
    richness.range <-  c(richness.range,NA)
    richness.sd <-  c(richness.sd,NA)
    richness.discrepancy <- c(richness.discrepancy, NA)
    
    clusters.total <-  c(clusters.total,NA)
    clusters.divergence.sim <-  c(clusters.divergence.sim,NA)
  }
    
  print(paste("run",run_id[i],"done"))
  
  setwd("../")
}


# output summary table ----
config <- read_csv(paste0("Y:/TKeggin/genesis/v1.0/config/",run,"/config_parameters.csv"))

summary_table <- data.frame(run_id,
                            timestep_final,
                            species.total,
                            species.surviving,
                            species.divergence,
                            species.range,
                            t_opt.mean,
                            t_opt.max,
                            t_opt.min,
                            t_opt.range,
                            t_opt.evenness,
                            t_opt.diversity,
                            niche.mean,
                            niche.max,
                            niche.min,
                            niche.range,
                            niche.sd,
                            niche.evenness,
                            niche.diversity,
                            richness.avg,
                            richness.max,
                            richness.min,
                            richness.range,
                            richness.sd,
                            richness.discrepancy,
                            clusters.total,
                            clusters.divergence.sim)
summary_table <- merge(config,summary_table)


write_csv(summary_table, "./summary_table.csv")
