#
# This script generates the per run summary metrics of a batch of simulations in an output folder
# Thomas Keggin (thomas.keggin@usys.ethz.ch)
#
# set session ####

setwd("Y:/TKeggin/genesis/v1.0/output/1d_2000m_17c/5_all")

library(tidyverse)
library(gen3sis)
library(ape)
library(raster)

# set up true richness df for comparison ####
load("R:/data/raw_reefish_fish.range.global/mat_pa_cam_vs_gaspar.Rdata") # load gaspar richness data
ugly_mat_pa$richness <- rowSums(ugly_mat_pa[,-c(1,2)])

ugly_mat_pa <- ugly_mat_pa[,c("Longitude","Latitude","richness")]
rich <- filter(ugly_mat_pa, richness > 1)
rm(ugly_mat_pa)

# mask with input landscape.
land <- readRDS("D:/genesis/input/1d_2000m_17c/landscapes.rds")$temp[,c(1,2,3)]
#land <- filter(land, land[,3] > 1)
land.raster <- rasterFromXYZ(land) # input raster
rich.raster <- rasterFromXYZ(rich) # richness raster
rich.raster <- resample(rich.raster,land.raster) # match extents
rich.masked <- mask(rich.raster, land.raster) # mask richness by the input

rich.true <- as.data.frame(rich.masked, xy = TRUE)

# create vectors ####
species.total <- c()
species.surviving <- c()

t_opt.max <- c()
t_opt.min <- c()
t_opt.range <- c()

niche.avg <- c()
niche.max <- c()
niche.min <- c()
niche.range <- c()
niche.sd <- c()

richness.avg <- c()
richness.max <- c()
richness.min <- c()
richness.range <- c()
richness.sd <- c()
richness.discrepancy <- c()

clusters.total <- c()

# loop time ####
run_id   <- list.files()[-21]
timestep <- 0

for(i in 1:length(run_id)){
  
  setwd(paste0(run_id[i]))
  
  # load data ####
  
  richness  <- readRDS(paste0("./richness/richness_t_",timestep,".rds"))
  species   <- readRDS(paste0("./species/species_t_",timestep,".rds"))
  summary   <- readRDS("sgen3sis.rds")
  landscape <- readRDS(paste0("./landscapes/landscape_t_",timestep,".rds"))
  coords    <- landscape$coordinates
  
  # species  ####
  species.total     <- c(species.total,summary$summary$phylo_summary['0',][1])
  species.surviving <- c(species.surviving,summary$summary$phylo_summary['0',][2])
  
  
  if(species.surviving[i] > 0){
    # phylogeny ####
    #phylogeny <- read.nexus("phy.nex")
    #plot.phylo(phylogeny)
    
    # t_opt ####
    t_opt <- c()
    for(sp in 1:species.total[i]){
      x     <- species[[sp]]$traits[,'t_opt']
      t_opt <- c(t_opt,x)
    }
    t_opt.max   <- c(t_opt.max,range(t_opt)[2])
    t_opt.min   <- c(t_opt.min,range(t_opt)[1])
    t_opt.range <- c(t_opt.range,t_opt.max[i]-t_opt.min[i])
    
    # niche ####
    niche <- c()
    for(sp in 1:species.total[i]){
      x     <- species[[sp]]$traits[,'niche']
      niche <- c(niche,x)
    }
    
    niche.avg   <- c(niche.avg,mean(niche))
    niche.max   <- c(niche.max,range(niche)[2])
    niche.min   <- c(niche.min,range(niche)[1])
    niche.range <- c(niche.range,niche.max[i]-niche.min[i])
    niche.sd    <- c(niche.sd,sd(niche))
    
    # richness ####
    richness.avg   <- c(richness.avg,mean(richness))
    richness.max   <- c(richness.max,max(richness))
    richness.min   <- c(richness.min,min(richness))
    richness.range <- c(richness.range,richness.max[i]-richness.min[i])
    richness.sd    <- c(richness.sd,sd(richness))
    
    # richness discrepancy ####
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
    
    # cluster ####
    # per species
    clusters.sp <- c()
    for(sp in 1:species.total[i]){
      x        <- species[[sp]]$divergence$index
      clusters.sp <- c(clusters.sp,length(unique(x)))
    }
    #names(clusters.sp) <- 1:species.total[1]
    clusters.total <- c(clusters.total,sum(clusters.sp))
    
  } else {
    t_opt.max <- c(t_opt.max,NA)
    t_opt.min <- c(t_opt.min,NA)
    t_opt.range <- c(t_opt.range,NA)
    
    niche.avg <- c(niche.avg,NA)
    niche.max <- c(niche.max,NA)
    niche.min <-  c(niche.min,NA)
    niche.range <-  c(niche.range,NA)
    niche.sd <-  c(niche.sd,NA)
    
    richness.avg <-  c(richness.avg,NA)
    richness.max <-  c(richness.max,NA)
    richness.min <-  c(richness.min,NA)
    richness.range <-  c(richness.range,NA)
    richness.sd <-  c(richness.sd,NA)
    richness.discrepancy <- c(richness.discrepancy, NA)
    
    clusters.total <-  c(clusters.total,NA)

  }
  
  
  setwd("../")
}

config <- read_csv("Y:/TKeggin/genesis/v1.0/config/5.1_all/config_parameters.csv")

summary_table <- data.frame(run_id,
  species.total,
                            species.surviving,
                            t_opt.max,
                            t_opt.min,
                            t_opt.range,
                            niche.avg,
                            niche.max,
                            niche.min,
                            niche.range,
                            niche.sd,
                            richness.avg,
                            richness.max,
                            richness.min,
                            richness.range,
                            richness.sd,
  richness.discrepancy,
  clusters.total)
summary_table <- merge(config,summary_table)


write_csv(summary_table, "./summary_table.csv")


# per cell ####
clusters.cell <- c()
clusters.spp  <- c()
for(sp in 1:species.total){
  x        <- species[[sp]]$divergence$index
  clusters.cell <- c(clusters.cell,x)
  clusters.spp  <- c(clusters.spp,rep(sp,length(x)))
}
clusters.df           <- data.frame(names(clusters.cell),clusters.cell,clusters.spp)
colnames(clusters.df) <- c("cell","cluster","species")






















