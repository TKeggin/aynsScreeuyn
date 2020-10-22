setwd("C:/Users/thoma/OneDrive/Documents/cod_optimisation")

library(tidyverse)
library(readxl)

weapon_placeholder <- "m4_socom"

raw  <- read_excel("./cod.xlsx")[,-15]
data <- filter(raw, weapon == weapon_placeholder)

barrel      <- filter(data, type == "barrel")[,-c(1:3)]
laser       <- filter(data, type == "laser")[,-c(1:3)]
muzzle      <- filter(data, type == "muzzle")[,-c(1:3)]
optic       <- filter(data, type == "optic")[,-c(1:3)]
stock       <- filter(data, type == "stock")[,-c(1:3)]
underbarrel <- filter(data, type == "underbarrel")[,-c(1:3)]
ammo        <- filter(data, type == "ammo")[,-c(1:3)]

variant_metrics     <- data[1,-c(1:4)]
variant_attachments <- data.frame()

for(b in 1:dim(barrel)[1]){
  for(l in 1:dim(laser)[1]){
    for(m in 1:dim(muzzle)[1]){
      for(o in 1:dim(optic)[1]){
        for(s in 1:dim(stock)[1]){
          for(u in 1:dim(underbarrel)[1]){
            for(a in 1:dim(ammo)[1]){
              
              attachments <- c(as.character(barrel[b,"attachment"]),
                               as.character(laser[l,"attachment"]),
                               as.character(muzzle[m,"attachment"]),
                               as.character(optic[o,"attachment"]),
                               as.character(stock[s,"attachment"]),
                               as.character(underbarrel[u,"attachment"]),
                               as.character(ammo[a,"attachment"]))
              
              metrics <- barrel[b,-1] +
                laser[l,-1] +
                muzzle[m,-1] +
                optic[o,-1] +
                stock[s,-1] +
                underbarrel[u,-1] +
                ammo[a,-1]
              
              variant_attachments <- rbind(variant_attachments,attachments)
              variant_metrics     <- rbind(variant_metrics,metrics)
            }
          }
        }
      }
    }
    print(paste("Laser ",l," of ", dim(laser)[1], " done"))
  }
  print(paste("Barrel ",b," of ", dim(barrel)[1], " done"))
}

variant_metrics <- variant_metrics[-1,] # remove placeholder first row
colnames(variant_attachments) <- c("barrel","laser","muzzle","optic","stock","underbarrel","ammo")

variants_raw <- cbind(variant_attachments,variant_metrics)
variants <- variants_raw

# count the empty slots in a variant
variants$default_count <- rowSums(variants== "default")

# output viable variants
write_csv(variants, paste0("./",weapon_placeholder,".csv"))
