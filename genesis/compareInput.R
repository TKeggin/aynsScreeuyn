
getwd()



land_thomas <- readRDS("Y:/TKeggin/genesis/input/6d/landscapes.rds")
land_sample <- readRDS("Y:/TKeggin/genesis/input/sample_world_4d/landscapes.rds")

dist_thomas <- readRDS("Y:/TKeggin/genesis/input/6d/distances_full/distances_full_900.rds")
dist_sample <- readRDS("Y:/TKeggin/genesis/input/sample_world_4d/distances_full/distances_full_0.rds")

dim(dist_sample)
dim(dist_thomas)
mode(dist_thomas)
mode(dist_sample)

# comapare no. of habitable cells in landscapes and dist matrices

hab_cells_land <- c()
for(i in seq(1:dim(land_thomas$temp)[2]-2)){
  
  hab_cells_land <- c(hab_cells_land,length(rownames(land_thomas$temp)[which(!is.na(land_thomas$temp[,i]))]))
}

hab_cells_land <- hab_cells_land[-1]
hab_cells_land <- hab_cells_land[-1]

hab_cells_dist <- c()
for(i in seq(1:length(hab_cells_land))){
  
  hab_cells_dist <- c(hab_cells_dist,dim(readRDS(paste0("Y:/TKeggin/genesis/input/6d/distances_full/distances_full_",i-1,".rds")))[1])
  print(i)
}

all.equal(hab_cells_dist,hab_cells_land)

dim(dist_sample) # no. of cells in distance matrix

summary(land_sample$temp[,3])
dim(land_sample$temp)

i <- 1 # set timestep

length(rownames(land_thomas$temp)[which(!is.na(land_thomas$temp[,i+2]))])

dim(readRDS("Y:/TKeggin/genesis/input/6d/distances_full/distances_full_0.rds"))
