library(tidyverse)

# check dimensions of marrey data

setwd("C:/Users/thoma/Documents/Marrey/Input/6d")

all_geo_hab <- readRDS("all_geo_hab.rds")
for(i in c(1,2)){
  print(dim(all_geo_hab[[i]]))
}

length(list.files("./distance_matrices/"))

step0 <- readRDS("./distance_matrices/geo_dist_m_ti_t_0.rds")
dim(step0)

# check number of non-NA cells in the timestep

sum(!is.na(all_geo_hab[[1]][,1203]))

# plot all_geo_hab...

data <- all_geo_hab[[2]]


# check dimensions of sample data ####

setwd("C:/Users/thoma/Desktop/gasm/input/sample_world_4d")
all_geo_hab <- readRDS("all_geo_hab.rds")

for(i in c(1,2)){
  print(dim(all_geo_hab[[i]]))
}

length(list.files("./distance_matrices/"))



