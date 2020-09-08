# set session ####

setwd("M:/data/temp_customs")

library("tidyverse")
library("readxl")
library("measurements")

# load data ####

data <- read_excel("./metadata_eDNA_Svalbard_MJ.xlsx", sheet = 2)

# convert coordinates ####

for(i in c("latitude_start","longitude_start","latitude_end","longitude_end")){
  
  data[i] <- gsub('°',' ', data[[i]])
  data[i] <- gsub('\\.',' ', data[[i]])
  data[i] <- gsub('N','', data[[i]])
  data[i] <- gsub('E','-', data[[i]])
  
  data[i] <- conv_unit(data[[i]], from = 'deg_min_sec', to = 'dec_deg')
  
}





# output data

write_csv(data,"./Mayotte.csv")
