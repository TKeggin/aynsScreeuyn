# set session ####

setwd("Z:/LE_projects/megafauna/data/sandbox")

library("tidyverse")
library("readxl")
library("measurements")

# load data ####

data <- read_excel("./Browse samples.xlsx")

# convert coordinates ####

data$latitude <-  gsub('°',' ', data$latitude)
data$longitude <- gsub('°',' ', data$longitude)
data$latitude <-  gsub('\'',' ', data$latitude)
data$longitude <- gsub('°',' ', data$longitude)

data$latitude = measurements::conv_unit(data$latitude, from = 'deg_dec_min', to = 'dec_deg')
data$longitude = measurements::conv_unit(data$longitude, from = 'deg_dec_min', to = 'dec_deg')

data$latitude <- -as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)

# output data

write_csv(data,"./Mayotte.csv")
