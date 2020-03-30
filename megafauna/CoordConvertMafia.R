# set session ####

setwd("Z:/LE_projects/megafauna/data/sandbox")

library("tidyverse")
library("readxl")
library("measurements")

# load and wrangle data ####

data <- read_excel("../raw_megafauna_metadata/raw_WIO/Species_Captured_ Mafia_2017.xlsx", sheet = 1) %>% 
  distinct(Sample_ID, .keep_all = TRUE)

colnames(data)[8:9] <- c("latitude","longitude")

data <- filter(data, Sample_ID != "NO CATCH")

data[ data == "NA" ] <- NA

# split the data into DMM and DMS ####

# convert ' to _ to ease handling

data$latitude <-  gsub('\'',' ', data$latitude)
data$longitude <- gsub('\'',' ', data$longitude)

# filter formats

dmm <- filter(data, !grepl("  ",latitude))
dms <- filter(data, grepl("  ",latitude))

# convert DMM to DD ####

dmm$latitude  <- gsub(' ','', dmm$latitude)
dmm$longitude <- gsub(' ','', dmm$longitude)

dmm$latitude  <- gsub('°',' ', dmm$latitude)
dmm$longitude <- gsub('°',' ', dmm$longitude)

dmm$latitude = conv_unit(dmm$latitude, from = 'deg_dec_min', to = 'dec_deg')
dmm$longitude = conv_unit(dmm$longitude, from = 'deg_dec_min', to = 'dec_deg')


# convert DMS to DD ####

dms$latitude  <- gsub('  ','', dms$latitude)
dms$longitude <- gsub('  ','', dms$longitude)

dms$latitude <-  gsub('°',' ', dms$latitude)
dms$longitude <- gsub('°',' ', dms$longitude)

dms$latitude = conv_unit(dms$latitude, from = 'deg_min_sec', to = 'dec_deg')
dms$longitude = conv_unit(dms$longitude, from = 'deg_min_sec', to = 'dec_deg')

# combine converted data back together ####

mafia <- rbind(dms,dmm)

# export data ####

write_excel_csv(mafia, "./mafia.csv")





