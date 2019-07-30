# set session ####

setwd("Z:/LE_projects/megafauna/data/sandbox")

library(tidyverse)
library(readxl)

# parameters
minSample <- 30
minSite   <- 3

# load and wrangle data ####

data <- read_excel("./Browse samples.xlsx")

data$gen_sp <- paste(data$Genus,data$species, sep = " ")

# create summaries ####

countSpp <- count(data,gen_sp)

countRad <- group_by(data, gen_sp) %>%
            count(!is.na(RAD_library)) %>%
            filter(`!is.na(RAD_library)` == TRUE)
countRad <- select(countRad,gen_sp,n)
names(countRad) <- c("gen_sp","rad")

countExt <- group_by(data, gen_sp) %>%
            count(!is.na(Extraction_date)) %>%
            filter(`!is.na(Extraction_date)` == TRUE)
countExt <- select(countExt,gen_sp,n)
names(countExt) <- c("gen_sp","extract")

shortlist <- filter(countSpp, n >= minSample)

# make a summary of the number of samples per site per species
siteSummary <-  filter(data, gen_sp %in% shortlist$gen_sp) %>%
                group_by(gen_sp) %>%
                count(Sample_site) %>%
                spread(Sample_site,n)
siteSummary <-  siteSummary[,-length(colnames(siteSummary))]

no_sites    <- rowSums(!is.na(siteSummary))-1
shortlist   <- cbind(shortlist,no_sites)
shortlist   <- filter(shortlist, no_sites >= minSite)
siteSummary <- filter(siteSummary, gen_sp %in% shortlist$gen_sp)

# count the number extracted and RAD sequenced

shortlist <- merge(shortlist,countExt, all.x = TRUE)
shortlist <- merge(shortlist,countRad, all.x = TRUE)

# output summaries ####

write_csv(shortlist, path = "./shortlist.csv")
