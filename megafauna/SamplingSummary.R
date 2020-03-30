# set session ####

setwd("Z:/LE_projects/megafauna/data/sandbox")

library(tidyverse)
library(readxl)

# parameters (defaults are hashed)
minSample   <- 0 #15
minSites    <- 1  #3
minSiteSize <- 10 #8
excludeRegion <- c("Indian Ocean") #c()
excludeSite <- c("Santa Marta") #c()

# load and wrangle data ####

# load data
data <- read_excel("./Browse samples.xlsx")

# remove missing NA genera and species
data <- filter(data, Genus != "NA" & species != "NA")

# add region
data$region <- data$Sample_site %in% c("Providencia","Santa Marta", "Martinique")
data$region[data$region == "TRUE"] <- "Caribbean"
data$region[data$region == "FALSE"] <- "Indian Ocean"

# create genus-species column
data$gen_sp <- paste(data$Genus,data$species, sep = " ")

# filter out regions and sites
data <- filter(data, !(region %in% excludeRegion)) %>% filter(!(Sample_site %in% excludeSite))

# count samples ####

countN   <- count(data,gen_sp)

# count extractions ####

# count the number of samples extracted per species
countExt <- group_by(data, gen_sp) %>%
  count(!is.na(Extraction_date)) %>%
  filter(`!is.na(Extraction_date)` == TRUE)
countExt <- select(countExt,gen_sp,n)
names(countExt) <- c("gen_sp","extract")

# count the number of samples extracted per site per species
countExtracted <- data.frame()
for(i in unique(data$gen_sp)){
  x <- filter(data, gen_sp == i) %>% 
    group_by(Sample_site) %>% 
    count(!is.na(Extraction_date))
  gen_sp <- as.character(rep(i, times = length(x$Sample_site)))
  x <- data.frame(gen_sp,x)
  names(x) <- c("gen_sp", "Sample_site", "extracted", "n")
  countExtracted <- rbind(countExtracted, x)
}
countExtracted <- spread(countExtracted, extracted, n)

# count sequenced ####

# count the number of samples sequenced per species
countRad <- group_by(data, gen_sp) %>%
  count(!is.na(RAD_library)) %>%
  filter(`!is.na(RAD_library)` == TRUE)
countRad <- select(countRad,gen_sp,n)
names(countRad) <- c("gen_sp","rad")

# count the number of samples sequenced per site per species
countRaded <- data.frame()
for(i in unique(data$gen_sp)){
  x <- filter(data, gen_sp == i) %>% 
    group_by(Sample_site) %>% 
    count(!is.na(RAD_library))
  gen_sp <- as.character(rep(i, times = length(x$Sample_site)))
  x <- data.frame(gen_sp,x)
  names(x) <- c("gen_sp", "Sample_site", "sequenced", "n")
  countRaded <- rbind(countRaded, x)
}
countRaded <- spread(countRaded, sequenced, n)

# count per site (siteSummary) ####

# count number of individuals per site
siteSummary <-group_by(data, gen_sp) %>%
  count(Sample_site) %>%
  spread(Sample_site,n)

siteSummary[is.na(siteSummary)] <- 0

no_sites    <- rowSums(!is.na(siteSummary))-1


# create shortlists ####

# min pop size (minSites-th largest site per species)
minPopSize <- c()
for(i in seq(1:dim(siteSummary)[1])){
  
  x <- as.numeric(siteSummary[i,])
  x <- sort(x[!is.na(x)], decreasing=TRUE)
  minPopSize <- c(minPopSize,min(x[1:minSites]))
  
}

# shortlist
shortlist <- cbind(countN,no_sites,minPopSize)
shortlist <- filter(shortlist, n >= minSample &
                      no_sites >= minSites &
                      minPopSize >= minSiteSize)
shortlist <- merge(shortlist,countExt, all.x = TRUE)
shortlist <- merge(shortlist,countRad, all.x = TRUE)
shortlist[is.na(shortlist)] <- 0

siteSummary <- filter(siteSummary, gen_sp %in% shortlist$gen_sp)

# extraction and sequencing progress
shortExt <- filter(countExtracted, gen_sp %in% shortlist$gen_sp)
shortExt[is.na(shortExt)] <- 0
shortExt$n <- shortExt$'FALSE' + shortExt$'TRUE'

shortRad <- filter(countRaded, gen_sp %in% shortlist$gen_sp)
shortRad[is.na(shortRad)] <- 0
shortRad$n <- shortRad$'FALSE' + shortRad$'TRUE'



# compare to manual shortlist ####

# read in spp lists
auto  <- shortlist$gen_sp
manual <- read_excel("./shortlist.xlsx", sheet=1)$gen_sp

# compare the lists
compareMan <- data.frame(manual,manual%in%auto)
compareAuto <- data.frame(auto,auto%in%manual)

# produce lists of samples to extract and sequence ####

extractList <- filter(data, gen_sp %in% shortlist$gen_sp) %>%
  filter(is.na(Extraction_plate))

radList <- filter(data, gen_sp %in% shortlist$gen_sp) %>%
  filter(is.na(RAD_library))

# plotting ####

plotAll <- ggplot(shortlist, aes(x = reorder(shortlist$gen_sp, -shortlist$n))) +
  geom_col(aes(y=n), colour = "darkgrey", fill = "lightgrey") +
  geom_col(aes(y=extract), colour="red", fill = "lightgrey") +
  geom_col(aes(y=rad), fill="red") +
  geom_text(aes(y=n+5, label=no_sites)) +
  ylim(c(0,160)) +
  xlab(element_blank()) +
  ylab("no. of samples") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.2),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour="grey", linetype = "solid"),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank())

plotExt <- ggplot(shortExt, aes(x = Sample_site)) +
  geom_col(aes(y = n)) +
  geom_col(aes(y = shortExt$'TRUE'), fill = "blue") +
  xlab(element_blank()) +
  ylab("no. of samples") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.2),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour="grey", linetype = "solid"),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank())

plotRad <- ggplot(shortRad, aes(x = Sample_site)) +
  geom_col(aes(y = n)) +
  geom_col(aes(y = shortRad$'TRUE'), fill = "red") +
  xlab(element_blank()) +
  ylab("no. of samples") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.2),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour="grey", linetype = "solid"),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank())




