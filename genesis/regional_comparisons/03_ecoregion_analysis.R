# this script joins together existing summaries of each run and the regional continuity values
# set session ####
library(tidyverse)

# load data ####
ecoregions          <- read_csv("C:/Users/thoma/OneDrive/Documents/PhD/chapter_1/analysis/regional_continuity/01_sim_ecoregions.csv")
regional_continuity <- read_csv("C:/Users/thoma/OneDrive/Documents/PhD/chapter_1/summaries/regional_continuity.csv")
run_summaries       <- read_csv("C:/Users/thoma/OneDrive/Documents/PhD/chapter_1/summaries/all.csv")

# calculate overall run continuity ####

run_summaries$continuity <- run_summaries$species.surviving/run_summaries$clusters.total

# make data joinable ####
colnames(regional_continuity)[1:2] <- c("batch_id","run_id")

regional_continuity$id <- paste0(regional_continuity$batch_id,"_",regional_continuity$run_id)
run_summaries$id       <- paste0(run_summaries$batch_id,"_all_",run_summaries$run_id)

# join the data ####

data <- inner_join(run_summaries,regional_continuity)

# plot the data ####

plot_data <- data %>% filter(realm == "Central Indo-Pacific" &
                               is.na(start))

ggplot(plot_data, aes(x=dispRange, y = continuity)) +
  geom_point(aes(colour = as.factor(batch_id)))





