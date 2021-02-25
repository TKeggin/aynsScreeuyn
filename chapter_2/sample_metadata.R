# set sesson ----

library(tidyverse)
library(readxl)

# load data ----

metadata   <- read_excel("C:/Users/thoma/OneDrive/Documents/PhD/chapter_2/meta/sample_data.xlsx", sheet = 1)
target_spp <- read_excel("C:/Users/thoma/OneDrive/Documents/PhD/chapter_2/meta/sample_data.xlsx", sheet = 2)

# process data ----

carib <- target_spp %>% filter(ocean == "carib")
wio   <- target_spp %>% filter(ocean == "wio")

data <- metadata %>% filter(species %in% wio$species)

# export data ----

write_csv(data,"C:/Users/thoma/OneDrive/Documents/PhD/chapter_2/wio_spp_meta.csv")
