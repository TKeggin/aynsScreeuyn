# set sesson ----

library(tidyverse)
library(readxl)

# load data ----

metadata   <- read_excel("M:/data/temp_megafauna_exports/Browse samples.xlsx")
target_spp <- read_excel("C:/Users/thoma/OneDrive/Documents/PhD/chapter_2/meta/target_spp.xlsx")

# split into ocean basins ----

carib <- target_spp %>% filter(ocean == "carib")
wio   <- target_spp %>% filter(ocean == "wio")

wio_data   <- metadata %>% filter(species %in% wio$species)
carib_data <- metadata %>% filter(species %in% carib$species)

# Check WIO for potential ----

# check to Maldives morpho viability
A_clarkii <- metadata %>% filter(species == "Amphiprion_clarkii") # not enough samples (maybe whole genome seq?)

# filter those with libraries
wio_lib <- wio_data %>% filter(!is.na(library_number))

wio_sum <- wio_lib %>% 
  group_by(sample_site,species) %>% 
  count() %>% 
  pivot_wider(names_from = sample_site, values_from = n)

# summarise Caribbean samples ----
# split into viable and unviable samples
unique(carib_data$tissue_status)
carib_data$viable <- carib_data$tissue_status %in% c("good","small",NA,"overpacked","good_double","small_double")

carib_viable <- carib_data %>% filter(viable == TRUE)

carib_sum <- carib_viable %>% 
  group_by(sample_site,species) %>% 
  count() %>% 
  pivot_wider(names_from = sample_site, values_from = n)
carib_sum2 <- carib_data %>% 
  group_by(sample_site,species) %>% 
  count() %>% 
  pivot_wider(names_from = sample_site, values_from = n)


# export data ----

write_csv(A_clarkii, "C:/Users/thoma/OneDrive/Documents/PhD/chapter_2/A_clarkii.csv")

write_csv(carib_sum,"C:/Users/thoma/OneDrive/Documents/PhD/chapter_2/meta/caribbean_samples.csv")
