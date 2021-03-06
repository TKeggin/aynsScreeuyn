# set session ####
library(tidyverse)

# set conditions ####
no_spp           <- 200           # number of species
niche_trait      <- runif(no_spp) # generate random traits
niche_width      <- 0.1           # competitive range
abundance_before <- runif(no_spp, min=1,max=1) # generate random initial abundances

names(niche_trait) <- 1:no_spp
names(abundance_before) <- 1:no_spp

# competition function ####
niche_comp    <- c() # nice vector house to put the competitive penalties in

for(i in 1:length(niche_trait)){
  sp_i        <- niche_trait[i] # niche trait of the species
  comp_range  <- c(sp_i-niche_width,sp_i+niche_width) # find the range of competition
  spp_comp    <- subset(niche_trait, niche_trait>comp_range[1] & niche_trait<comp_range[2]) # filter out non-competing species
  abd_comp    <- abundance_before[names(spp_comp)]
  comp_i      <- abundance_before[i]/sum(abd_comp) # competitive penalty is the proportion of abundance that species has in the competitive space
  #comp_i      <- (1/length(spp_comp)) # competitive penalty is the reciprocal of the number of competing species
  niche_comp  <- c(niche_comp,comp_i) # store penalty in the penalty vector
  
}

# calculate abundance ####
abundance_after  <- abundance_before*niche_comp

# plot ####
data <- data.frame(niche_trait,
                   niche_comp,
                   abundance_before,
                   abundance_after)

data_plot <- pivot_longer(data = data,
                          cols = (c(niche_comp,
                                    abundance_before,
                                    abundance_after)))

ggplot(data_plot) +
  geom_point(aes(x=niche_trait, y=value, size = name, colour = name), shape = 21, stroke = 2) +
  geom_density(data = data, aes(x=niche_trait, y=..scaled..)) +
  coord_cartesian(expand = FALSE,
                  xlim = c(0, 1), ylim = c(0, 1)) +
  theme_classic()

ggplot(data_plot) +
  geom_point(aes(x=niche_trait, y=value, colour = name)) +
  geom_density(data = data, aes(x=niche_trait, y=..scaled..)) +
  coord_cartesian(expand = FALSE,
                  xlim = c(0, 1), ylim = c(0, 1)) +
  theme_classic()

