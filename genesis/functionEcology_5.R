# set session ####
library(tidyverse)
library(microbenchmark)

# set conditions ####
no_spp           <- 10 # number of species
niche_trait      <- runif(no_spp, min=0.5,max=0.5) # generate random traits
niche_width      <- 0.1           # competitive range
abundance_before <- c(0,runif(no_spp-1, min=0,max=1)) # generate random initial abundances

# competition function ####
niche_comp    <- c() # nice vector house to put the competitive penalties in
niche_trans   <- c() # same as above, but for the transgressions

for(i in 1:length(niche_trait)){
  sp_i        <- niche_trait[i] # niche trait of the species
  comp_range  <- c(sp_i-niche_width,sp_i+niche_width) # find the range of competition
  abd_comp    <- abundance_before[niche_trait>comp_range[1] & niche_trait<comp_range[2]] # the abundances of the competing species
  comp_i      <- abundance_before[i]/sum(abd_comp) # competitive penalty is the proportion of abundance that species has in the competitive space

  #proportion out-of-bounds
  if(comp_range[2] > 1){
    trans         <- comp_range[2]-1
    trans_penalty <- trans/(comp_range[2]-comp_range[1])
  } else {if(comp_range[1] < 0){
    trans         <- -comp_range[1]
    trans_penalty <- trans/(comp_range[2]-comp_range[1])
  } else {
    trans_penalty <- 0
  }}
  
  niche_trans <- c(niche_trans, trans_penalty) # store transgression in the trans vector
  niche_comp  <- c(niche_comp,comp_i)          # store penalty in the penalty vector
  
}

# calculate abundance ####
abundance_after <- abundance_before*(1-niche_trans) # apply transgression penalty
abundance_after <- abundance_after*niche_comp       # apply competition penalty
abundance_after <- ifelse(abundance_after < 0.1, 0, abundance_after) # drive to extinction under 0.1

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
  ggtitle("") +
  geom_point(aes(x=niche_trait, y=value, colour = name)) +
  #geom_density(data = data, aes(x=niche_trait, y=..scaled..)) +
  coord_cartesian(expand = FALSE,
                  xlim = c(0, 1), ylim = c(0, 100)) +
  theme_classic()

#ggplot(data_plot) +
#  geom_point(aes(x=niche_trait, y=value, size = name, colour = name), shape = 21, stroke = 2) +
#  geom_density(data = data, aes(x=niche_trait, y=..scaled..)) +
#  coord_cartesian(expand = FALSE,
#                  xlim = c(0, 1), ylim = c(0, 1)) +
#  theme_classic()


