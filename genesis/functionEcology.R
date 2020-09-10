# set session ####
library(tidyverse)

# look at the normal distribution ####
values <- rnorm(100000, mean=50, sd=15)
test   <- data.frame(values)

ggplot(test, aes(x =values)) +
  geom_histogram(bins = 1000)

# competition function ####
no_spp <- 50
# niche inputs
niche_trait <- runif(no_spp) # generate random traits
#niche_trait <- c(10,10,10,10,10,11,11,90)
niche_width <- 0.1  # competitive range
niche_comp  <- c() # competition penalty

# calculate competition penalty
for(i in 1:length(niche_trait)){
  
  sp_i        <- niche_trait[i]
  spp_other   <- niche_trait[-i]
  comp_range  <- c(sp_i-niche_width,sp_i+niche_width) # find the range of competition
  #spp_comp    <- subset(spp_other, spp_other>comp_range[1] & spp_other<comp_range[2]) # filter out non-competing species
  spp_comp    <- subset(niche_trait, niche_trait>comp_range[1] & niche_trait<comp_range[2]) # filter out non-competing species
  comp_i      <- sum(sqrt((sp_i-spp_comp)^2)) # find the sum of the distances between species i and all other species
  #comp_i      <- length(spp_comp)/length(niche_trait) # find the proportion of the total species
  #comp_i      <- (1/length(spp_comp))
  niche_comp  <- c(niche_comp,comp_i)
}
niche_comp <- niche_comp/max(niche_comp) # scale to 0-1
niche_comp[is.nan(niche_comp)] <- 0      # remove NaN values (if max(niche_comp) == 0)

# calculate energetic bias
# <- dnorm(50, mean = 50, sd = 15)
#abundance <- (100/abd_opt)*dnorm(niche_trait, mean = 50, sd = 15)

# calculate abundance
#abundance <- rep(100, no_spp)
#abundance <- sample(30:100, size = no_spp, replace = TRUE)
abundance_before <- runif(no_spp, min=0.9,max=0.9)
abundance_after  <- abundance_before*(1-niche_comp)

# plot
data <- data.frame(niche_trait,
                   niche_comp,
                   abundance_before,
                   abundance_after)

data <- pivot_longer(data = data, cols = (c(niche_comp,abundance_before,abundance_after)))

ggplot(data) +
  geom_line(aes(x=niche_trait, y=value, colour = name)) +
  coord_cartesian(expand = FALSE,
                xlim = c(0, 1), ylim = c(0, 1)) +
  theme_classic()

