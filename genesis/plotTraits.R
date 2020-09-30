# set session ####
library(tidyverse)

setwd("C:/Users/thoma/OneDrive/Documents/PhD/genesis/test_environment/output/6d_all/test/")

# create plots directory
dir.create("./plots/traits")

# read and quantify timesteps
timesteps.file <- list.files("./species")
timesteps.seq  <- seq(min(parse_number(timesteps.file)),max(parse_number(timesteps.file)))

for(t in timesteps.seq){
  
  # load species data
  species   <- readRDS(paste0("./species/species_t_",t,".rds", sep = ""))
  land      <- readRDS(paste0("./landscapes/landscape_t_",t,".rds", sep = ""))
  abundance <- readRDS(paste0("./abundance/abundance_t_",t,".rds", sep = ""))
  
  # vector of all environmental temperature values
  t_env <- land$environment[,"temp"]
  
  t_opt <- c() # vector for all the t_opt values
  for(sp in seq(1,length(species))){
    
    # add all the unique t_opt values of a species to the global t_opt vector
    t_opt <- c(t_opt,unique(species[[sp]]$traits[,"t_opt"])) 
  }
  
  # plot a nice histogram of temp trait frequency
  t_opt <- data.frame(t_opt)
  trait <- ggplot(t_opt, aes(x=t_opt)) +
              geom_histogram(data = t_opt, bins = 100, aes(fill = "red", x = t_opt)) +
              ggtitle(land$timestep) +
              xlim(c(20,30)) +
              scale_y_continuous(expand = c(0,0)) +
              theme_classic() +
              theme(legend.position = "none")
  
  jpeg(file.path(paste0("./plots/traits/",paste0(sprintf("%04i",t)),".jpg")), width = 1360, height = 960)
  print(trait)
  dev.off()
  
  print(paste("done with ",t))
  
}


# sandbox

t_opt <- data.frame(t_opt, "t_opt")
t_env <- data.frame(t_env, "t_env")
colnames(t_opt) <- c("temp","type")
colnames(t_env) <- c("temp","type")
data  <- rbind(t_opt, t_env)

ggplot(data, aes(x=temp, fill=type)) +
  geom_histogram(bins = 100, alpha=0.5, position="identity") +
  ggtitle(land$timestep) +
  xlim(c(20,30)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(legend.position = "none")

ggplot(data, aes(x=temp, fill=type)) +
  geom_histogram(alpha=0.2, position="identity")

