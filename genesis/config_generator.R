# set session ####

HOME <- "C:/Users/thoma/OneDrive/Documents/PhD/genesis/configs/1_dispersal"
setwd(HOME)

library(randtoolbox)
library(tidyverse)

# set the number of runs
n <- 10

# convert the sobol numbers to within defined parameter space
linMap <- function(x, from, to) {(x - min(x)) / max(x - min(x)) * (to - from) + from}


# generate parameter values ####

# create parameter value table
params_table                  <- data.frame(sobol(n, 2, init = T))
params_table                  <- cbind(1:dim(params_table)[1], params_table)
colnames(params_table)        <- c("run_id",
                                   #"t_opt",
                                   "dispRange",
                                   "speciation"#,
                                   #"adaptation",
                                   #"abdDeath"
                                   )

# seed
params_table$seed             <- 1989

# start
params_table$start            <- 390
# 390*0.166 ~ 65 mya

# intial abundance
params_table$initialAbundance <- 100

# t_opt
##params_table[,"t_opt"]        <- round(linMap(params_table[,"t_opt"], from=17, to=30),0)

# dispersal range
params_table[,"dispRange"]    <- round(linMap(params_table[,"dispRange"], from=1, to=1000),0)
# varies from <20 to <15000 km / generation depending on species (Green et al. 2015).
# each step spans multiple generations.

# speciation
params_table[,"speciation"]   <- round(linMap(params_table[,"speciation"], from=12, to=600),0)
# 0.01 - 0.5 speciations/my (Rabosky, 2020)
# 2 - 100 my
# 12 - 600 time steps

# adaptation
##params_table[,"adaptation"]   <- round(linMap(params_table[,"adaptation"], from=0.1, to=0.9),2)

# abundance death
##params_table[,"abdDeath"]     <- round(linMap(params_table[,"abdDeath"], from=10, to=95),0)


# Write table ####

write_csv(params_table, "./config_parameters.csv")


# Generate config files ####

for(i in 1:nrow(params_table)){ 

  #setwd(HOME)
  
  params <- params_table[i,]
  
  config_i <- readLines('template.R')
  config_i <- gsub('params\\$initialAbundance', params$initialAbundance, config_i)
  config_i <- gsub('params\\$speciation', params$speciation, config_i)
  #config_i <- gsub('params\\$t_opt', params$t_opt, config_i)
  config_i <- gsub('params\\$seed', params$seed, config_i)
  config_i <- gsub('params\\$start', params$start, config_i)
  config_i <- gsub('params\\$dispRange', params$dispRange, config_i)
  #config_i <- gsub('params\\$adaptation', params$adaptation, config_i)
  #config_i <- gsub('params\\$abdDeath', params$abdDeath, config_i)
  
  #setwd("./1_d_s")
  writeLines(config_i, paste0(i, '.R'))

}

#create bat file
run_head <- '@runAsMultiple,@Node_NODE14'
script_name <- 'run_genesis.R'
r_version <- "/_shared/R3.6.1/r-with-tools.bat"
config_dir <- paste0("-c ./config/1_dispersal/", 1:dim(params_table)[1], '.R')
input_dir <- '-i ../input/1d_2000m_17c/'
output_dir <- '-o output/1d_2000m_17c/'
other_par <- '-s all -v'

run_body <- paste(r_version, script_name, config_dir, input_dir, output_dir, other_par)
write(c(run_head, run_body), file='1_dispersal.bat')
