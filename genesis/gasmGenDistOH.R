##################################################
## Description: Creates the Input Data for world
## with certain resolution from all_geo_hab file
## Can be called from the cluster or directly though a
## interactive session
##
## Date: 2018-03-16 15:26:39
##
## Author: Oskar Hagen (oskar@hagen.bio)
## Edits: Thomas Keggin (thomaskeggin@hotmail.com)
##################################################


### Variables ####
InputDir <- "C:/Users/keggint/polybox/sandbox/gasm"
# should refer to the finiest resolution of the air surface temperature maps

OutputDir <- "C:/Users/keggint/polybox/sandbox/gasm/WorldMap200-0Ma_multiple_2d" 
# here we just add the OutputDir... degree info will be added according to degree information

habitable_hab <- ">=-99999" # do not add more nines here! there is still a comparison to be done...

crossing_water <- 0.5

NAtolerance <- 1.5 # (division of the proportion of NA's )

aggregatefactor <- 2

# cosmetics
# getting equal colour to all gradients..
maxTemp <- 40
minTemp <- -40
breaks <- seq(minTemp, maxTemp, by=1)
ramp=colorRampPalette(c("white","white","purple","purple", "blue", "blue", "blue", "cyan", "cyan", "cyan", "lightgreen", "yellow", "orange" ,"red", "darkred", "darkred", "darkred", "darkred", "chocolate4"))(length(breaks)-1)
colNA="#1F407A" # ETH BLUE


#### Libraries loading ####
lib <- c("raster","matrixStats","sp","gdistance","geosphere","parallel") # "fpc"
sapply(lib,library, character.only=TRUE)

#### Functions ####
agg.func <- function(x, ...){
  # function for aggregation...
  if (sum(is.na(x))>(aggregatefactor*aggregatefactor)/NAtolerance ){
    return(NA)
  } else {
    return(min(x, na.rm=T))
  }
}

select.habitable_hab <- function(what, inverse=FALSE, condition=habitable_hab){
  #what is a string!
  selected <- eval(parse(text=paste(what, condition )))
  
  if (inverse==TRUE){
    selected <- !selected
  }
  
  return(selected)
}

#### end function ####

#### consistency checks ####

#get the world name from the OutputDir and extract the degree info
path <- strsplit(OutputDir, "/")[[1]]
path <- path[(length(path))] 
degree <- as.integer(gsub(".*_(\\d+)d","\\1", path))
if (degree!=aggregatefactor){
  stop("Please check the aggregate factor and the degree informed...")
}
# this reads the folder name where degree information is stored... Seems a bit gnarly. TK

# create dir if not existing....
dir.create(file.path(OutputDir, "geo_dist_m"))
dir.create(file.path(OutputDir, "geo_dist_m", "geo_dist_m_ti"))
dir.create(file.path(OutputDir, "geo_dist_m", "geo_dist_m_titn"))


# create plot dir
if (!file.exists(file.path(OutputDir, "plot"))) {
  dir.create(file.path(OutputDir, "plot"))
}
#create used_scripts
if (!file.exists(file.path(OutputDir, "used_scripts"))) {
  dir.create(file.path(OutputDir, "used_scripts"))
}


#list files
load(file.path(InputDir, "all_geo_hab", "all_geo_hab.Rdata"))
all_geo_hab <- all_geo_hab$temp
latest_frame <- ncol(all_geo_hab)
if(interactive()){
  t_start <- 3
  t_end <- latest_frame #until the present
} else{
  args <- commandArgs(trailingOnly = TRUE)
  t_start <- as.numeric(args [1])
  t_end <- t_start #only the informed timestep
}
#this is the number of frame! 1201 correspondes to 200.00Ma

for (i in t_start:t_end){ #start big loop
  
  #-------------#
  ##### Ti   #### 
  #-------------#
  #computation of internal distances
  print("on internal distances")
  
  cat("i=ab",i,"\n")
  
  formatedyear <- names(all_geo_hab[i])
  
  cat("Working on year", formatedyear, "\n")
  
  #Step_1 <- rasterFromXYZ(all_geo_hab[,c(1,2,i+2)])
  Step_1 <- rasterFromXYZ(all_geo_hab[,c(1,2,i)])
  print(paste(formatedyear,"[loaded]"))
  
  #plot
  jpeg(file.path(OutputDir, "plot", paste0(formatedyear,".jpg") ), width = 680, height = 480)
  par(mar=c(0,0,0.2,0.5)+0.2, oma=c(0,0,0,0))
  plot(Step_1, colNA=colNA,
       col=ramp, breaks=breaks,legend.width=1,  legend.shrink=0.64 , axes=FALSE,box=FALSE, xlab="", ylab="",
       axis.args=list(at=seq(minTemp, maxTemp, 10),
                      labels=seq(minTemp, maxTemp, 10), 
                      cex.axis=2, lwd=0, line=0.2))
  title(paste("GaSM world @", formatedyear), line=-2.5, cex.main=3)
  dev.off()
  print("plotting done!")
  
  # replacing NA`s by negative values`
  Step_1[is.na(Step_1)] <- -999999999
  
  
  hab_180<-Step_1
  #all earth is suitable!
  
  hab_180[select.habitable_hab("hab_180")] <- 1
  # if we have values use this....
  #### ATTENTION!!!! HERE WE DEFINE A COST FOR CROSSING!!!!!
  hab_180[select.habitable_hab("hab_180", inverse = T)] <- crossing_water 
  
  # if we have NA`s use this
  # hab_180[is.na(hab_180)] <- 0
  
  #plot(hab_180)
  
  # create a transition object (cost)
  trans_hab_180<-transition(hab_180, transitionFunction=min, directions=16)
  #go arround the world
  points_180.df<-(as.data.frame(Step_1, xy=T))
  points_180_habitable.df<-points_180.df[select.habitable_hab("points_180.df[,3]"), 1:2]
  
  # calculate the least-cost distance between points.
  dist1_180<-(costDistance(trans_hab_180, cbind(points_180_habitable.df[,1],points_180_habitable.df[,2]), cbind(points_180_habitable.df[,1],points_180_habitable.df[,2])))
  #make tables with the same order of coordinates to do pmin properly and connect the world
  points_360.df<-points_180.df
  points_360.df$x[points_360.df$x<0]<-points_360.df$x[points_360.df$x<0]+360
  Step_2<-rasterFromXYZ(points_360.df)
  
  hab_360<-Step_2
  hab_360[select.habitable_hab("hab_360")]<-1
  hab_360[select.habitable_hab("hab_360", inverse = T)]<-crossing_water
  #running transition
  trans_hab_360<-transition(hab_360, transitionFunction=min, directions=16)
  
  points_360_habitable.df<-points_180_habitable.df
  points_360_habitable.df$x[points_360_habitable.df$x<0]<-points_360_habitable.df$x[points_360_habitable.df$x<0]+360
  
  dist1_360<-(costDistance(trans_hab_360, cbind(points_360_habitable.df[,1],points_360_habitable.df[,2]), cbind(points_360_habitable.df[,1],points_360_habitable.df[,2])))
  geo_dist_m_ti<-pmin(dist1_180,dist1_360)
  
  save(geo_dist_m_ti,file=file.path(OutputDir,"geo_dist_m", "geo_dist_m_ti" , paste0("geo_dist_m_ti_t_",i,".RData",sep="")) )
  cat("Done with", formatedyear, "\n")
  
  
  
  #-------------#
  ##### TiTn   #### 
  #-------------#
  #Computation of external distances
  
  if (i!=1) {
    print("on external distances")
    cat("i=ab",i,"\n")

    formatedyear <- names(all_geo_hab[i])
    
    cat("Working on year", formatedyear, "\n")
    
    Step_1 <- rasterFromXYZ(all_geo_hab[,c(1,2,i)])
    Step_2 <- rasterFromXYZ(all_geo_hab[,c(1,2,i-1)])
    
    if (aggregatefactor!=1){
      Step_1 <- aggregate(Step_1, aggregatefactor, fun=agg.func)
      Step_2 <- aggregate(Step_2, aggregatefactor, fun=agg.func)
    }
    
    # replacing NA`s with negative values`
    Step_1[is.na(Step_1)] <- -999999999
    Step_2[is.na(Step_2)] <- -999999999
    
    
    hab_180<-Step_1
    #all earth is suitable!
    hab_180[select.habitable_hab("hab_180")] <- 1
    hab_180[select.habitable_hab("hab_180", inverse = T)] <- crossing_water
    
    trans_hab_180<-transition(hab_180, transitionFunction=min, directions=16)
    
    points_180_step1.df<-(as.data.frame(Step_1, xy=T))
    points_180_habitable_step1.df<-points_180_step1.df[
      select.habitable_hab("points_180_step1.df[,3]")
      ,1:2]
    
    points_180_step2.df<-(as.data.frame(Step_2, xy=T))
    points_180_habitable_step2.df<-points_180_step2.df[
      select.habitable_hab("points_180_step2.df[,3]")
      ,1:2]
    
    dist2_180<-(costDistance(trans_hab_180, cbind(points_180_habitable_step1.df[,1],points_180_habitable_step1.df[,2]), 
                             cbind(points_180_habitable_step2.df[,1],points_180_habitable_step2.df[,2])))
    
    points_360_step1.df<-points_180_step1.df
    points_360_step1.df$x[points_360_step1.df$x<0]<-points_360_step1.df$x[points_360_step1.df$x<0]+360
    
    Step1_360<-rasterFromXYZ(points_360_step1.df)
    
    hab_360<-Step1_360
    #hab_360<-Step_2
    hab_360[select.habitable_hab("hab_360")]<-1
    hab_360[select.habitable_hab("hab_360", inverse = T )]<-crossing_water
    
    trans_hab_360<-transition(hab_360, transitionFunction=min, directions=16)
    
    points_360_habitable_step1.df<-points_180_habitable_step1.df
    points_360_habitable_step1.df$x[points_360_habitable_step1.df$x<0]<-points_360_habitable_step1.df$x[points_360_habitable_step1.df$x<0]+360
    
    points_360_habitable_step2.df<-points_180_habitable_step2.df
    points_360_habitable_step2.df$x[points_360_habitable_step2.df$x<0]<-points_360_habitable_step2.df$x[points_360_habitable_step2.df$x<0]+360
    
    dist2_360<-(costDistance(trans_hab_360, cbind(points_360_habitable_step1.df[,1],points_360_habitable_step1.df[,2]), 
                             cbind(points_360_habitable_step2.df[,1],points_360_habitable_step2.df[,2])))
    
    geo_dist_m_titn<-pmin(dist2_180,dist2_360)
    
    
    
    save(geo_dist_m_titn, file=file.path(OutputDir, "geo_dist_m", "geo_dist_m_titn", paste("geo_dist_m_titn_t_",i,"_",(i-1),".RData", sep="")))
    cat("Done with", formatedyear, "\n")
  }
} #end big loop

