setwd("C:/Users/keggint/polybox/Zurich/images/gasm/elevation/3dplots/trijur")


directories <- c("C:/Users/keggint/polybox/Zurich/images/gasm/elevation/3dplots/trijur",
                 "C:/Users/keggint/polybox/Zurich/images/gasm/elevation/3dplots/cret",
                 "C:/Users/keggint/polybox/Zurich/images/gasm/elevation/3dplots/ceno")


files     <- rev(list.files("./"))
new_files <- seq(901,length(files)+900)

new_files <- sprintf("%04d", new_files)
new_files <- paste(new_files,".png", sep = "")


name_index     <- 1
for(file in files){
  
  file.rename(file,new_files[name_index])
  name_index <- name_index + 1
}


