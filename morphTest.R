setwd("R:/data/raw_reefish_morphometrics/reefish_WIO/TPS/Seychelles/Acanthurus_nigrofuscus_SC")

library("geomorph")

data <- readland.tps("./Acanthurus_nigrofuscus_SC.TPS", specID = "imageID")
