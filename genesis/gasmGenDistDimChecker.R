


for(i in seq(3,10)) { 
  
  nam <- paste("ti", i, sep = "")
  load(paste("C:/Users/keggint/polybox/sandbox/gasm/WorldMap200-0Ma_multiple_2d/geo_dist_m/geo_dist_m_ti/geo_dist_m_ti_t_",i,".RData", sep = ""))
  x <- geo_dist_m_ti
  assign(nam, dim(x))
  
  nam <- paste("titn", i, sep = "")
  load(paste("C:/Users/keggint/polybox/sandbox/gasm/WorldMap200-0Ma_multiple_2d/geo_dist_m/geo_dist_m_titn/geo_dist_m_titn_t_",i,"_",i-1,".RData", sep = ""))
  x <- geo_dist_m_titn
  assign(nam, dim(x))
  
}


