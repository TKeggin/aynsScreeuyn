
# run this as an add-on to the .rmd file

library(gridExtra)

parameters <- c("t_opt",
                "dispRange",
                "speciation",
                "mutationRate",
                "n_width")
plots <- list()

conts <- c("continuity")

plotDataAll <- pivot_longer(data, cols = all_of(conts))

p <- 1
for(c in 1:length(conts)){
  
  plotData <- plotDataAll %>% filter(name == conts[c])
  
  for(i in 1:length(parameters)){
    
    plotData[,"parameter"] <- plotData[,parameters[i]]
    
    plot <- ggplot(plotData) +
      geom_point(aes(x=parameter,
                     y=value)) +
      #ylab(conts[c]) +
      xlab(parameters[i]) +
      theme_classic()
    
    plots[[p]] <- plot
    p <- p + 1
  }
}

grid.arrange(grobs = plots, ncol=2, as.table = FALSE)
