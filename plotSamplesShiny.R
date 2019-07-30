# set session ####

library("shiny")
library("readxl")
library("ggmap")
library("maptools")
library("maps")
library("ggrepel")

setwd("Z:/LE_projects/megafauna/data/sandbox")

# load data ####

# load data
data <- read_excel("Browse samples.xlsx", sheet = 1)

# make a new column with full Genus species name
data$gen_sp <- paste(data$genus,data$species, sep = " ")

# user interface ####

ui <- fillPage( title = "Sampling Sites",
    
    selectInput(inputId = "species",
                label = "Choose a species:",
                choices = sort(data$gen_sp),
                selected = sort(data$gen_sp)[1]),
    
    plotOutput(outputId = "distPlot", height = "100%")

) # fillPage

# server ####

server <- function(input, output) {
  
  # plot the distribution map
  output$distPlot <- renderPlot({
    
    # filter for selected species
    plotData <- filter(data, gen_sp == input$species)
    
    # aggregate sites to give central point for labels
    agg.data <- aggregate(cbind(longitude,latitude) ~ sample_site, data = plotData, mean)
    
    # set the world map
    mapWorld <- borders("world", colour="lightgrey", fill="lightgrey")
    
    # create the plot
    ggplot(plotData) +
      mapWorld +
      geom_point(aes(x = longitude, y = latitude, colour = sample_site), size = 3) +
      geom_text_repel(data = agg.data, box.padding = 0.7, aes(x = longitude, y = latitude, label=sample_site), size=7) +
      coord_fixed() +
      theme_void() +
      theme(legend.position = "none")
    
    
  }) # renderPlot
  
} # server

# run app ####

shinyApp(ui, server)


