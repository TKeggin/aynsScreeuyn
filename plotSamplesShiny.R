# set session ####

library("shinyWidgets")
library("tidyverse")
library("shiny")
library("readxl")
library("ggmap")
library("maptools")
library("maps")
library("ggrepel")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")

setwd("Z:/LE_projects/megafauna/data/sandbox")

# load data ####

data <- read_excel("Browse samples.xlsx", sheet = 1) %>% distinct(Sample_ID, .keep_all = TRUE)


data$extracted <- !is.na(data$Extraction_plate)
data$rad       <- !is.na(data$RAD_library)

# make a new column with full Genus species name
data$gen_sp <- paste(data$Genus,data$species, sep = " ")

# user interface ####

ui <- fluidPage(title = "Sampling",
                
                fluidRow(height = "100px",
                    column(4,
                           selectInput(inputId = "species",
                                          label = "Choose a species:",
                                          choices = sort(data$gen_sp),
                                          selected = "Chromis weberi",
                                          multiple = TRUE,
                                          width = "100%")),
                    
                    column(2,
                           selectInput(inputId = "ext",
                                          label = "Extracted?",
                                       selected = list("not extracted","extracted"),
                                          choices = list("not extracted" = FALSE,
                                                       "extracted" = TRUE),
                                          multiple = TRUE),
                           selectInput(inputId = "rad",
                                          label = "RADseq?",
                                       selected = list("not sequenced","sequenced"),
                                          choices = list("not sequenced" = FALSE,
                                                        "sequenced" = TRUE),
                                          multiple = TRUE)),
                    column(2,
                           strong("To zoom in:"),
                           p("Drag a box around the area, then double click."),
                           strong("To zoom out:"),
                           p("Double click."))
                        ), # fluidRow 
                
                tabsetPanel(type = "tabs",
                            tabPanel("Map",
                                     plotOutput(outputId = "distPlot",
                                                height = "500px",
                                                dblclick = "distPlot_dblclick",
                                                brush = brushOpts(
                                                  id = "distPlot_brush",
                                                  resetOnNew = TRUE))),
                            tabPanel("Summary",
                                     tableOutput("summary")))
                ) # fluidPage

# server ####

server <- function(input, output) {
  
  
  # reactive ranges for selecting areas
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # double click brush zooming
  observeEvent(input$distPlot_dblclick, {
    brush <- input$distPlot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  # plot the distribution map
  output$distPlot <- renderPlot({
    
    # filter for selected species, extracted, and rad
    plotData <- filter(data, gen_sp %in% input$species) %>%
      filter(extracted %in% input$ext) %>%
      filter(rad %in% input$rad)
    
    # aggregate sites to give central point for labels
    agg.data <- aggregate(cbind(Longitude,Latitude) ~ Sample_site, data = plotData, mean)
    
    # number of samples per site
    sampleSize <- count(plotData, Sample_site)
    agg.data$size <- sampleSize$n
    
    # set the world map
    world <- ne_coastline(scale = "small", returnclass = "sf")
    # create the plot
    ggplot() +
      geom_sf(data = world) +
      geom_point(data = plotData,
                 aes(x = Longitude,
                     y = Latitude,
                     colour = Sample_site),
                 size = 3) +
      geom_text_repel(data = agg.data,
                      box.padding = 0.7,
                      aes(x = Longitude,
                          y = Latitude,
                          label= paste(Sample_site,"\n",size,sep=" ")),
                      size=6) +
      coord_sf(xlim = ranges$x,
                           ylim = ranges$y,
                           expand = TRUE) +
      theme_void() +
      theme(legend.position = "none", panel.grid = element_line(colour = "transparent"))
    
  }) # renderPlot
  
  # Summary table (from megafaunaSamplingSummary.R)
  
  output$summary <- renderTable({
    
    siteSummary <- group_by(data, gen_sp) %>% 
                    filter(gen_sp %in% input$species) %>%
                    filter(extracted %in% input$ext) %>%
                    filter(rad %in% input$rad) %>%
                    count(Sample_site) %>%
                    spread(Sample_site,n)
  siteSummary[is.na(siteSummary)] <- 0
  siteSummary
  }, digits = 0)
  
} # server

# run app ####

shinyApp(ui, server)