#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(tidyr)
library(readr)
library(shinyWidgets)
library(DT)
library(summarytools)


meteors <- read.csv("meteors.csv", header = TRUE, sep=",")
drop_na(meteors)


#let's remove some nulls, and exclude a few years
meteor_data <- subset(meteors, meteors$year >= 1900)
meteor_data


#it is a very large dataset, so include a dataframe with only the top 30 most frequent meteor-classes:
top_30_classes <- names(sort(table(meteor_data$recclass), decreasing = TRUE)[1:30])
#print(top_classes)
top_30_classes <- meteor_data[meteor_data$recclass %in% top_30_classes, ]


# Define UI
ui <- fluidPage(
  titlePanel("Exploring Meteorites"),
  theme=shinythemes::shinytheme("slate"),
  sidebarLayout(
    #side bar text
    sidebarPanel(h4("This is an exploration of the NASA meteor data
                   which contains the location, mass, composition, and fall year for over 45,000 meteorites
                   that have struck Earth."),
                 p("This visualization utilizes the variables:"),
                 strong("name:"),
                 p("the name of the meteorite"),
                 strong("id:"),
                 p("a unique identifier for the meteorite"),
                 strong("recclass:"),
                 p("the class of the meteorite;
                   one of a large number of classes based on physical and chemical characteristics"),
                 strong("mass:"),
                 p("the mass of the meteorite, in grams"),
                 strong("fall:"),
                 p("whether the meteorite was seen falling, or was discovered after its impact, given as:"),
                 em("Fell:"),
                 p("the meteorite's fall was observed"),
                 em("Found:"),
                 p("the meteorite's fall was not observed"),
                 strong("year:"),
                 p("the year the meteorite fell, or the year it was found (depending on the value of fell"),
                 strong("reclat:"),
                 p("the latitude of the meteorite's landing"),
                 strong("reclong:"),
                 p("the longitude of the meteorite's landing")
                 
    ),
    
    
    mainPanel(
      tabsetPanel(
        
        #tab for the map
        tabPanel("Map",
                 sliderInput("year_choice",
                             "choose a range of years",
                             min=1900,
                             max=2024,
                             value = c(1900, 2024),
                             step=2,
                             sep="",
                             width = "3000px"),
                 leafletOutput("map", width = "100%"),
                 br(),
                 p("Yellow = Fell - Meteorite's fall was observed."),
                 p("Red = Found - Meteorite's fall was not observed.")
                 #slider for choosing a year
        ),
        
        #tab for the class plots
        tabPanel("Meteor Class and Mass", 
                 selectInput("type_choice",
                             "choose a meteor class",
                             choices = c(top_30_classes$recclass),
                             selected = "H6",
                             multiple = TRUE),
                 plotOutput("class_distribution", height="500px", width="100%")),
        
      )    
    )
  )
)

# Define server

server <- function(input, output) {
  
  # year input for the leaflet map
  year <- reactive({
    meteor_data[meteor_data$year >= input$year_choice[1] & meteor_data$year <= input$year_choice[2],]
  })    
  # map output
  output$map <- renderLeaflet({
    pal <- colorFactor(c("yellow", "red"), domain = c("Found", "Fell"))
    leaflet(year()) %>%
      addTiles() %>%
      addProviderTiles("Esri.NatGeoWorldMap") %>%
      setView(0,0, zoom = 2) %>%
      
      #add markers
      addCircleMarkers(
        radius = ~ifelse(fall == "Found", 2, 4),
        color = ~pal(fall),
        stroke = FALSE, 
        fillOpacity = 0.8,
        lat = ~reclat,
        lng = ~reclong,
        popup = paste("Name:", meteor_data$name,
                      "Year:", meteor_data$year,
                      "Weight in grams: ", meteor_data$mass,
                      "Fall-status:", meteor_data$fall))
  })
  
  
  #This plot outputs the mass/meteorclass barchart
  #first filter the data
  output$class_distribution <- renderPlot({
    validate(
      need(input$type_choice, "Choose at least one meteor class"))
    
    filtered_class <- meteor_data[meteor_data$recclass %in% input$type_choice, ]
    
    filtered_class %>%
      #then make the plot
      ggplot(aes(x = recclass, y = mass, fill=recclass)) +
      scale_fill_brewer(palette="Set2") +
      ylab("mass in grams") +
      xlab("meteor class") +
      stat_summary(fun = "median", geom = "bar") + 
      stat_summary(fun ="median", geom="text", 
                   aes(label = round(after_stat(y), 0)), 
                   vjust = -0.2, 
                   size=5, 
                   colour = "black")
  })
  
  
  
  
  
  
}


# Run the application
shinyApp(ui, server)

