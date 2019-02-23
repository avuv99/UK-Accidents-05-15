#setwd("/Users/aohana1/Documents/Study/Semester_A_2018/Data Visualization/ShinyApp")
#load('Accidents_Vehic.RData')
#acc.vic.cas<-acc.vic.cas[-c(1)]
#points.location <- acc.vic.cas[c(1:1000),c(2,3,4,17)]

library(shiny)
library(leaflet)
library(tidyr)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, 
             body {width:100%;height:100%}"),
  leafletOutput("map", width= "100%", height = "100%"),
  
  absolutePanel(top = 20, right = 50,
                # sliderInput("range", "Select the Years Range", min = min(acc.vic.cas$Year), max =max(acc.vic.cas$Year)
                #             , step = 1, value=c(2005,2006))
                # ,
                selectInput("Severity", "Choose Accident Severity:",c("Fatal", "Serious","Slight"),selected = "Fatal"
                )
  )
  
  )

server <- function(input, output, session){
  a <-acc.vic.cas
  points.location <- a[c(1:1000),c(2,3,4,17)]
  ## Filter Accident severity
  filtered <- reactive({
    
    #points.location <- filter(points.location,points.location$Year>=input$range[1] & points.location$Year<=input$range[2])
    
    points.location[points.location$Accident_Severity == input$Severity,]
  })
  
  # filtered.Years <- reactive({
  #   quakes[quakes$mag>= input$range[1] & quakes$mag<=input$range[2], ]
  # })
  
  
  ## Put the initial static content along with leaflet the way it should appear initially
  output$map <- renderLeaflet({
    leaflet(data=points.location) %>% 
      addTiles() %>% 
      addMarkers()
    
    
  })
  
  ## Put the dynamic content along with leafletproxy to make updates to the map
  ## On change of range slider input, data subset will change
  ## Leaflet proxy updates the map without re-creating the map from scratch
  ## Clear the previous markers
  ## add new markers based on the new subset data 
  observe(leafletProxy("map", data=points.location) %>%
            clearMarkers() %>%
            addMarkers(lng = ~Longitude, lat = ~Latitude)

  )
  
  ## Below lines of code if you want to use observeEvent with leafletProxy
  observeEvent(input$Severity,
               leafletProxy("map", data=filtered()) %>%
                 clearMarkers() %>%
                 addMarkers()
               )

  
  
}

shinyApp(ui=ui, server=server)