library(shiny)
library(ggplot2)
library(dplyr) 
library(leaflet)
library(RColorBrewer)
#rm(list = ls())
#setwd("/Users/aohana1/Documents/Study/Semester_A_2018/Data Visualization/ShinyApp")
#load('Accidents_Vehic.RData')
#acc.vic.cas <- read.csv("./Accedint_Vehic_Cas.CSV", header = T)


#save.image(file='Accidents_Vehic.RData')

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Accident at the UK 2005-2015"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput("range", "Select the Years Range", min = min(acc.vic.cas$Year), max =max(acc.vic.cas$Year)
                  , step = 1, value=range(acc.vic.cas$Year))
      ,
      selectInput(inputId = "xAxis",
                  label = "Choose X axis:",
                  choices = c("Accident_Severity", "Day", "Month","Casualty_Severity"))
      
    ,
    selectInput(inputId = "yAxis",
                label = "Choose Y axis:",
                choices = c("Day", "Hour", "Day_Num","Age_of_Driver"))

  ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    att1 <- input$xAxis
    att2 <- input$yAxis
    #Year.Min <-input$range[1]
    #Year.Max <-input$range[2]
    
    
    a <- acc.vic.cas#acc

    a<- filter(a,a$Year>=input$range[1] & a$Year<=input$range[2])
    #a$Year<- (a$Year>=Year.Min) 
    #a$Year<- (a$Year<=Year.Max)
    a <- a %>% group_by_(att1,att2 ) %>%
             summarise(n=n()) 
    #a["Norm"] <-(a$n - min(a$n))/(max(a$n) - min(a$n))

    ggplot(data = a, aes_string(x = att1, y = att2)) +
      geom_tile(aes(fill = n)) + #+ scale_fill_gradient(low="yellow", high="red")
    ggtitle("Number of accidents per two chosen variables") +
    labs(fill="Number of accidents") +
      geom_text(aes(label= n),size=3)
   
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
