library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Set Baseload Value & Curtail Wind Energy!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("select", label = h3("Select Graph"),choices = list("Monthly data" = 1, "Weekly data" = 2, "Daily data" = 3,"Hourly data"=4),selected = 1),
      
      sliderInput("bins","Set Baseload Value:",min = 5000,max = 7000,value = 30)
      
     ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      plotOutput("distPlot"),
      textOutput("text1")
      
     
    )
  )
  
))