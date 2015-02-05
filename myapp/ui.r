library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Set Baseload Value & Curtail Wind Energy!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins","Set Baseload Value:",min = 5000,max = 7000,value = 30),
    
    selectInput("select", label = h3("Select Set Point"),choices = list("Low,Med,High" = 1, "Med" = 2, "High" = 3),selected = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
  
))