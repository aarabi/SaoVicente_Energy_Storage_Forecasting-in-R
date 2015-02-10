library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Set Baseload Value & Curtail Wind Energy!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("bins","Set Baseload Value:",min = 5000,max = 7000,value = 30),
      sliderInput("prob1","Set Point for Night",min = 0,max = 1,value = 0.35),
      sliderInput("prob2","Set Point for Morning",min = 0,max = 1,value = 0.65),
      sliderInput("prob3","Set Point for Evening",min = 0,max = 1,value = 1)
      
      
      
     ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      plotOutput("distPlot"),
      textOutput("text2"),
      textOutput("text3"),
      textOutput("text4"),
      textOutput("text5"),
      textOutput("text6"),
      textOutput("text7")
       )
  )
  
)
)