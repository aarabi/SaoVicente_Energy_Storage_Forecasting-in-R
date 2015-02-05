library(shiny)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output)
  {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot(
    {
   
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    week_data$baseload<- input$bins
    
   
    if(input$select=="1")
    {
      week_data$curtail[0:9]<- 1500
      week_data$curtail[10:16]<- 3000
      week_data$curtail[17:23]<- 2000
    }
    else if(input$select=="2")
    {
      week_data$curtail<- 2000
    }
    else
    {
      week_data$curtail<- week_data$Del
    }
    
    print(week_data$curtail)
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    #plot a few graphs
    ggplot(data=week_data, aes(x= 0:23, y=Demand, group=1, factor(hour) )) +
      geom_line(aes(x= 0:23, y= Demand, color = "Demand"))+
      geom_line(aes(x= 0:23, y= ifelse(Demand-baseload>Del,Del+baseload,Demand), color = "Del"))+
      geom_line(aes(x= 0:23, y= baseload, color = "Baseload"))+
      geom_point(aes(x= 0:23, y= ifelse(Demand-baseload>curtail,curtail+baseload,Demand), color = "Baseload"))+
      ylim(1000, 10000)
    
  }
  )
})