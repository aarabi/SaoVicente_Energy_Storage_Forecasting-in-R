library(shiny)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output)
  {
  
   output$distPlot <- renderPlot({
     
     data_list<- c("month","week","day","hour")
     var1<-data_list[as.numeric(input$select)]
     
     var<- get(paste(data_list[as.numeric(input$select)],"_data",sep=""))
     var$baseload <- input$bins
     
     #plot a few graphs
     ggplot(data=var, aes(x= period, y=Demand)) +
       geom_line(aes(x=period, y= Demand, color = "Demand"))+
       geom_line(aes(x= period, y= ifelse(Demand-baseload>Del,Del+baseload,Demand), color = "Wind_Delivered"))+
       geom_line(aes(x= period, y= baseload, color = "Baseload"))+
       ylim(c(2000,10000))
   })
   
   output$text1 <- renderText({ 
     paste("You have selected", input$bins)
   })
  
  
  
})