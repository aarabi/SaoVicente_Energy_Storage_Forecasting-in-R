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
        
          ggplot(data=var, aes(x= period, y=Demand)) +
         geom_line(aes(x=period, y= Demand, color = "Demand"))+
         geom_line(aes(x= period, y= ifelse(Demand-baseload>Del,Del+baseload,Demand), color = "Wind_Delivered"))+
         geom_line(aes(x= period, y= baseload, color = "Baseload"))+
      geom_line(aes(x= period, y= storage_ten, color = "Storage"))+
                ylim(c(5000,10000))

       
    
      
   })
   
   
  output$distPlot2 <- renderPlot({
    data_list<- c("month","week","day","hour")
    var1<-data_list[as.numeric(input$select)]
    
    
    var<- get(paste(data_list[as.numeric(input$select)],"_data",sep=""))
    var$baseload <- input$bins
    if(input$radio==1&&input$select==1)
    {
      var$curtail<-c(1500,1500,1500,2500,2500,2500,500,0,500,1500,500,1500)
      
     
      
      ggplot(data=var, aes(x= period, y=Demand)) +
        geom_line(aes(x=period, y= Demand, color = "Demand"))+
        geom_line(aes(x= period, y= ifelse(Demand-baseload>Del,Del+baseload,Demand), color = "Wind_Delivered"))+
        geom_line(aes(x= period, y= baseload, color = "Baseload"))+
        geom_point(aes(x=period, y= curtail+baseload, color="SetPoint"))+
        ylim(c(5000,10000))
      
    }
    else if(input$radio==2&&input$select==2)
    {
      
      var$curtail<-0
      a<-as.data.frame(aggregate(total[c("Del")], by = total[c("week.y")], FUN=min))
      
      a<- unlist(a[-1])
      
      var$curtail<-a
      var$curtail<- (var$Del+var$curtail)/2
      
      ggplot(data=var, aes(x= period, y=Demand)) +
        geom_line(aes(x=period, y= Demand, color = "Demand"))+
        geom_line(aes(x= period, y= ifelse(Demand-baseload>Del,Del+baseload,Demand), color = "Wind_Delivered"))+
        geom_line(aes(x= period, y= baseload, color = "Baseload"))+
        geom_point(aes(x=period, y= curtail+baseload, color="SetPoint"))+
        ylim(c(5000,10000))
      
    }
    
  })
  
  
})