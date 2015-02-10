library(shiny)
library(ggplot2)
# Define server logic required to draw a histogram
shinyServer(function(input, output)
  {
 
  output$distPlot <- renderPlot({
    
    
    
    
    total$baseload <- input$bins
    prob1<-input$prob1
    prob2<-input$prob2
    prob3<-input$prob3
    total$setpoint_daily<-0
    #setpoints
    for(i in 1:nrow(total))
    {
      if(total$hour.y[i]==4)
      {
        
        total$setpoint_daily[(i-5):i]<-quantile(total$Del[0:5], prob1)
        
      }
      else if(total$hour.y[i]==11)
      {
        
        total$setpoint_daily[(i-7):i]<-quantile(total$Del[0:5],prob2)
      }
      else if(total$hour.y[i]==21)
      {
        
        total$setpoint_daily[(i-10):i]<-quantile(total$Del[0:5],prob3)
      }
      else if(total$hour.y[i]==23)
      {
        
        total$setpoint_daily[(i-2):i]<-quantile(total$Del[0:5], prob2)
      }
    }
    
    #storage with daily set points
    total$p<-0
    
    total$p<- c((total$setpoint_daily+total$baseload)-total$Demand)
    total$storage_d_ten<-0
    stor_ten=10000
    
    for(i in 2:nrow(total))
    {
      if(total$p[i]>0)
      {
        if((total$storage_d_ten[i-1]+total$p[i])<stor_ten)
        {
          total$storage_d_ten[i]<-total$storage_d_ten[i-1]+total$p[i]
        }
        
        else
        {
          total$storage_d_ten[i]<-total$p[i]
        }
      }
      else
      {
        if(total$storage_d_ten[i-1]>0)
        {
          if((total$p[i]+total$storage_d_ten[i-1])<0)
          {
            total$storage_d_ten[i]<-0
          }
          else
          {
            total$storage_d_ten[i]<-total$storage_d_ten[i-1]+total$p[i]
          }
        }
        
      }
    }
    
    
    a<-sum(total$storage_d_ten)
    a<- round(a*0.001,digits=2)
    a<-paste("Saved Energy (Mwh):",a)
    b<- total$Demand-(total$baseload+total$setpoint_daily)
    b<- round(sum(b)*0.001,digits=2)
    a<-paste(a,"                        Unmet Demand(Mwh):")
    a<-paste(a,b)
    ggplot(data=total, aes(x= merged, y=Demand)) +
      geom_point(aes(x=merged, y= Demand, color = "Demand"))+
      geom_line(aes(x= merged, y= baseload+setpoint_daily, color = "Curtailed Wind Delivered"))+
      geom_line(aes(x= merged, y= baseload, color = "Baseload"))+
      geom_line(aes(x= merged, y= baseload+Del, color = "Uncurtailed Wind Delivered"))+
      ggtitle(a)+
      theme(plot.title = element_text(lineheight=.8, face="bold"))
    
     
    
  }) 
 
})