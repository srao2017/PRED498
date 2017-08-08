
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(reshape2)

shinyServer(function(input, output) {
  model_result_file="https://s3.amazonaws.com/aws-website-pzpass-ejkuo/pzpass/PZPass/station_score_by_hour_gbm_test_out.csv"
  P<-read.csv(model_result_file)
  #S<-as.list(unique(P$Station))
  #output$selectUI<-renderUI({selectInput("sel_station","Station",S)})
  
  output$StationDensityPlot <- renderPlot({


   # Day of week normalized to start at Sunday = 0
    dt<-Sys.Date()
    dt<-as.POSIXct(as.character(dt))
    woy<-as.integer(strftime(dt,format="%W"))
    dow<-as.integer(strftime(dt,format="%w"))
    day<-as.integer(input$dow)
    dow<-(dow+day)%%7
    sta<-input$sel_station
    #P$Capacity<-abs(P$cap_per_hr.x/P$count.x)/2
    P$Capacity<-170
    d<-P[(P$Station %in% sta) & (P$Day %in% dow) & (P$Week %in% woy),]
    d<-d[c("Hour","ScoreExt","ScoreEnt","Capacity")]
    
    d <- melt(d, id.vars="Hour")
    
    ggplot(d, aes(Hour,value,col=variable)) + 
      geom_smooth(level=0.0,size=2) + labs(y="Turnstile\nPersons/hour") +
      scale_color_manual(labels = c("Exits", "Entries", "Treshold"), values = c("blue", "red","orange")) +
      theme(legend.position="top") +
      theme(legend.title=element_blank()) +
      theme(axis.text.y = element_text(size=15)) + theme(plot.title=element_text(margin = margin(t=30,b=-20)))
      #+      labs(title=toString(sta))
 
  })

})
