#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
require(DT)
require(dplyr)
library(lubridate)


shinyServer(function(input, output) {

  setwd("/srv/shiny-server/pzpass/ts_pred")
  model_result_file="https://s3.amazonaws.com/aws-website-pzpass-ejkuo/pzpass/PZPass/APPInput.csv"
  dataset<-read.csv(model_result_file, header = T)

  # read the pre computed values from this table. Note this table is processed by
  # PrepareResultsTabulation.R which updates the Congestion column
  #dataset<-read.csv("Results tabulation.csv",header = T)
  dataset$X<-NULL
  dataset$Validation<-NULL

  dataset$RushTrending<-NULL       
  #extract hour only
  dataset$Hour<-as.integer(strftime(dataset$Time,format="%H"))
  Weekday<-weekdays(as.Date(dataset$Time))
  #extract week day only as a shortened word
  dataset$Day<-weekdays(as.Date(dataset$Time))
  dataset$WeekDayNum<-wday(as.Date(dataset$Time))
  
  # extract date only
  Date<-strftime(dataset$Time,format="%x")
  WeekDayNum<-wday(Sys.Date())
  # to show icons
  dataset$RushTrending<-ifelse(dataset$Foot.Traffic == "PreCongestion",
                               '<img src="thumbs_dn.png" height=30></img>',
                               '<img src="thumbs_up.png" height=30></img>')

  output$direction<-renderText({
      day_date<-paste0("Predictions based on data up to:")
  })
  
  output$day_date<-renderText({
    direction<-paste0(Weekday[1],", ", Date[1])
  })
  
  #output$dataset<-renderTable({
  #  dataset<-dataset[dataset$Station==input$selectId,]
  #  dataset<-dataset[,c("Hour","Congestion")]
  #})
  
  output$dataset<-DT::renderDataTable({

       # Select data for remainder of the week
       dataset<-dataset[dataset$WeekDayNum >= WeekDayNum & dataset$WeekDayNum <= WeekDayNum+1,]
       # subset for selected station
       dataset<-dataset[dataset$Station==input$selectId,]
       #subset for columns we need
       dataset<-dataset[,c("Day", "Hour", "RushTrending")]
       DT::datatable(dataset,
                     # Hide the row number column 0
                     options=list(columnDefs=list(list(visible=FALSE,targets=c(0)))), 
                     #need this to display thumbs icons as img tags
                     escape=FALSE)
  })
})










