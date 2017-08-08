
library(shiny)
#library(forecast)
#library(dplyr)
library(ggplot2)
dataset<-read.csv("Results tabulation.csv",header = T)
dataset$X<-NULL
dataset$Validation<-NULL

  
dataset <- dataset %>% group_by(Station) %>% arrange(Time,Forecast,Actual) %>%
  mutate(RushTrending=c(0,diff(Forecast)))
#dataset$Congestion<-ifelse(dataset$RushTrending=="NA","NA",(ifelse(dataset$RushTrending<0,"DOWN","UP")))
 
dataset$Congestion<-ifelse(dataset$RushTrending<=0,"DOWN","UP")


 
dataset$RushTrending<-NULL       

library(data.table)


names(dataset)<-c("Station","TIME,  (Day-hh-mm-ss)","Predicted Congestion, Commuters/hr","Congestion, 24 hrs earlier","Congestion Trend")
  

  
ui <- fluidPage( 
  
 
  ### the rest of your code

  
         titlePanel(div(img(src="Picture2.png",height = 175, width = 400, align = "right"), "Hello, NY-MTA Commuters!, Our Goal is to Smoothen Your Commute",style = "color:blue")), 
                        
                      
      # selectInput("selectId", label =h2("Select Station"), 
      #             choices = list("103 ST" = 1, "116 ST" = 2, "125 ST" = 3,"14 ST-UNION SQ" = 4,"145 ST" = 5,"42 ST-BRYANT PK" = 6,
      #                          "42 ST-PORT AUTH"=7, "49 St"=8, "50 ST"=9, "7 AV"=10),selected = 2),
                
               # choices=list(unique(dataset$Station)),
      
      selectInput("selectId","Select Station",
                  
                  # c( "103 ST","116 ST","125 ST","14 ST-UNION SQ","145 ST","42 ST-BRYANT PK","42 ST-PORT AUTH","49 ST","50 ST","7 AV","72 ST-2 AVE","8 AV","86 ST","86 ST-2 AVE",    
                  # "ATL AV-BARCLAY","WAY-LAFAYETTE","BEDFORD AV","CROWN HTS-UTICA","DELANCEY/ESSEX","FLUSHING-MAIN","FOREST HILLS 71","GRAND ST","GRD CNTRL-42 ST","JAMAICA CENTER",
                  # "JAY ST-METROTEC","JKSN HT-ROOSVLT","LEXINGTON AV/53","SUTPHIN-ARCHER","W 4 ST-WASH SQ")),
                  
      
                  names(table(dataset$Station))),
                    
                    
                    
                    # "34 ST-PENN STA", "FULTON ST","GRD CNTRL-42 ST","86 ST", 
                    # "TIMES SQ-42 ST", "14 ST-UNION SQ","34 ST-HERALD SQ")),
      #radioButtons("dow",label="",choices=list("Today"=0,"Tomorrow"=1),selected=0)
      #    )
      
      
      
      
      
      

      
      mainPanel(
         #tabsetPanel(
          #tabPanel("Table of Rush Predictions at Station",
                   strong("Table of Rush Predictions at Station"),
                   tableOutput("dataset"),
                   #tableOutput("table")
                   textOutput("text1"),
                   textOutput("text2"),
                   plotOutput("myPlot")
            
                   
                   
                   
                          
          ) 
    
      
                
      
      
      ) 
      


server <- function(input, output) {
  

  output$text1 <- renderText({ 
    paste("You have selected", input$selectId)
  })
  

  

  
  output$dataset<-renderTable({
   # statefilter<-subset(dataset,dataset$Station==input$select)
    
   dataset<-dataset[dataset$Station==input$selectId,]
   #dataset<-dataset[dataset$Station=='116 ST',]
    
   
  })
 
  output$myPlot<-renderPlot({
  

    file<-as.data.frame(dataset[dataset$Station==input$selectId,])
    
    #file<-dataset[dataset$Station=='103 ST',]

    
    #ggplot(data = as.data.frame(dataset[dataset$Station=='103 ST',]),mapping = aes(x=Time, y = Forecast)) + geom_point() +geom_line()
    
    plot(as.POSIXct(as.character(file$Time)),file$Forecast, type='b',xlim=c(file$Time[1],file$Time[6]))
    #   xlim=c(as.POSIXct(as.character(file$Time[1]),as.POSIXct(as.character(file$Time[nrow(file)])))))
    
    
    
     })

}

shinyApp(ui = ui, server = server)






  
  
