#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
require (DT)

shinyUI(fluidPage( 

  selectInput("selectId","Select Station",
              
               c( "103 ST","116 ST","125 ST","14 ST-UNION SQ","145 ST","42 ST-BRYANT PK","42 ST-PORT AUTH","49 ST","50 ST","7 AV","72 ST-2 AVE","8 AV","86 ST","86 ST-2 AVE",    
               "ATL AV-BARCLAY","WAY-LAFAYETTE","BEDFORD AV","CROWN HTS-UTICA","DELANCEY/ESSEX","FLUSHING-MAIN","FOREST HILLS 71","GRAND ST","GRD CNTRL-42 ST","JAMAICA CENTER",
               "JAY ST-METROTEC","JKSN HT-ROOSVLT","LEXINGTON AV/53","SUTPHIN-ARCHER","W 4 ST-WASH SQ")),

  
  mainPanel(
    textOutput("direction"),
    textOutput("day_date"),
    #tableOutput("dataset")
    DT::dataTableOutput("dataset")
  ) 
) 

)
