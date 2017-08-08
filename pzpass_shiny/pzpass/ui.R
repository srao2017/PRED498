
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  headerPanel("", windowTitle="Predicted Turnstile Traffic"),
  # Sidebar with a slider input for number of bins
  #sidebarPanel(
    selectInput("sel_station",
                "Prediction for Station",
                c("125 ST",
  "14 ST",
  "14 ST-UNION SQ",
  "161/YANKEE STAD",
  "23 ST",
  "28 ST",
  "34 ST-HERALD SQ",
  "34 ST-PENN STA",
  "42 ST-PORT AUTH",
  "47-50 STS ROCK",
  "50 ST",
  "59 ST",
  "59 ST COLUMBUS",
  "7 AV",
  "86 ST",
  "96 ST",
  "ATL AV-BARCLAY",
  "BOWLING GREEN",
  "CANAL ST",
  "CHAMBERS ST",
  "CHURCH AV",
  "CORTLANDT ST",
  "FULTON ST",
  "GRD CNTRL-42 ST",
  "JAY ST-METROTEC",
  "KINGS HWY",
  "LEXINGTON AV/53",
  "METS-WILLETS PT",
  "TIMES SQ-42 ST",
  "WALL ST"
  )),
    #htmlOutput("selectUI"),
    radioButtons("dow",label="",choices=list("Today"=0,"Tomorrow"=1),selected=0)
#    )
  ,

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("StationDensityPlot",height="75%")
    )
  ))

