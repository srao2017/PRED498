library(dplyr)

setwd("/srv/shiny-server/pzpass/ts_pred")

dataset<-read.csv("Results tabulation.csv",header = T)
dataset$X<-NULL
dataset$Validation<-NULL

dataset <- dataset %>% group_by(Station) %>% arrange(Time,Forecast,Actual) %>%
  mutate(RushTrending=c(0,diff(Forecast)))
dataset$Congestion<-ifelse(dataset$RushTrending=="NA","NA",(ifelse(dataset$RushTrending<0,"DOWN","UP")))

dataset$Congestion<-ifelse(dataset$RushTrending<=0,"DOWN","UP")
write.csv(dataset, "Results tabulation.csv")