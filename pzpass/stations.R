setwd("/srv/shiny-server/pzpass/pzpass")
P<-read.csv("station_score_by_hour_gbm_test_out.csv")
U<-unique(P$Station)
write.csv (U,"./stations.csv")
P<-read.csv("stations.csv")
P


dt<-Sys.Date()
dt<-as.POSIXct(as.character(dt))
woy<-as.integer(strftime(dt,format="%W"))
dow<-as.integer(strftime(dt,format="%w"))
day<-as.integer(0)
dow<-(dow+day)%%7
