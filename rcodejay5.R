#directory set up , change this per your location
setwd('C:\\Users\\Jay Chakravarty\\Documents\\DL 498')



#datafile "Turnstile_Usage_Data__2016.csv" is needed from website: https://data.ny.gov/Transportation/Turnstile-Usage-Data-2016/ekwu-khcy


#Importing only 2017 data 
#datafile<-read.csv( "Turnstile_Usage_Data__2016.csv",sep=",", header=TRUE, nrows=2000000)

datafile<-read.csv( "turnstileData2017.csv",sep=",", header=TRUE)

datafile[,"X"]<-NULL
#colnames(datafile[,8])<-"Time"


library(data.table)
#New column names raionalized
setnames(datafile,old=c('UNIT','STATION','LINENAME','DIVISION','DATE','DESC','ENTRIES','EXITS','TIME'),new=c('Unit','Station','Linenames','Division','Date','Desc','Entries','Exits','Time'))


datafile$TIME<-paste(datafile$Date,datafile$Time, sep = " ", collapse = NULL)


#Converting into time series friendly data via lubridate library

library(lubridate)
library(plyr)
library(dplyr)

#New variable in time format of mdy_hms 






#Lubridate time series conversion
datafile$TIME<-mdy_hms(datafile$TIME)

#sorting Staion and Time Variables
datafile<-datafile[with(datafile, order(Station,TIME)), ]

#removig non zero minute and seconds counter as it creates error
datafile<-datafile[((second(datafile$TIME) | (minute(datafile$TIME)))==0), ]



#Deleting duplicate entries

datafile<-unique(datafile)

start<-'2017-05-01 00-00-00'


datafile <- datafile[!(datafile$TIME < start),]




#creating the total entry and exit in each station (that cntains multiple turnstiles)

datafile1<-na.omit(datafile%>%
  group_by(Station,TIME)%>%
  #arrange(Entries,Exits) %>%
  summarize_each(funs(sum,n()),Entries,Exits))

names(datafile1)<-c("Station","TIME","Entries","Exits","Entry_Turnstl","Exit_Turnstl")



#creating the total entry and exit frequency by differencing in each station (that cntains multiple turnstiles)


datafile1 <- datafile1 %>% group_by(Station) %>% arrange(TIME,Entries,Exits,Entry_Turnstl,Exit_Turnstl) %>%
  mutate(EntryFreQ=c(0,diff(Entries)),ExitFreQ=c(0,diff(Exits)))


datafile1<-datafile1[!(datafile1$EntryFreQ<0 | datafile1$ExitFreQ<0),]




datafile1 <- datafile1 %>% group_by(Station) %>% arrange(TIME,Entries,Exits,Entry_Turnstl,Exit_Turnstl) %>%
  mutate(EntryFreQ=c(0,diff(Entries)),ExitFreQ=c(0,diff(Exits)))


datafile1<-datafile1[!(datafile1$EntryFreQ<0 | datafile1$ExitFreQ<0),]


#datafile1<-datafile1[!((datafile1[,7]<0)|(datafile1[,8]<0)),]
datafile1<-datafile1[!((datafile1[,'EntryFreQ']>100000)|(datafile1[,'ExitFreQ']>100000)),]



#creating the time interval variable

 datafile1 <- datafile1 %>% group_by(Station) %>% arrange(TIME,Entries,Exits) %>%
    mutate(TIME_DIFF = c(4,diff(TIME)))    
 
 
 #creating the total entry and exit frequency by  in each station (that cntains multiple turnstiles) and deviding by time interval
 #toget entry per hour and exit per hor data
 
 datafile1$EntryFreQ<-ifelse(datafile1$TIME_DIFF==0,0,datafile1$EntryFreQ/datafile1$TIME_DIFF)
 datafile1$ExitFreQ<-ifelse(datafile1$TIME_DIFF==0,0,datafile1$ExitFreQ/datafile1$TIME_DIFF)

 datafile1$EntryFreQperTrnstl<-ifelse(datafile1$TIME_DIFF==0,0,(datafile1$EntryFreQ/(datafile1$TIME_DIFF*datafile1$Entry_Turnstl)))
 datafile1$ExitFreQperTrnstl<-ifelse(datafile1$TIME_DIFF==0,0,(datafile1$ExitFreQ/(datafile1$TIME_DIFF*datafile1$Exit_Turnstl)))
                                                                
 
 
 
 #Deleting negative and overtly high frequencty data
 
 # datafile1<-datafile1[!((datafile1[,5]<0)|(datafile1[,6]<0)),]
 # datafile1<-datafile1[!((datafile1[,5]>100000)|(datafile1[,6]>100000)),]
 # 


#creating total time (Elasped) variable for time series needs (probable)

datafile1 <- datafile1 %>% group_by(Station) %>% arrange(TIME,Entries,Exits,datafile1[,6],datafile1[,7]) %>%
  mutate(TOTAL_TIME =(cumsum(TIME_DIFF)))

#Add Entries and Exits fr total traffic including per turnstyle

#datafile1$Trafficperhr<-datafile1$EntryFreQ+datafile1$ExitFreQ

#datafile1$Traffictrnstlhr<-datafile1$EntryperTrnstl+datafile1$ExitperTrnstl
#datafile1$Traffictrnstlhr<-datafile1$EntryFreQperTrnstl+datafile1$ExitFreQperTrnstl



#rationalizing column names

names(datafile1)<-c('Station','TIME','Entries','Exits',"Entry_Turn","Exit_Turn",'EntryFreQ','ExitFreQ','TIME_DIFF',
                    "EntryperTrnstl","ExitperTrnstl","TOTAL_TIME")

#removing rows with all NA s

datafile1 <- datafile1[,colSums(is.na(datafile1))<nrow(datafile1)]


#a file with observation numbers (n) and mean and medians of Entry & Exit frequencies

datafile2<-as.data.frame(datafile1)%>%
  group_by(Station)%>%
  #arrange(Entries,Exits) %>%
  summarize_each(funs(mean, median,n()),EntryFreQ,ExitFreQ,EntryperTrnstl,ExitperTrnstl)
#printing time series plots

datafile2<-as.data.frame(datafile2)

datafile2<-datafile2[order(-datafile2$EntryFreQ_median),]

#datafile2<-[datafile2$EntryFreQ_mediansort(datafile2$EntryFreQ_median,decreasing=T),]
#selecting high volume stationss @ >1500 entries per hour and that have lots of data

this.a<-datafile2[1:30,"Station"]

#this.a <-datafile2[intersect(which(datafile2[,'EntryFreQ_n']>600),which(datafile2[,'EntryFreQ_median']>900)),1][[1]]


#Creating plots for 30 most crowded stations in NYCMTA


library(ggplot2)
#library(hexbin)
library(gridExtra)

#exit plots
#par(mfrow=c(2,2))
p<-''
q<-''


#iterating for 30 crowded stations

for(i in 1:length(this.a)){

#for(i in 1:20){
  
  
  #this.a <- levels(unique(datafile1$Station))
  #this.a<-names(sort(table(datafile1[,1]), decreasing=T))
  #this.a <-names(which((sort(table(datafile1[,1]),decreasing=T))>2000))

  
  
  # Eit plots
  p<-ggplot(datafile1[datafile1$Station==this.a[i],], aes(x=TOTAL_TIME, y=EntryFreQ)) + geom_line() + geom_point() + 
    ylim(0,1.5*(mean(datafile1[datafile1$Station==this.a[i],]$EntryFreQ))) + xlim(0,max(datafile1[datafile1$Station==this.a[i],]$TOTAL_TIME))+
    ggtitle(paste0("Entry per hr Time Series for Station", this.a[i])) +labs(x='hours', y='Entry per hour')
  

  
  # scale_shape_manual(values=c(1,2))  # Use a hollow circle and triangle
  
  #Entry plots
  
  q<-ggplot(datafile1[datafile1$Station==this.a[i],], aes(x=TOTAL_TIME, y=EntryperTrnstl)) + geom_line() + geom_point() + 
    ylim(0,1.5*(mean(datafile1[datafile1$Station==this.a[i],]$EntryperTrnstl))) + xlim(0,max(datafile1[datafile1$Station==this.a[i],]$TOTAL_TIME))+
      ggtitle(paste0("Entry per Turnstyle Time Series for Station", this.a[i])) + labs(x='hours', y='Entry perturnstile per hour')
  
 
  #printing in loop 
  print(p)
  print(q)
  
}
#do.call(grid.arrange(p,nrow=2,ncol=1))
#do.call(grid.arrange(p,q))
####################################################################################################################
#understnding the periodicity o the data.



######################################################################################################################
#Creating a file DatafileNA that introduces 'NA' in place of the missing data


#required libraries
library(forecast)
library(tseries)
library(imputeTS)
library(TSA)




################################################################################################################################
#Imputing NAs with timeseries missing data and correcting irreguar time intervaks. Finally compiling a prediction input file

#checking the time difference values for any aberant values other than 4 (hours) such as 5,8 (hours)

tank<-data.frame()
for(i in 1:length(this.a)){
  
  drob<-as.numeric(names(table(datafile1[datafile1$Station==this.a[i],'TIME_DIFF'])))
  tank<-rbind(tank,drob)
  
}
tank  # inspect value in rows if anything other than 4 or 5

datafile1<-as.data.frame(datafile1)






#Starter file
library(dplyr)
library(data.table)


# 
# #################################################################

# #################################################################

#Loop , this.a vector pulled from the top 30 congested stations

DatafilewNA<-data.frame()

for(j in 1:length(this.a)){
  
  #Since datafile is very lrge , we consider only the data for prediction June 2016 onwards 
  if (this.a[j]=='CHURCH AV'){next}
  
  file<-datafile1[datafile1$Station==this.a[j],]

  
  #file$TIME<-as.POSIXct((file$TIME))
  
  
  #if(any(file$TIME_DIFF!=4)) Toggle1<- as.data.frame(c(seq.POSIXt((file[1,'TIME']),(file$TIME[nrow(file)]),by=14400)))
  
  if(any(file$TIME_DIFF!=4)) Toggle1<- as.data.frame((seq.POSIXt((file[file$TOTAL_TIME==min(file$TOTAL_TIME),]$TIME),(file[file$TOTAL_TIME==max(file$TOTAL_TIME),]$TIME),by=14400)))
  
  
  if(any(file$TIME_DIFF!=4)) Toggle2<- as.data.frame(c(seq((file[1,'TOTAL_TIME']),(file$TOTAL_TIME[nrow(file)]),by=4)))
  
  
  if(all(file$TIME_DIFF==4)) Toggle1<-as.data.frame(file$TIME)
  if(all(file$TIME_DIFF==4)) Toggle2<-as.data.frame(file$TOTAL_TIME)
  
  

  
  Toggle<-(cbind(Toggle1,Toggle2))
  
  Toggle[,3]<-this.a[j]
  
  
  names(Toggle)<-c('TIME','TOTAL_TIME','Station')
  
  

  
  file<-merge(Toggle,file, all.x=TRUE)
  
  DatafilewNA<-rbind.fill(DatafilewNA,file)
  
}

#######################################################################3
#Total Traffic at Turnstile
DatafilewNA$TRAFFIC<-DatafilewNA$EntryperTrnstl+DatafilewNA$ExitperTrnstl
DatafilewNA$WDAY<-wday(DatafilewNA$TIME)



####################################################################################################################################
#Auto Arima Predictons
#########################################################################################################

par(mfrow=c(2, 1))

#Starter list for loop
###########################################################################################################
actual1<-list()
validation1<-list()
MTAStation<-list()
Forecast1<-list()
Forecast42_1<-list()
comparison1<-list()
ACCURACY1<-list()
forecastime1<-list()
forecastime42_1<-list()
########################################################
actual2<-list()
validation2<-list()
MTAStation2<-list()
Forecast2<-list()
Forecast42_2<-list()
comparison2<-list()
ACCURACY2<-list()
forecastime2<-list()
forecastime42_2<-list()
########################################################
actual3<-list()
validation3<-list()
#MTAStation3<-list()
Forecast3<-list()
Forecast42_3<-list()
comparison3<-list()
ACCURACY3<-list()
forecastime3<-list()
forecastime42_3<-list()
########################################################


#looping around all the crowded stations

for(i in 1:length(this.a)){                  #enable this line when all 30 statins are to be predicted
  #for(i in 1:4){                              #disable when enabling the previous line
  
  if (this.a[i]=='CHURCH AV'){next}
    
    file<-DatafilewNA[DatafilewNA$Station==this.a[i],]
    
    
   #creating time series for validation set (left last six data points)
    
############Generating msts Time series ############################
    timeseries1<- msts(file[1:(nrow(file)-12),]$EntryFreQ,seasonal.periods=c(6))
    timeseries2<- msts(file[1:(nrow(file)-12),]$ExitFreQ,seasonal.periods=c(6))
    timeseries3<- msts(file[1:(nrow(file)-12),]$TRAFFIC,seasonal.periods=c(6))
##########################################################################    
    
#creating time series for full set  
#################################################################################
    timeseries1ALL<- msts(file[1:nrow(file),]$EntryFreQ,seasonal.periods=c(6))
    timeseries2ALL<- msts(file[1:nrow(file),]$ExitFreQ,seasonal.periods=c(6))
    timeseries3ALL<- msts(file[1:nrow(file),]$TRAFFIC,seasonal.periods=c(6))
####################################################################################
    
############Generating msts Time series for exogenous variable WDAY###########################################
    #Xregts<- msts(file[1:(nrow(file)-6),]$WDAY,seasonal.periods=c(6))

#####################################################################################################  
    
    #creating time series for exogenous vatriable WDAY   
####################################################################################################
    #XregALLts<- msts(file[1:nrow(file),]$WDAY,seasonal.periods=c(6))
    XregALLts<-c(7,7,7,7,7,7,1,1,1,1,1,1) #,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6)
    Xreg42<-c(7,7,7,7,7,7,1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6,6)
####################################################################################################  
    
    
#Kalman filter based imputation of NAs

############ kalman filter based imputations helped in prediction t#################
#     imp1<-na.kalman(timeseries1,model="auto.arima",smooth=F,nit=-1)
#     imp2<-na.kalman(timeseries2,model="auto.arima",smooth=F,nit=-1)
#     imp3<-na.kalman(timeseries3,model="auto.arima",smooth=F,nit=-1)

    #####################################################################################  
    imp1<-na.seasplit(timeseries1, algorithm = "kalman",smooth=T,nit=-1)
    imp2<-na.seasplit(timeseries2, algorithm = "kalman",smooth=T,nit=-1)
    imp3<-na.seasplit(timeseries3, algorithm = "kalman",smooth=T,nit=-1)
#####################################################################################    
    impALL1<-na.seasplit(timeseries1ALL, algorithm = "kalman",smooth=T,nit=-1)
    impALL2<-na.seasplit(timeseries2ALL, algorithm = "kalman",smooth=T,nit=-1)
    impALL3<-na.seasplit(timeseries3ALL, algorithm = "kalman",smooth=T,nit=-1)
#######################################################################################    

    
        ############ kalman filter based imputations helped in prediction t#################
    # impALL1<-na.kalman(timeseries1ALL,model="auto.arima",smooth=F,nit=-1)
    # impALL2<-na.kalman(timeseries2ALL,model="auto.arima",smooth=F,nit=-1)
    # impALL3<-na.kalman(timeseries3ALL,model="auto.arima",smooth=F,nit=-1)
    #####################################################################################      
    
#Automated ARIMA modelling for validation

#timeseries1.arima<-auto.arima(timeseries1, trace=T,test='adf',ic='aic')
#######################################################################################

    timeseries1.arima<-auto.arima(imp1,xreg=file[1:(nrow(file)-12),]$WDAY, trace=F,test='adf',ic='aic')
    timeseries2.arima<-auto.arima(imp2,xreg=file[1:(nrow(file)-12),]$WDAY,trace=T,test='adf',ic='aic')
    timeseries3.arima<-auto.arima(imp3,xreg=file[1:(nrow(file)-12),]$WDAY,trace=T,test='adf',ic='aic')
########################################################################################    
# ARIMA modelling for all data
    timeseries1ALL.arima<-auto.arima(impALL1, xreg=file$WDAY, trace=F,test='adf',ic='aic')
    timeseries2ALL.arima<-auto.arima(impALL2, xreg=file$WDAY,trace=F,test='adf',ic='aic')
    timeseries3ALL.arima<-auto.arima(impALL3, xreg=file$WDAY, trace=F,test='adf',ic='aic')
    
#timeseries1ALL.arima<-auto.arima(timeseries1ALL, trace=T,test='adf',ic='aic')
#############################################################################################################
    #Xregts.arima<-auto.arima(Xregts,xreg=file[1:(nrow(file)-6),]$WDAY, trace=F,test='adf',ic='aic')
###########################################################################################################   


#Plots showng NAs (disabled for now)

#plotNA.distribution(timeseries1, main =paste('TS Data & Missing data for Station:',this.a[i]), xlab='Time units (Days)',ylab='Entry/hr into station')

#plot showing where NAs wereimputed

#plotNA.imputations(x.withNA = timeseries1,x.withImputations = imp,x.withTruth = NULL, main = paste('Kalman Imputation for:',this.a[i]) , xlab='Time units (Days)',ylab='Entry/hr into station')

    ######################################################################################
#Forecastin on model for validation set
    forecast.timeseries1<-forecast(timeseries1.arima, xreg=file[((nrow(file)-11):nrow(file)),]$WDAY,h=12)
    forecast.timeseries2<-forecast(timeseries2.arima, xreg=file[((nrow(file)-11):nrow(file)),]$WDAY, h=12)
    #forecast.timeseries3<-forecast(timeseries3.arima, xreg=file[1:(nrow(file)-6),]$WDAY, h=6)
    forecast.timeseries3<-forecast(timeseries3.arima, xreg=file[((nrow(file)-11):nrow(file)),]$WDAY, h=12)
    
    ######################################################################################  

    ######################################################################################
#actual Forecast h=6 for all train data (h=6 means at 4 hours interval, this is a daya woth of prediction)
    forecast1.all<-forecast(timeseries1ALL.arima, xreg=XregALLts, h=12)
    forecast2.all<-forecast(timeseries2ALL.arima, xreg=XregALLts, h=12)
    forecast3.all<-forecast(timeseries3ALL.arima, xreg=XregALLts, h=12)
################################################################################################    

    #actual Forecast h=42 for all train data (h=2 means at 4 hours interval, this is a 7 days woth of prediction)
    forecast1_42<-forecast(timeseries1ALL.arima, xreg=Xreg42, h=42)
    forecast2_42<-forecast(timeseries2ALL.arima, xreg=Xreg42, h=42)
    forecast3_42<-forecast(timeseries3ALL.arima, xreg=Xreg42, h=42)
    ################################################################################################    
    
######################################################################################################    
    #plot of validation set forecast (red hollow circles) Data is truncated between 5.5 to 7.5 weeks span
    plot(forecast.timeseries1, type='l', pch=0,xlim=c(60,100),ylim=c(0,2.5*mean(file$EntryFreQ,na.rm=T)),main =paste('Entry Frequency Forecast & validation for Station:',this.a[i]) , xlab='Time units (Days)',ylab='Entry/hr into station')

#Validation set prediction red hollow labels
    lines(forecast.timeseries1$mean,col="red",type='p',pch=19)

#all actual data green hollow circles
    lines(timeseries1ALL,type='p', col='blue')

#frecasted values h=6 from Arima model
    lines(forecast1.all$mean, type='b', col='black',pch=19)


    legend("bottomleft", legend=c("actual", "Validation", "Forecast"),col=c("blue", "red","black"), pch=19, cex=0.8, box.lty=0)
##############################################################################################################
    #plot of validation set forecast (red hollow circles) Data is truncated between 5.5 to 7.5 weeks span
    plot(forecast.timeseries2, type='l', pch=0,xlim=c(60,100),ylim=c(0,2.5*mean(file$ExitFreQ,na.rm=T)),main =paste('Exit Frequency Forecast & validation for Station:',this.a[i]) , xlab='Time units (Days)',ylab='Exit/hr into station')
    
    #Validation set prediction red hollow labels
    lines(forecast.timeseries2$mean,col="red",type='p',pch=19)
    
    #all actual data green hollow circles
    lines(timeseries2ALL,type='p', col='green')
    
    #frecasted values h=6 from Arima model
    lines(forecast2.all$mean, type='b', col='black',pch=19)
    
    
    legend("bottomleft", legend=c("actual", "Validation", "Forecast"),col=c("green", "red","black"), pch=19, cex=0.8, box.lty=0)
########################################################################################################################
    
    #plot of validation set forecast (red hollow circles) Data is truncated between 5.5 to 7.5 weeks span
    plot(forecast.timeseries3, type='l', pch=0,xlim=c(60,100),ylim=c(0,2.5*mean(file$TRAFFIC,na.rm=T)),main =paste('TRAFFIC/(hr-Turnstile) Forecast & validation for Station:',this.a[i]) , xlab='Time units (Days)',ylab='Traffic/(hr-Turnstile) into station')
    
    #Validation set prediction red hollow labels
    lines(forecast.timeseries3$mean,col="red",type='p',pch=19)
    
    #all actual data green hollow circles
    lines(timeseries3ALL,type='p', col='brown')
    
    #frecasted values h=6 from Arima model
    lines(forecast3.all$mean, type='b', col='black',pch=19)
    
    
    legend("bottomleft", legend=c("actual", "Validation", "Forecast"),col=c("brown", "red","black"), pch=19, cex=0.8, box.lty=0)
    ########################################################################################################################
    
    
    
    
##################################################################################################################    
#plot of actual Six (=24 hours) forecast 

#Predictions of 6 points (=24 hours) of entry freqencies per hour(EntryFreQ ~ similar to Congestion or Rush)


#list of validation set predictions (to compare with actual values at that point)
    validation1<-c(validation1,(as.vector(forecast.timeseries1$mean)))
    validation2<-c(validation2,(as.vector(forecast.timeseries2$mean)))
    validation3<-c(validation3,(as.vector(forecast.timeseries3$mean)))
    

#list of predictions form the model "Forecast" to be fed to the App 
    Forecast1<-c(Forecast1,as.vector(forecast1.all$mean))
    Forecast2<-c(Forecast2,as.vector(forecast2.all$mean))
    Forecast3<-c(Forecast3,as.vector(forecast3.all$mean))
    
#list of predictions form the model "Forecast" to be fed to the App for 1 week forecast
    Forecast42_1<-c(Forecast42_1,as.vector(forecast1_42$mean))
    Forecast42_2<-c(Forecast42_2,as.vector(forecast2_42$mean))
    Forecast42_3<-c(Forecast42_3,as.vector(forecast3_42$mean))
    
    
    
    

#Actual values that to be comoared with the validation set predictions
    actual1<-c(actual1,tail(DatafilewNA[DatafilewNA$Station==this.a[i],]$EntryFreQ,length(forecast.timeseries1$mean)))
    actual2<-c(actual2,tail(DatafilewNA[DatafilewNA$Station==this.a[i],]$ExitFreQ,length(forecast.timeseries2$mean)))
    actual3<-c(actual3,tail(DatafilewNA[DatafilewNA$Station==this.a[i],]$TRAFFIC,length(forecast.timeseries3$mean)))
    

#forecastime[i]<-tail(DatafilewNA[DatafilewNA$Station==this.a[i],]$TIME,length(forecast.timeseries1$mean))

    forecastime1<-c(forecastime1,as.character(seq((as.POSIXct(file[nrow(file),'TIME'])+14400),by=14400,length.out=length(forecast.timeseries1$mean))))
    forecastime2<-c(forecastime2,as.character(seq((as.POSIXct(file[nrow(file),'TIME'])+14400),by=14400,length.out=length(forecast.timeseries2$mean))))
    forecastime3<-c(forecastime3,as.character(seq((as.POSIXct(file[nrow(file),'TIME'])+14400),by=14400,length.out=length(forecast.timeseries3$mean))))

    
    forecastime42_1<-c(forecastime42_1,as.character(seq((as.POSIXct(file[nrow(file),'TIME'])+14400),by=14400,length.out=length(forecast1_42$mean))))
    # forecastime42_2<-c(forecastime42_2,as.character(seq((as.POSIXct(file[nrow(file),'TIME'])+14400),by=14400,length.out=length(forecast2_42$mean))))
    # forecastime42_3<-c(forecastime42_3,as.character(seq((as.POSIXct(file[nrow(file),'TIME'])+14400),by=14400,length.out=length(forecast3_42$mean))))
    # 
    
    
    
    

#inserting Station Name list
    MTAStation<-c(MTAStation,as.vector(rep(this.a[i], 12)))
    MTAStation2<-c(MTAStation2,as.vector(rep(this.a[i], 42)))
    
    

    ACCURACY1<-c(ACCURACY1,list(accuracy(forecast.timeseries1$mean, tail(file$EntryFreQ,12),length(forecast.timeseries1$mean))[2]))
    ACCURACY2<-c(ACCURACY2,list(accuracy(forecast.timeseries2$mean, tail(file$ExitFreQ,12),length(forecast.timeseries2$mean))[2]))
    ACCURACY3<-c(ACCURACY3,list(accuracy(forecast.timeseries3$mean, tail(file$TRAFFIC,12),length(forecast.timeseries3$mean))[2]))


}

comparison<-data.frame(unlist(MTAStation),unlist(forecastime1),unlist(Forecast1),unlist(validation1),unlist(actual1),unlist(forecastime2),unlist(Forecast2),
                       unlist(validation2),unlist(actual2),unlist(forecastime3),unlist(Forecast3),unlist(validation3),unlist(actual3))

colnames(comparison)<-c('Station','Time(Ent)','Forecast(Ent)','Validation(Ent)','Actual(Ent)','Time(Ext)','Forecast(Ext)','Validation(Ext)','Actual(Ext)',
                          'Time(Trf)','Forecast(Trf)','Validation(Trf)','Actual(Trf)' )


ACCURACY<-data.frame(unlist(ACCURACY1),unlist(ACCURACY2),unlist(ACCURACY3))

names(ACCURACY)<-c("ACCURACY(Ent)-RMSE","ACCURACY(Ext)-RMSE","ACCURACY(Trf)-RMSE")

APPInput<-data.frame(unlist(MTAStation2),unlist(forecastime42_1),unlist(Forecast42_1),unlist(Forecast42_2),unlist(Forecast42_3))

colnames(APPInput)<-c('Station','Time','Forecast(Entry/h)',',Forecast(Exit/h)','Forecast(Traffic/h_turnstl)')



######################################################################################################

plot(comparison[,'Actual(Ent)'],comparison[,'Validation(Ent)'], main="ACTUAL V VALIDATION SET Jan-Jul 2017", xlab='Actual Value, Entries/hr',ylab='Validation Value, Entries/hr',type='p', col='pink', pch=19)
plot(comparison[,'Actual(Ext)'],comparison[,'Validation(Ext)'], main="ACTUAL V VALIDATION SET Jan-Jul 2017", xlab='Actual Value, Exit/hr',ylab='Validation Value, Exit/hr',type='p', col='green', pch=19)
plot(comparison[,'Actual(Trf)'],comparison[,'Validation(Trf)'], main="ACTUAL V VALIDATION SET Jan-Jul 2017", xlab='Actual Value, Traffic/hr-Turnstile',ylab='Validation Value, Traffic-Turnstile/hr',type='p', col='brown', pch=19)





# write results in CSV file

write.csv(comparison, file = "Results tabulation.csv")

write.csv(APPInput, file = "APPInput.csv")

#################################################################################################




