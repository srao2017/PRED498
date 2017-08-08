######################################################################
# NY MTA Models on cleaned data
# Data is mostly clean after removal of extreme values and insertion
# of new derived parameters: Month, Week, Day, Ent, Ext
# Ent = Entries, Ext = Exits
#
#
# Models in this code are: lm, tree, gbm
#
#
# NY MTA Turnstile data files can be retrieved @
# http://web.mta.info/developers/turnstile.html
#
# Author:       Sanjay Rao
# Organization: Northwestern University (student)
# Course:       PREDICT 498 Capstone
# Project:      SmartCity IoT (Team 03)
#
######################################################################

library(tree)
library(gbm)
library(forecast)
library(ggplot2)
library(reshape2)
library(ISLR)
library(boot)

#directory set up 
setwd('C:/EDUCATION/NWU/498 - Capstone/TEAM SMARTCITY/DATA_ANALYTICS/DATA/')

#
# Read the cleaned and processed turnstile data
# The data were wrangled using PostgreSQL
# Data were loaded from the original Turnstile_Usage_Data__2016.csv file
# downloaded from:
# https://data.ny.gov/Transportation/Turnstile-Usage-Data-2016/ekwu-khcy
#

# Define variables to hold the datafiles for training and testing 
#
train_datafile<-"TRAINING/DailyExitsEntries-30.csv"

validation_datafile<-"VALIDATION/processed_30_20170722.csv"

trunstiles_per_station<-"TRAINING/TurnstilesPerStation16.csv"
tps<-read.csv(trunstiles_per_station, header=TRUE, sep=",")

# Read the data file
datafileTrain<-read.csv(train_datafile, sep=",", header=TRUE)
datafileTrain<-merge(datafileTrain,tps,by="Station")
# Remove negative values
datafileTrain<-datafileTrain[datafileTrain$Ent >0,]
datafileTrain<-datafileTrain[datafileTrain$Ext >0,]

# Load the test datafile
datafileValidate<-read.csv(validation_datafile, sep=",", header=TRUE)
datafileValidate<-merge(datafileValidate,tps,by="Station")
# Remove negative values
datafileValidate<-datafileValidate[datafileValidate$Ent >0,]
datafileValidate<-datafileValidate[datafileValidate$Ext >0,]

#------------------------------BEGIN FUNCTIONS-------------------------------
Score<-function(df, model_type="gbm",pct_act_turnstiles=0.75) {

  # pct_act_turnstiles is the percentage of active turnstiles
  # this percentages affects the score. Lower percentages mean that more people 
  # use fewer available turnstiles and therefore increase congestion
  #
  trunstiles_per_station<-"TRAINING/TurnstilesPerStation16.csv"
  tps<-read.csv(trunstiles_per_station, header=TRUE, sep=",")
  
  df$Interval<-as.numeric(c(0,df$Hour[2:nrow(df)] - df$Hour[1:(nrow(df)-1)]))
  df$Interval<-ifelse (df$Interval<0, df$Interval+24, df$Interval)
  
  df$Interval<-ifelse(df$Interval<=0,4,df$Interval)
  
  # Join the turnstiles per station to the df using Station as key
  df<-merge(df,tps,by="Station")
  
  # compute number of turnstiles that may be available for entries
  # assumes that half of the total available turnstiles are used 
  # for entry and the other half for exit
  t_count_ent<-df$count.x
  # compute number of turnstiles that may be available for exits
  t_count_ext<-df$count.x
  # compute an IntervalHour(Ent,Ext) normalizing a station to a turnstile!
  # assumes a default of 75% of all available turnstiles are usable
  # assumes equal distribution between entries and exits
  # computes "turnstile hour" (like "man hour") by multiplying no. turnstiles and the interval
  df$turnstileHourEnt<-t_count_ent*pct_act_turnstiles*(df$Interval*1.0)
  df$turnstileHourExt<-t_count_ext*pct_act_turnstiles*(df$Interval*1.0)

  df$ScoreExt<-df$PredExt/df$turnstileHourExt
  df$ScoreEnt<-df$PredEnt/df$turnstileHourEnt
  write.csv( df, paste0("RESULTS/station_score_by_hour_",model_type,"_test_out.csv"),sep=",", col.names=TRUE)
  df
}


PlotScores<-function(model_type="gbm", subway="FULTON ST", day=3, week=26,plot_span=1) {

  P<-read.csv(paste0("RESULTS/station_score_by_hour_",model_type,"_test_out.csv"))
  P$ScoreEnt<-abs(P$ScoreEnt)
  P$ScoreExt<-abs(P$ScoreExt)
  P$Capacity<-170
  d<-P[(P$Station %in% subway) & (P$Day %in% day) & (P$Week %in% week),]
  d<-d[c("Hour","ScoreExt","ScoreEnt","Capacity")]
  

  d <- melt(d, id.vars="Hour")

  p<-ggplot(d, aes(Hour,value,col=variable)) + 
    geom_smooth(level=0.0,size=2,span=plot_span) + 
    labs(y="Turnstile\nPersons/hour") +
    scale_color_manual(labels = c("Exits", "Entries", "Treshold"), values = c("blue", "red","orange")) +
    theme(legend.title=element_blank()) +
    theme(axis.text.y = element_text(size=15)) + 
    theme(plot.title=element_text(margin = margin(t=30,b=-20)))+labs(title=toString(subway))
  p
}
#------------------------------END FUNCTIONS-------------------------------


#---------------------------Linear Model---------------------------#
# Linear model - Lets see how we do with a multivariate polynomial linear model
# factor is needed to take care of the categorical variable "Station"
# Use the parameters Station, Month, Week, Day and Hour - all of which can be provided by a user
# when the model is deployed and the volume for a specific day and hour are needed
#
# Reinitialize X
X<-datafileTrain
Z<-datafileValidate


summary(X)
summary(Z)

set.seed(17)
# Ploy LM for Entries model 
cv.error.6=rep(0 ,6)
for (i in 1:6){
    # fit the model for a selected polynomial degree
    glm.fitEnt=glm(log(Ent)~factor(Station) + poly(Month,i) + poly(Week,i) + poly(Day,i) + poly(Hour,i) , data=X)
    # do a 6 fold cross validation for the chosen polynomial degree
    cv.error.6[i]=cv.glm(X,glm.fitEnt ,K=6)$delta [1]
}
cv.error.6

# Ploy LM for Exits model
set.seed(17)
cv.error.6=rep(0 ,6)
for (i in 1:6){
  glm.fitExt=glm(log(Ext)~factor(Station) + poly(Month,i) + poly(Week,i) + poly(Day,i) + poly(Hour,i) , data=X)
  cv.error.6[i]=cv.glm(X ,glm.fitExt ,K=6)$delta [1]
}
cv.error.6


plot(glm.fitEnt)
plot(glm.fitExt)

# Get summary stats for the model
summary(glm.fitEnt)
summary(glm.fitExt)
# Get the coefficients
coef(glm.fitEnt)
coef(glm.fitExt)

summary(glm.fitEnt)$coef
summary(glm.fitExt)$coef

# Predict values
X$PredEnt<-exp(predict(glm.fitEnt, newdata=X))
X$PredExt<-exp(predict(glm.fitExt, newdata=X))

rmse_ent_train<-sqrt(mean((X$PredEnt - X$Ent)^2))
rmse_ent_train

rmse_ext_train<-sqrt(mean((X$PredExt - X$Ext)^2))
rmse_ext

# Write the result to a csv file
write.csv (X, "RESULTS/station_volume_by_hour_GLM_out.csv") 

# Validate on test data
Z$PredEnt<-exp(predict(glm.fitEnt,newdata = Z))
#Compute MSE
rmse_ent<-sqrt(mean((Z$PredEnt - Z$Ent)^2))
rmse_ent

Z$PredExt<-exp(predict(glm.fitExt,newdata=Z))
# Compute MSE
rmse_ext<-sqrt(mean((Z$PredExt - Z$Ext)^2))
rmse_ext

Z<-Score(Z,"GLM")
print(PlotScores("GLM", subway="TIMES SQ-42 ST", day=4))


#---------------------------TREE---------------------------#
# Tree model - 
# factor is needed to take care of the categorical variable "Station"
# Use the parameters Station, Month, Week, Day and Hour - all of which can be provided by a user
# when the model is deployed and the volume for a specific day and hour are needed
#
# Requires the tree library
#


# Reinitialize X
# Reinitialize X
X<-datafileTrain

Z<-datafileValidate


tree.fitEnt=tree(Ent~factor(Station) + Month + Week + Day + Hour, data=X)
tree.fitExt=tree(Ext~factor(Station) + Month + Week + Day + Hour, data=X)

summary(tree.fitEnt)
summary(tree.fitExt)

coef(tree.fitEnt)
coef(tree.fitExt)
summary(tree.fitEnt)$coef
summary(tree.fitExt)$coef

# Do the prediction Y-hat values
X$PredEnt<-predict(tree.fitEnt)
X$PredExt<-predict(tree.fitExt)
# See summary stats
summary(X$PredEnt)
summary(X$PredExt)

# See boxplots of Entries per hour
boxplot(X$PredEnt~X$Hour)
boxplot(X$Ent~X$Hour)
boxplot(X$PredEnt~X$Day)
boxplot(X$Ent~X$Day)
write.csv( X, "RESULTS/station_volume_by_hour_TREE_out.csv",sep=",", col.names=TRUE)

# Validate on test data
Z$PredEnt<-predict(tree.fitEnt,newdata=Z)
#Compute MSE
rmse_ent<-sqrt(mean((Z$PredEnt - Z$Ent)^2))

Z$PredExt<-predict(tree.fitExt,newdata=Z)
# Compute MSE
rmse_ext<-sqrt(mean((Z$PredExt - Z$Ext)^2))

Z<-Score(Z,"tree")
print(PlotScores("tree",subway="FULTON ST", day=4))


#---------------------------Boosted Regression Tree---------------------------#
# Requires the gbm library
#
library(caret)

# Reinitialize X
X<-datafileTrain
Z<-datafileValidate


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)


set.seed(825)
gbmFit1 <- train(Ent~Station + Month + Week + Day + Hour, data = X, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1



set.seed(123)
#boost.fitEnt<-gbm(Ent~. -Date -Time -Ext -count -cap_per_hr, data=X, distribution="gaussian", n.trees=15000, interaction.depth=4, shrinkage=0.01)
boost.fitEnt<-gbm(Ent~Station + Month + Week + Day + Hour, data=X, distribution="gaussian", n.trees=150, interaction.depth=32, shrinkage=1,  n.cores=4)

summary(boost.fitEnt)
plot(boost.fitEnt)
X$PredEnt<-abs(predict(boost.fitEnt, n.trees=150, newdata=X))
#Compute MSE
mse<-mean((X$PredEnt - X$Ent)^2)
se<-sqrt(mse)
se
gbm.perf(boost.fitEnt)

boost.fitExt<-gbm(Ext~Station + Month + Week + Day + Hour, data=X, distribution="gaussian", n.trees=150, interaction.depth=32, shrinkage=1)
summary(boost.fitExt)
plot(boost.fitExt)
X$PredExt<-abs(predict(boost.fitExt, n.trees=150, newdata=X))
#Compute MSE
mse<-mean((X$PredExt - X$Ext)^2)
se<-sqrt(mse)
se
gbm.perf(boost.fitExt)

# Save the models
save (boost.fitEnt, file="MODELS/gbm_ent_model.rda")
save (boost.fitExt, file="MODELS/gbm_ext_model.rda")


write.csv( X, "RESULTS/station_volume_by_hour_GBM_out.csv",sep=",", col.names=TRUE)

#boost.fitEnt<-load(file="MODELS/gbm_ent_model.rda")
#boost.fitExt<-load(file="MODELS/gbm_ext_model.rda")

X<-Score(X,"gbm")
print(PlotScores("gbm",subway="34 ST-HERALD SQ",day=0,week=32))

# Validate on test data
Z$PredEnt<-abs(predict(boost.fitEnt, n.trees=150, newdata=Z))
#Compute test MSE
mse<-mean((Z$PredEnt - Z$Ent)^2)
se<-sqrt(mse)
se
#Z$EntMse<-Z$PredEnt - Z$Ent
Z$PredExt<-abs(predict(boost.fitExt, n.trees=150, newdata=Z))
#Compute test MSE
mse<-mean((Z$PredExt - Z$Ext)^2)
se<-sqrt(mse)
se

#Z$ExtMse<-Z$PredExt - Z$Ext
Z<-Score(Z,"gbm")
print(PlotScores("gbm",subway="34 ST-HERALD SQ",day=2, week=29))

#########################################################################
############################# END #######################################
#########################################################################



