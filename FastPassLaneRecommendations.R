######################################################################
# Author:       Sanjay Rao
# Organization: Northwestern University (student)
# Course:       PREDICT 498 Capstone
# Project:      SmartCity IoT (Team 03)
#
# Recommend the mix of Turnstiles and Fast pass lanes 
# for the to 30 stations ("top" criteria is based on number of turnstiles)
# 
# Input here is the output of the model (GBM Model)
#
# The data is read into a data table so that we can "group by" station
# The goal of this code is to determine the number of fast pass lanes
# needed to alleviate congestion and also recommend the mix of turnstile to fastpass lane
# for a station. Basically the formula cuts 50% of turnstiles and replaces them
# with a smaller number of fastpass lanes (4 turnstiles = 1 fast pass lane)
#
#
######################################################################


#Output directory set up 
setwd('C:/EDUCATION/NWU/498 - Capstone/TEAM SMARTCITY/DATA_ANALYTICS/DATA/RESULTS')
#
# Get the file from the S3 repository which stores the model output
datafile<-"https://s3.amazonaws.com/aws-website-pzpass-ejkuo/pzpass/PZPass/station_score_by_hour_gbm_test_out.csv"
# Read the data
td_in<-read.csv(datafile)
#
# Based on previous Erlang C based computations the max per hour capacity of a 
# turnstile is 340 (8 secs/person) - note if Erlang C is not adopted 
# this will be 450 (60mins*60secs/8)
#
td_in$per_tstile_overload<-ifelse ( (ol=(td_in$ScoreExt + td_in$ScoreEnt) - 340) > 0, ol, 0)

# Only consider stations with an overload > 0
td_load<-td_in[td_in$per_tstile_overload > 0,]

# Use data table
library(data.table)

# Load the data into the data table
DT <- data.table(td_load)

# Start creating the output
# 1st column is Station
# Group by Station and create 2nd column called Tstile_count
td_out<-DT[,max(count.x), by = "Station"]
colnames(td_out)[2] <- "Tstile_Count"

# Group by Station and create 3rd column called Overload
td_out_overload<-DT[,max(per_tstile_overload), by = "Station"]
td_out<-merge(td_out, td_out_overload, by="Station")
colnames(td_out)[3] <- "Overload"

# Group by Station and create 3rd column called Tstiles_needed.
td_out_needed_tstiles<-DT[, max(per_tstile_overload)/340, by = "Station"]
td_out<-merge(td_out, td_out_needed_tstiles, by="Station")
colnames(td_out)[4] <- "Tstiles_Needed"

# Group by Station and create 4th column called FP_Lanes_Needed. This is based on the
# assumption that FP lanes are 4 times faster than turnstiles
td_out_needed_fp_lanes<-DT[, max(per_tstile_overload)/(340*4), by = "Station"]
td_out<-merge(td_out, td_out_needed_fp_lanes, by="Station")
colnames(td_out)[5] <- "FP_Lanes_Needed"

# Create recommendations - cut turnstiles in half and add smaller number of FP lanes
# not forgetting the addition of FP lanes to relieve overload
td_out$TS_Recommended<-td_out[, (td_out$Tstile_Count/2)]
td_out$FP_Recommended<-td_out[, (td_out$Tstile_Count/2)/4+td_out$FP_Lanes_Needed]

# Write the output to a csv file
write.csv(td_out,"Recommendation.csv")

