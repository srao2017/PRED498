

datafile<-"https://s3.amazonaws.com/aws-website-pzpass-ejkuo/pzpass/PZPass/station_score_by_hour_gbm_test_out.csv"
td_in<-read.csv(datafile)

td_in$per_tstile_overload<-ifelse ( (ol=(td_in$ScoreExt + td_in$ScoreEnt) - 340) > 0, ol, 0)

td_load<-td_in[td_in$per_tstile_overload > 0,]

library(data.table)
DT <- data.table(td_load)

td_out_tcount<-DT[,max(count.x), by = "Station"]
td_out_overload<-DT[,max(per_tstile_overload), by = "Station"]
td_out_needed_tstiles<-DT[, max(per_tstile_overload)/340, by = "Station"]
td_out_needed_fp_lanes<-DT[, max(per_tstile_overload)/(340*4), by = "Station"]

write.csv(td_out_overload,"tmp1.csv")
write.csv(td_out_needed_tstiles,"tmp2.csv")
write.csv(td_out_needed_fp_lanes,"tmp3.csv")
write.csv(td_out_tcount,"tmp4.csv")
