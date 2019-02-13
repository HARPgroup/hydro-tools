# Kelsey Reitz
#2/6/2019
# Quality Assurance of original Point Source Evaluation code 
# (original named PointSourceEval_SRVA_original in the directory)

#this script only looks at the two OR segments in question, rather than the entire dataset.  

## load libraries
library(lubridate)
library(dplyr)
library(RCurl)

setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis")


# # Observation of current data listed within the folder --------------
# divr07 <- read.csv("ps_sep_div_ams_p532cal_062211_3007.csv")
# diva08 <- read.csv("ps_sep_div_ams_p532cal_062211_3008.csv")
# 
# divr_segs <- divr07[which(divr07$rivsegs=="OR2_8130_7900" | divr07$rivsegs=="OR2_8020_8130"),]
# diva_segs <- diva08[which(diva08$rivsegs=="OR2_8130_7900" | diva08$rivsegs=="OR2_8020_8130"),]
# 

#Re-analysis using just the two OR Segments -------------

deq <- "http://deq2.bse.vt.edu/p532c-sova/wdm/river/p532cal_062211/eos/"

#MUST CHANGE WHAT CODE YOU'RE DOING.-- changed by: 
code <- 3007

rivsegs <- data.frame()
rivsegs[1,1] <- "OR2_8130_7900"
rivsegs[2,1] <- "OR2_8020_8130"
i <- 1


for (i in 1:nrow(rivsegs)){
study_seg <- paste0("ps_sep_div_ams_p532cal_062211_",rivsegs[i,1],"_",code,".csv")
#check to make sure that the file exists on the site. 
#if it doesn't exist, move on to the next segment
goodtogo <- url.exists(paste0(deq, study_seg)) # check to make sure csv exists

  if (goodtogo ==TRUE){
    importdata <- read.csv(paste0(deq, study_seg))
    colnames(importdata) <- c("year", "month", "day", "hour", "ps")
  }

#store the results of this pull for seg 1 and 2
if (i==1){
  seg_8130_7900 <- data.frame(importdata)
} else if (i==2){
  seg_8020_8130 <- data.frame(importdata)
}

}

## Format data into one consolidated data frame
seg_8020_8130$rownum <- 1:nrow(seg_8020_8130)
seg_8130_7900$rownum <- 1:nrow(seg_8130_7900)

mergesegs <- merge(seg_8020_8130, seg_8130_7900, by.x = "rownum", by.y = "rownum", all = TRUE)
mergesegs <- select(mergesegs, rownum, year.x, month.x, day.x, hour.x, ps.x, ps.y)
colnames(mergesegs) <- c("rownum", "year", "month", "day", "hour", "8020_8130", "8130_7900")



## Begin analysis of point sources and where / what year they are in  --------------------
# 
# convert hourly to daily: referencing the data_downloader R script from HARP 2018 DEQ_Model_vs_USGS_comparison


# IMPORTING AND EXPORTING MODEL DATA ----------------------------------------------------

# Splitting the River Segment string into each segment name



RivSegStr <- seg_8130_7900        # not analyzing upstream, just down
start.date <- "1984-01-01"
end.date <- "2005-12-31"

model_days <- length(seq(as.Date(start.date):as.Date(end.date)))

#Reads data into data frame "ovols", exports each seg's data

# Downloading and exporting hourly model data
model_hourly <- RivSegStr 
  
# Converting hourly to daily data and exporting daily data
# model_hourly <- model_hourly[-1,]
#model_hourly$V1 <- trimws(model_hourly$V1, which = "both")
colnames(model_hourly) <- c("year","month","day","hour","ovol", "rownum")
model_hourly$date <- as.Date(paste0(model_hourly$year,"-",model_hourly$month,"-",model_hourly$day))
model_daily <- aggregate(model_hourly$ovol, list(model_hourly$date), FUN = sum)
colnames(model_daily) <- c("date","mod.flow")
#write.csv(model_daily, file = paste0(container, "/data/new_(updated)_data/raw_data/model_data/daily_data/",RivSeg," - Daily Raw Data.csv"))
  
# convert from ac-ft/day to cfs
model_daily$mod.flow <- model_daily$mod.flow * 0.504167
model_daily <- subset(model_daily, year(model_daily$date)==2002)

# different ways that monthly flow may be interpreted by the model

# mean of monthly flows
model_monthly_bymean <- aggregate(model_daily$mod.flow, list(month(model_daily$date)), FUN = mean)
colnames(model_monthly_bymean) <- c("month","mod.flow")

# day one of the month
model_monthly_byday1 <- data.frame(subset(model_daily, day(model_daily$date)==1))
colnames(model_monthly_byday1) <- c("month","mod.flow")



# pull in withdrawal data from wayside and spring hollow --------------------
mon_wayside <- read.csv("wayside_mon.csv", stringsAsFactors = FALSE)
mon_spholla <- read.csv("springhol_mon.csv", stringsAsFactors = FALSE) 

mon_wayside$Date <- as.Date(mon_wayside$Date)
mon_spholla$Date <- as.Date(mon_spholla$Date)
mon_wayside$Value <- as.numeric(gsub(",", "",mon_wayside$Value))
mon_spholla$Value <- as.numeric(gsub(",", "",mon_spholla$Value))


mon_wayside <- subset(mon_wayside, varkey=="wd_mgm" & year(mon_wayside$Date)==2002)
mon_spholla <- subset(mon_spholla, varkey=="wd_mgm" & year(mon_spholla$Date)==2002)

vahydro <- data.frame(mon_wayside$Date, mon_wayside$Value, mon_spholla$Value)
vahydro <- vahydro[order(vahydro$mon_wayside.Date), ]




## compare wayside, spring hollow, and MEAN monthly model data -------------
comparison <- data.frame(vahydro[,1:3], model_monthly_bymean$mod.flow)
colnames(comparison)<- c("date", "wayside", "sphol", "model")

comparison$summed <- comparison$wayside + comparison$sphol
comparison$error <- 100*(comparison$model - comparison$summed) / (comparison$model)
write.csv(comparison, "segs_vs_meanmonthly.csv")


## compare wayside, spring hollow, and day1 monthly model data -------------
comparisonday <- data.frame(vahydro[,1:3], model_monthly_byday1$mod.flow)
colnames(comparisonday)<- c("date", "wayside", "sphol", "model")

comparisonday$summed <- comparisonday$wayside + comparisonday$sphol
comparisonday$error <- 100*(comparisonday$model - comparisonday$summed) / (comparisonday$model)
write.csv(comparisonday,"segs_vs_day1monthly.csv")
