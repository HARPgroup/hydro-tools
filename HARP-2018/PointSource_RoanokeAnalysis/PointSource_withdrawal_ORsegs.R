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



## Begin actual consolidation from days >>> year  --------------------
# code does not work beyond this currently

means <- data.frame() #create an empty storage dataframe
years<- data.frame(unique(mergesegs$year))
 
## Search for whether data exists in each of these years (get number of unique wdraw values) -----------
# the data frame uniquecheck will return a value of NA if there are no point source withdrawals for that year 
uniquecheck <- data.frame()
k <- 1 # case study 1996 

for (k in 1:nrow(years)){
datayear <- subset(mergesegs, year==years[k,1])

# unique(datayear$`8020_8130`)
# unique(datayear$`8130_7900`)
uniquecheck[k,1]<- years[k,1]
uniquecheck[k,2]<- length(unique(datayear$`8020_8130`))
uniquecheck[k,3]<- length(unique(datayear$`8130_7900`))

if (uniquecheck[k,2]==1){
  uniquecheck[k,2]<- (unique(datayear$`8020_8130`))
  if (uniquecheck[k,3]==1){
    uniquecheck[k,3]<- (unique(datayear$`8130_7900`))
  }
  
} else if (uniquecheck[k,3]==1){
  uniquecheck[k,3]<- (unique(datayear$`8130_7900`))
}
}
colnames(uniquecheck) <- c("year", "8020_8130", "8130_7900")
uniquecheck[uniquecheck == 0] <- NA

print(uniquecheck)




# unique(OR2_8130_7900$ps)
# unique(OR2_8020_8130$ps)
  
  