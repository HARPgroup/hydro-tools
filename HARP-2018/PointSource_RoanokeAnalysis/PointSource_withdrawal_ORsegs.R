# Kelsey Reitz
#2/6/2019
# Quality Assurance of original Point Source Evaluation code 
# (original named PointSourceEval_SRVA_original in the directory)

#this script only looks at the two OR segments in question, rather than the entire dataset.  

setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/PointSource_RoanokeAnalysis")


# Observation of current data listed within the folder --------------
divr07 <- read.csv("ps_sep_div_ams_p532cal_062211_3007.csv")
diva08 <- read.csv("ps_sep_div_ams_p532cal_062211_3008.csv")

divr_segs <- divr07[which(divr07$rivsegs=="OR2_8130_7900" | divr07$rivsegs=="OR2_8020_8130"),]
diva_segs <- diva08[which(diva08$rivsegs=="OR2_8130_7900" | diva08$rivsegs=="OR2_8020_8130"),]


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
  seg_8020_7900 <- data.frame(importdata)
}

}




## Begin actual consolidation from days >>> year  --------------------
# code does not work beyond this currently

iszero <- sum(importdata$ps)==0 #True or False? 

if (iszero == TRUE){ #then fill with value of 0 for all years for that seg
  pointsource[i,2:ncol(pointsource)] <- rep(0, length(years))
  
}else if (iszero==FALSE){ #then calculate mean of all hourly values for that year
  means <- data.frame() #create an empty storage dataframe
  
  k <- 1
  for (k in 1:length(years)){ #loop the mean calculation for every year of analysis
    datayear <- subset(importdata, year==years[k])
    datayear <- datayear[1:nrow(datayear)-1,] #remove the 24th hour of dec 31
    meanval <- summarize(datayear, mean(ps))
    means[1,k] <- meanval
    k <- k + 1
  }
  colnames(means) <- as.character(years)
  pointsource[i,2:ncol(pointsource)] <- means
}

}else if (goodtogo == FALSE){
  pointsource[i,2:ncol(pointsource)] <- rep("N.Data", length(years))
}
i <- i + 1

}








unique(OR2_8130_7900$ps)
unique(OR2_8020_8130$ps)
  
  