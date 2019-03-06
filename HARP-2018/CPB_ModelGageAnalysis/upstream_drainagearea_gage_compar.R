# 2/20/2019
# create code to compare drainage areas of all upstream segments from gages

# LOAD LIBRARIES AND SET WORKSPACES -----------------------
library(dataRetrieval)
library(stringr)
library(rapportools)

setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis")
file_path <- "C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis/GIS_Seg"
  
# LOAD AND FORMAT DATA FROM FOLDER ---------------------
all_segs <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQCrbbMS-XgzxOG0btMencBB_6F7kTqTodlb9FQU67nlGCSKKmXsXZNC-G-sUghdI4ya8heUIugBMjn/pub?output=csv", stringsAsFactors = FALSE) 
all_segs <- data.frame(all_segs$RiverSeg, all_segs$AreaSqMi)
colnames(all_segs) <- c("rivseg", "model_da")
      #pull a list of the segments that we're analyzing (came from the model)
      #note: Northern Rivers Segments csv



# Pull gages from GIS file (p53 calibstats shapefile) 
gages <- read.csv("Gage_stations.csv", stringsAsFactors = FALSE)
gages <- data.frame(as.numeric(as.character(gages$STAID)), gages$NAME, gages$CATCODE2)
gages$gages.NAME <- as.character(gages$gages.NAME)
gages$gages.CATCODE2 <- as.character(gages$gages.CATCODE2)
gages <- gages[(complete.cases(gages)),] #remove rows with na values

#remove susquehanna gages
gages$letter <- str_sub(gages$gages.CATCODE2, start=0L, end=1L)
gages <- gages[which(gages$letter!= "S"),]
gages <- data.frame(gages$as.numeric.as.character.gages.STAID.., gages$gages.CATCODE2)
gages$gages.gages.CATCODE2 <- as.character(gages$gages.gages.CATCODE2)
colnames(gages) <- c("staid", "rivseg")

#it is improperly labeled in GIS -- the file reads JB7_7070_0001, but it is *JL*
fixerror <- which(gages$rivseg=="JB7_7070_0001")
gages$rivseg[fixerror] <- "JL7_7070_0001"
rm(fixerror)

# RETRIEVE GAGE DRAINAGE AREAS 
k <- 1
for (k in 1:nrow(gages)){
  siteNo <- (str_pad(gages$staid[k], 8, pad = 0))
  gagedata <- readNWISsite(siteNo)
  gages$DAsqmi[k] <- gagedata$drain_area_va
}
rm(gagedata)

# Find matching gage / segments, see if drainage areas compare

matches <- (merge(x=gages, y=all_segs, by.x="rivseg", by.y="rivseg"))
matches <- data.frame(matches$rivseg, matches$staid, matches$DAsqmi, matches$model_da)
colnames(matches)<- c("segment", "gage", "gageDA", "segDA")

# ERROR HAS BEEN RESOLVED ---------
# #it is improperly labeled in GIS -- the file reads JB7_7070_0001, but it is *JL*
# #fixed this in lines 34-35
# length(unique(matches$gage)) #134
# length(unique(gages$staid)) #135
# #find the missing gage 
# matched <- intersect(matches$gage, gages$staid)
# all <-  union(matches$gage, gages$staid)
# non.matched <- all[!all %in% matched]


matches$error <- 100*(matches$gageDA-matches$segDA) / (matches$gageDA)

goodmatch <- matches[which(abs(matches$error) <= 5),] #QA good segments
badmatch <- matches[-which(abs(matches$error) < 5),]

write.csv(goodmatch, "lowerror_segs.csv")
write.csv(badmatch, "higherror_segs.csv")


# CORRECTED BAD SEGMENTS FROM THE DRIVE: 

badmatch2 <- read.csv("corrected_higherror.csv", stringsAsFactors = FALSE)
badmatch2 <- data.frame(badmatch2$gage, badmatch2$gageDA, badmatch2$corrected_segs)
colnames(badmatch2) <- c('gage', 'gageDA', 'adjustedsegs')
badmatch2$adjustedsegs <- as.character(badmatch2$adjustedsegs)

i <- 1
for (i in 1:nrow(badmatch2)){
  studyseg <- badmatch2$adjustedsegs[i]
  
  RivSegStr <- strsplit(studyseg, "\\+")
  RivSegStr <- RivSegStr[[1]]
  num.segs <- length(RivSegStr)
  
  if (num.segs == 1) {
    DA<- all_segs$model_da[which(all_segs$rivseg==as.character(RivSegStr))]
  }
  else if (num.segs !=1){
    j <- 1
    DA<- 0
    for (j in 1:num.segs){
      segnum <- RivSegStr[j]
      newDA <- all_segs$model_da[which(all_segs$rivseg==as.character(segnum))]
      DA <- DA + newDA
    }
  }
  if (is.empty(DA)==TRUE){
    DA <- NA
  }
  badmatch2$segDA[i] <- DA
}

badmatch2$error <- 100*(as.numeric(badmatch2$gageDA) - as.numeric(badmatch2$segDA)) / as.numeric(badmatch2$gageDA)

goodmatchrnd2 <- badmatch2[which(abs(badmatch2$error) <= 5),]
badmatchrnd2 <- badmatch2[-which(abs(badmatch2$error) < 5),]

write.csv(goodmatchrnd2, "lowerror_segs_adjustpt2.csv")
write.csv(badmatchrnd2, "higherror_segs_adjustpt2.csv")

