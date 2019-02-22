# 2/20/2019
# create code to compare drainage areas of all upstream segments from gages

# LOAD LIBRARIES AND SET WORKSPACES -----------------------
library(dataRetrieval)

setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis")
file_path <- "C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis/GIS_Seg"
  
# LOAD AND FORMAT DATA FROM FOLDER ---------------------
all_segs <- read.csv("ModelSegs.csv", stringsAsFactors = FALSE) #pull a list of the segments that we're analyzing (came from the model)
upstreamlist <- data.frame(read.csv(paste0(file_path, '/', 'All_Segs.csv'), stringsAsFactors = FALSE)) # get the upstream/downstream list

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

# RETRIEVE GAGE DRAINAGE AREAS 
k <- 1
for (k in 1:nrow(gages)){
  siteNo <- (str_pad(gages$staid[k], 8, pad = 0))
  gagedata <- readNWISsite(siteNo)
  gages$DAsqmi[k] <- gagedata$drain_area_va
}


# Find matching gage / segments, see if drainage areas compare

matches <- (merge(x=gages, y=all_segs, by.x="rivseg", by.y="rivseg"))
matches <- data.frame(matches$rivseg, matches$staid, matches$DAsqmi.x, matches$DAsqmi.y)
colnames(matches)<- c("segment", "gage", "gageDA", "segDA")


matches$error <- 100*(matches$gageDA-matches$segDA) / (matches$gageDA)

goodmatch <- matches[which(abs(matches$error) < 5),] #QA good segments
badmatch <- matches[-which(abs(matches$error) < 5),]

write.csv(goodmatch, "lowerror_segs.csv")
write.csv(badmatch, "higherror_segs.csv")


# CORRECTED BAD SEGMENTS FROM THE DRIVE: 

badmatch2 <- read.csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRY1mSIxHLrKXiovTYrh--GUFAbMereM2tUd-FdnjADDhmp0JKwA9bq8jYOv4e-Eg6ZmRRTQObfWjZG/pub?output=csv', stringsAsFactors = FALSE)
badmatch2 <- data.frame(badmatch2$gage, badmatch2$gageDA, badmatch2$adjustedseg)
colnames(badmatch2) <- c('gage', 'gageDA', 'adjustedsegs')
badmatch2$adjustedsegs <- as.character(badmatch2$adjustedsegs)

i <- 1
for (i in 1:nrow(badmatch2)){
  studyseg <- badmatch2$adjustedsegs[i]
  
  RivSegStr <- strsplit(studyseg, "\\+")
  RivSegStr <- RivSegStr[[1]]
  num.segs <- length(RivSegStr)
  
  if (num.segs == 1) {
    DA<- all_segs$DAsqmi[which(all_segs$rivseg==as.character(RivSegStr))]
  }
  else if (num.segs !=1){
    j <- 1
    DA<- 0
    for (j in 1:num.segs){
      segnum <- RivSegStr[j]
      newDA <- all_segs$DAsqmi[which(all_segs$rivseg==as.character(segnum))]
      DA <- DA + newDA
    }
  }
  if (is.empty(DA)==TRUE){
    DA <- NA
  }
  badmatch2$segDA[i] <- DA
}

badmatch2$error <- 100*(badmatch2$gageDA - badmatch2$segDA) / badmatch2$gageDA

goodmatchrnd2 <- badmatch2[which(abs(badmatch2$error) < 10),]
badmatchrnd2 <- badmatch2[-which(abs(badmatch2$error) < 10),]

write.csv(goodmatchrnd2, "lowerror_segs_adjustpt2.csv")
write.csv(badmatchrnd2, "higherror_segs_adjustpt2.csv")







# OLD CODE BEGINS HERE DO NOT DO NOT DO NOT RUN THIS -- it is ineffective. 
badmatch3 <- badmatchrnd2[,1:4]
rownames(badmatch3) <- 1:nrow(badmatch3)

i <- 1
for (i in 1:nrow(badmatchrnd2)){
readseg <- badmatchrnd2$adjustedsegs[i]

segda <- as.character(readseg)
find<- which(upstreamlist[,]== segda, arr.ind=TRUE)
if (sum(find)==0){
  newDA <- NA
  
  }else if (sum(find)!=0){
    row<- find[1] #identify the row of upstream that its in
    col<- find[2]
    addareas <- upstreamlist[row,2:col]
    addareas <- data.frame(t(addareas))
    j <- 1
    for (j in 1:nrow(addareas)){
      newarea <- all_segs$DAsqmi[which(all_segs$rivseg==addareas[j,1])]
      addareas$area[j] <- newarea
    }
    newDA <- sum(addareas$area)
}
badmatch3$newDA[i] <- newDA
}

badmatch3$newerror <- 100*(badmatch3$gageDA - badmatch3$newDA) / badmatch3$gageDA
better <- badmatch3[which(abs(badmatch3$newerror) < 20),]













