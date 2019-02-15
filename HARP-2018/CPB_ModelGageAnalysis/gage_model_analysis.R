# Analysis of gages and river segments for Cheasapeake Bay Watershed
# Kelsey Reitz
# 2/15/2019

## load libraries
library(lubridate)
library(dplyr)
library(RCurl)
library(dataRetrieval)
library(stringr)
library(rapportools)

setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis")

#Pull Hailey's model data from the drive
wshed <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQCrbbMS-XgzxOG0btMencBB_6F7kTqTodlb9FQU67nlGCSKKmXsXZNC-G-sUghdI4ya8heUIugBMjn/pub?output=csv", stringsAsFactors = FALSE)
wshed <- data.frame(wshed$Flow, wshed$RiverSeg, wshed$MajBas, wshed$RiverName, wshed$AreaSqMi)
colnames(wshed) <- c("flow", "rivseg", "basin", "river", "DAsqmi")
wshed$end <- str_sub(wshed$rivseg, start=10L, end=-1L) #identify 0000 and 0001 segments
wshed$rivseg <- as.character(wshed$rivseg)


wshed <- subset(wshed, wshed$basin!= "Susquehanna River Basin" & wshed$end != "0000" & wshed$end != "0001") #get rid of susquehanna stuff
rownames(wshed) <- 1:nrow(wshed)
# row 115 is a repeat of 114, searched in GIS and something is wrong with the 
# segment: seg PM7_4580_4820. Manually removing row 115
wshed <- wshed[-c(115),]
length(unique(wshed$rivseg))

# Pull gages from GIS file (p53 calibstats shapefile)
gages <- read.csv("Gage_stations.csv", stringsAsFactors = FALSE)
gages <- data.frame(as.numeric(as.character(gages$STAID)), gages$NAME, gages$CATCODE2)
gages$gages.NAME <- as.character(gages$gages.NAME)
gages$gages.CATCODE2 <- as.character(gages$gages.CATCODE2)
gages <- gages[(complete.cases(gages)),] #remove rows with na values
colnames(gages) <- c("staid", "name", "seg")

k <- 1
for (k in 1:nrow(gages)){
siteNo <- (str_pad(gages$staid[k], 8, pad = 0))
gagedata <- readNWISsite(siteNo)
gages$DAsqmi[k] <- gagedata$drain_area_va
}


common<- merge(wshed, gages, by.x="rivseg", by.y="seg")
colnames(common) <- c("segment", "flow", "modelbas", "river", "modelDA", "end_seg", "gageID", "gagename", "gageDA")

dacomp <- data.frame(common$segment, common$modelDA, common$gageID, common$gageDA)
colnames(dacomp) <- c("modelseg", "modelda", "gageID", "gageDA")

write.csv(dacomp, "Outlet_CPBsegs.csv")

#compare the segments with larger percent differences
dacomp$perdif <- 100*(dacomp$modelda - dacomp$gageDA) / dacomp$gageDA

largedif <- dacomp[which(dacomp$perdif > 2 | (dacomp$perdif) < -2), ]

#Troubleshoot step 1: pull upstream segments for large error segments

ModelSegments <- data.frame(matrix(nrow = nrow(wshed), ncol = 5))
colnames(ModelSegments)<- c('RiverSeg', 'Middle', 'Last', 'Downstream', "Upstream")
ModelSegments$RiverSeg <- wshed[,2]

i <- 1
for (i in 1:nrow(ModelSegments)){
  ModelSegments[i,2]<- str_sub(ModelSegments[i,1], start=5L, end=8L)
  ModelSegments[i,3]<- str_sub(ModelSegments[i,1], start=10L, end=-1L)
  i <- i + 1
}
# Determine Downstream Segment ----------
j <- 1
for (j in 1:nrow(ModelSegments)){
  Downstream <- which(ModelSegments$Middle==ModelSegments$Last[j])
  if (length(Downstream)==0){
    ModelSegments[j,4]  <- 'NA'
  }else if (length(Downstream)!=0){
    ModelSegments[j,4] <- as.character(ModelSegments[Downstream,1])
  }
  j<-j+1
}
# Determine Upstream Segment ------
m<-1
for (m in 1:nrow(ModelSegments)){
  Upstream <- which(ModelSegments$Downstream==ModelSegments$RiverSeg[m])
  NumUp <- ModelSegments$RiverSeg[Upstream]
  ModelSegments[m,5]<- paste(NumUp, collapse = '+')
  
  if (is.empty(ModelSegments[m,5])==TRUE){
    ModelSegments[m,5]<- 'NA'
  } 
  m<-m+1
}






# Begin Exerpt of other code

folder_location = 'C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis'
# Location of output destination
dir.create(paste0(folder_location, "\\GIS_Seg"), showWarnings = FALSE);
file_path <-"C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis\\GIS_Seg"



#Find the highest upstream segments (where water starts) ----------
#Lone wolf segments are segments with no head or tail
UpStartRow <- which(ModelSegments$Upstream=="NA")
UpStart <- ModelSegments[UpStartRow,]
rownames(UpStart)<- 1:nrow((UpStart))
LoneWolfRow <- as.vector(which(UpStart$Downstream=="NA" & UpStart$Upstream=="NA"))
LoneWolves <- UpStart[LoneWolfRow,]
UpStart <- UpStart[-c(LoneWolfRow),]


# Initalize variables for for loop that finds the next downstream segment ----------
i <- 1
alldata<- data.frame(matrix(ncol=2, nrow=nrow(UpStart)))


for (i in 1:nrow(UpStart)){
  
  #initialize variables for while loop  
  FirstSeg <- UpStart$RiverSeg[i]
  FirstSegRow<- which(ModelSegments$RiverSeg==FirstSeg)
  NextSeg <- ModelSegments$Downstream[FirstSegRow]
  x <- 2 #using x of 2 so that head segment is stored first, before entering while
  VarNextSeg <- data.frame(matrix(nrow=1, ncol=1))
  colnames(VarNextSeg) <- 'DownstreamSeg'
  VarNextSeg[1,1] <- FirstSeg
  
  #while loop adds the next downstream segment to the same row
  while (NextSeg != "NA"){
    NextSeg <- ModelSegments$Downstream[FirstSegRow]
    VarNextSeg[x,1] <- NextSeg
    FirstSeg <- NextSeg
    FirstSegRow<- which(ModelSegments$RiverSeg==FirstSeg)
    x <- x+1
  }
  VarNextSeg <-as.list(VarNextSeg[which(VarNextSeg!="NA"),]) #remove the last value from VarNextSeg
  #store VarNextSeg in alldata: has head and list of tails
  alldata[i,2] <- paste(VarNextSeg, collapse = '+')
  alldata[i,1] <- UpStart$RiverSeg[i] 
  i <- i + 1
}

# Initalize variables, then find the max number of river segments downstream of one head ----------
i <- 1
max<-1

for (i in 1:nrow(alldata)){
  DownSegs <- strsplit(alldata[i,2], "\\+")
  DownSegs <- t(as.data.frame(DownSegs[[1]]))
  maxnew <- length(DownSegs)
  if (maxnew>max){
    max<- maxnew
  }else 
    max <- max
  i <- i+1 
}

# Initialize variables for creating one large matrix of downstream segments -----------
j <- 1
newNA <- data.frame(matrix(nrow=1, ncol=1))
sepsegs <- data.frame(matrix(nrow=nrow(alldata), ncol=(max+2)))
colnames(sepsegs)<- 1:ncol(sepsegs)

for (j in 1:nrow(alldata)){
  DownSegs <- strsplit(alldata[j,2], "\\+")
  DownSegs <- data.frame(t(as.data.frame(DownSegs[[1]])))
  DownSegs[] <- lapply(DownSegs, as.character)
  
  numNA <- length(DownSegs)      #determine number of segments
  addNA <- as.numeric(max-numNA) #based on number of segments, may need to add NA values
  
  if (addNA !=0){                # add the required # of NAs and bind them to DownSegs
    newNA<- data.frame(t(rep(NA, addNA)))
    DownSegs<- cbind(DownSegs, newNA)
  }
  colnames(DownSegs)<- 1:ncol(DownSegs)
  
  #pull row j of all data, cbind it to DownSegs to make a complete row (stored in practice2)
  practice<- alldata[j,]
  practice2<- data.frame(cbind(practice, DownSegs)); 
  colnames(practice2)<- 1:ncol(practice2) 
  sepsegs[j,]<-practice2  #store results of practice2 in sepsegs for each j
  j <- j + 1
}

# Find furthest downstream segments, remove the lone wolves ----------
NoDownStream <- which(ModelSegments$Downstream=='NA')
NoDownStream <- ModelSegments[NoDownStream,]
rownames(NoDownStream) <- 1:nrow(NoDownStream)
LoneWolfRow <- as.vector(which(NoDownStream$Downstream=="NA" & NoDownStream$Upstream=="NA"))
LoneWolves <- NoDownStream[LoneWolfRow,]
NoDownStream <- NoDownStream[-c(LoneWolfRow),]

#pull only columns required for analysis (row 1=row 3, and row 2 is too condensed)
SegmentList <- sepsegs[,3:16]
colnames(SegmentList)<- 1:ncol(SegmentList)



# Create a csv for every tail with no outlet (that segment is the final outlet)----------
k <- 1
for (k in 1:nrow(NoDownStream)){
  RivSeg <- NoDownStream$RiverSeg[k]
  logic_SegList <- (SegmentList==RivSeg)
  logic_Rows<- which(apply(logic_SegList, 1, any))
  All_Segs <- SegmentList[logic_Rows,]
  All_Segs_Unique <- data.frame(wshed$rivseg)
  ASU <- data.frame(All_Segs_Unique)
  colnames(ASU)<- "RIVSEG"
  write.csv(ASU, paste0(file_path,"\\", RivSeg, "_Segs.csv"))
  k <- k + 1
}


#write a csv for the lone wolves
LoneSeg <- data.frame(LoneWolves$RiverSeg)
colnames(LoneSeg)<- "RIVSEG"
write.csv(LoneSeg, paste0(file_path,"\\Lone_Segs.csv"))

