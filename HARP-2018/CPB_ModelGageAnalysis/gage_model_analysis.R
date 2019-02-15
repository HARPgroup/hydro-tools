# Analysis of gages and river segments for Cheasapeake Bay Watershed
# Kelsey Reitz
# 2/15/2019

## load libraries
library(lubridate)
library(dplyr)
library(RCurl)
library(dataRetrieval)
library(stringr)

setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis")

#Pull Hailey's model data from the drive
wshed <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQCrbbMS-XgzxOG0btMencBB_6F7kTqTodlb9FQU67nlGCSKKmXsXZNC-G-sUghdI4ya8heUIugBMjn/pub?output=csv", stringsAsFactors = FALSE)
wshed <- data.frame(wshed$Flow, wshed$RiverSeg, wshed$MajBas, wshed$RiverName, wshed$AreaSqMi)
colnames(wshed) <- c("flow", "rivseg", "basin", "river", "DAsqmi")

wshed <- subset(wshed, wshed$basin!= "Susquehanna River Basin") #get rid of susquehanna stuff

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
colnames(common) <- c("segment", "flow", "modelbas", "river", "modelDA", "gageID", "gagename", "gageDA")

dacomp <- data.frame(common$segment, common$modelDA, common$gageID, common$gageDA)
colnames(dacomp) <- c("modelseg", "modelda", "gageID", "gageDA")

write.csv(dacomp, "Outlet_CPBsegs.csv")
