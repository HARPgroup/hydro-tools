# 2/20/2019
# create code to compare drainage areas of all upstream segments from gages


setwd("C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis")

file_path <- "C:/Users/Kelsey/Desktop/GitHub/hydro-tools/HARP-2018/CPB_ModelGageAnalysis/GIS_Seg"
  
links <- read.csv("GageSeg_CPBsegs.csv", stringsAsFactors = FALSE)
all_segs <- read.csv("ModelSegs.csv", stringsAsFactors = FALSE)
upstream <- data.frame(read.csv(paste0(file_path, '/', 'All_Segs.csv'), stringsAsFactors = FALSE))

i <- 1
for (i in 1:nrow(links)){
readseg <- links$modelseg[i]
dataexist <- file.exists(paste0(file_path, '/', readseg, "_Segs.csv"))

if (dataexist == TRUE){
  findcsv <- read.csv(paste0(file_path, '/', readseg, "_Segs.csv"))
  numsegs <- nrow(findcsv)
  segs<- paste(findcsv$RIVSEG[1:numsegs], sep = " ", collapse = '+')
  j <- 1
  area <- 0
  for (j in 1:nrow(findcsv)){
    segda <- as.character(findcsv$RIVSEG[j])
    sumda <- area + all_segs$DAsqmi[which(all_segs$rivseg==segda)]
    area <- sumda
  }
  
} else if (dataexist == FALSE){ # need to go into allsegs csv to find upstream stuff
  segda <- as.character(readseg)
  find<- which(upstream[,]== segda, arr.ind=TRUE)
  row<- find[1] #identify the row of upstream that its in
  col<- find[2]
  if (row != 25){
  addareas <- upstream[row,2:col]
  addareas <- data.frame(t(addareas))
  
  if (row == 4){
    segs <- segda
    area <- all_segs$DAsqmi[which(all_segs$rivseg==segda)]
    
  } else if (row !=4){
  j <- 1
  area <- 0
  for (j in 1:nrow(addareas)){
    segda <- as.character(addareas[j,1])
    sumda <- area + all_segs$DAsqmi[which(all_segs$rivseg==segda)]
    area <- sumda
  }
  areaseg <- area
  if (nrow(addareas)==1){
    segs <- segda
    area <- areaseg
  }else if (nrow(addareas)!=1){
    segs<- paste(addareas[1:nrow(addareas),1], sep = " ", collapse = '+')
    area <- areaseg
  }
  }
  } else if (row==25){
    segs <- segda
    area <- all_segs$DAsqmi[which(all_segs$rivseg==segda)]
  }
}

links$segs[i] <- segs
links$totalarea[i] <- area
}

links$error <- 100*(links$gageDA - links$totalarea) / links$gageDA
write.csv(links, "DrainageAreas.csv")




# Pull gages from GIS file (p53 calibstats shapefile) ------------------------------
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

write.csv(gages, "all_gages.csv")


segments <- data.frame(all_segs$rivseg, all_segs$DAsqmi)
