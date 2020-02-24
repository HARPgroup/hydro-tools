library(rgeos) #readWKT()
library(ggplot2) #fortify()

MinorBasins <- read.csv('C:/Users/nrf46657/Desktop/MinorBasinsAll.csv')
class(MinorBasins)

#-----------------------------------------------------------------
MinorBasins$id <- as.numeric(rownames(MinorBasins))
MB.list <- list()

#i <- 1
for (i in 1:length(MinorBasins$hydroid)) {
  print(paste("i = ",i,sep=''))
  print(as.character(MinorBasins$hydroid[i]))
  MB_geom <- readWKT(MinorBasins$geom[i])
  MB_name <- as.character(MinorBasins$name[i])
  MBProjected <- SpatialPolygonsDataFrame(MB_geom, data.frame('id'), match.ID = TRUE)
  MBProjected@data$id <- as.character(i)
  MBProjected@data$name <- as.character(MinorBasins$basin[i])
  MBProjected@data$code <- as.character(MinorBasins$name[i])
  MB.list[[i]] <- MBProjected
#}
# MB <- do.call('rbind', MB.list)
# MB@data <- merge(MB@data, MinorBasins, by = 'id')
# MB@data <- MB@data[,-c(2:3)]
# MB.df <- fortify(MB, region = 'id')
# MB.df <- merge(MB.df, MB@data, by = 'id')
#-----------------------------------------------------------------
raster::shapefile(MBProjected, paste("C:/Users/nrf46657/Desktop/Code_Copies_2.21.20/GIS_LAYERS_NEW/WKTtest2/",MB_name,".shp",sep=""),overwrite=TRUE)
}

#-----------------------------------------------------------------
#MinorBasins$name
shp_path <- 'C:/Users/nrf46657/Desktop/Code_Copies_2.21.20/GIS_LAYERS_NEW/WKTtest2/'
TU <- readOGR(paste(shp_path,'TU.shp',sep=''))
RL <- readOGR(paste(shp_path,'RL.shp',sep=''))
OR <- readOGR(paste(shp_path,'OR.shp',sep=''))
EL <- readOGR(paste(shp_path,'EL.shp',sep=''))
ES <- readOGR(paste(shp_path,'ES.shp',sep=''))
PU <- readOGR(paste(shp_path,'PU.shp',sep=''))
RU <- readOGR(paste(shp_path,'RU.shp',sep=''))
YM <- readOGR(paste(shp_path,'YM.shp',sep=''))
JA <- readOGR(paste(shp_path,'JA.shp',sep=''))
MN <- readOGR(paste(shp_path,'MN.shp',sep=''))
PM <- readOGR(paste(shp_path,'PM.shp',sep=''))
NR <- readOGR(paste(shp_path,'NR.shp',sep=''))
YL <- readOGR(paste(shp_path,'YL.shp',sep=''))
BS <- readOGR(paste(shp_path,'BS.shp',sep=''))
PL <- readOGR(paste(shp_path,'PL.shp',sep=''))
YP <- readOGR(paste(shp_path,'YP.shp',sep=''))
PS <- readOGR(paste(shp_path,'PS.shp',sep=''))
OD <- readOGR(paste(shp_path,'OD.shp',sep=''))
JU <- readOGR(paste(shp_path,'JU.shp',sep=''))
JB <- readOGR(paste(shp_path,'JB.shp',sep=''))
JL <- readOGR(paste(shp_path,'JL.shp',sep=''))


x <- bind(TU,RL,OR,EL,ES,PU,RU,YM,JA,MN,PM,NR,YL,BS,PL,YP,PS,OD,JU,JB,JL)

raster::shapefile(x, paste("C:/Users/nrf46657/Desktop/Code_Copies_2.21.20/GIS_LAYERS_NEW/WKTtest2/","MinorBasins",".shp",sep=""),overwrite=TRUE)


#REMAINING STEPS 
# 1) Load resulting .shp file in arcmap 
# 2) save as gdb

#-----------------------------------------------------------------




