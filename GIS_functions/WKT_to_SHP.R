library(rgeos) #readWKT()
library(rgdal) #readOGR()
library(raster) #bind()

#####################################################################################
# USER INPUTS
#####################################################################################
#WKT_layer <- read.csv('C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/hydro-tools/GIS_LAYERS/MinorBasins.csv')
WKT_layer <- read.table(file = 'https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/GIS_LAYERS/MinorBasins.csv', sep = ',', header = TRUE)

output_location <- "C:/Users/nrf46657/Desktop/shp_output/"
output_file <- "MinorBasins.shp"
#####################################################################################
#####################################################################################

WKT_layer$id <- as.numeric(rownames(WKT_layer))
WKT_layer.list <- list()

#i <- 1
for (i in 1:length(WKT_layer$code)) {
  print(paste("i = ",i," of ",length(WKT_layer$code),sep=''))
  print(as.character(WKT_layer$code[i]))
  if (WKT_layer$geom[i] == "") {
    WKT_layer_geom <- "NA"
  } else {
    WKT_layer_geom <- readWKT(WKT_layer$geom[i])
    WKT_layerProjected <- SpatialPolygonsDataFrame(WKT_layer_geom, data.frame('id'), match.ID = TRUE)
  }
  WKT_layer_name <- as.character(WKT_layer$name[i])
  WKT_layer_code <- as.character(WKT_layer$code[i])
  WKT_layerProjected@data$id <- as.character(i)
  WKT_layerProjected@data$name <- as.character(WKT_layer$name[i])
  WKT_layerProjected@data$code <- as.character(WKT_layer$code[i])
  WKT_layer.list[[i]] <- WKT_layerProjected

raster::shapefile(WKT_layerProjected, paste(output_location,WKT_layer_code,".shp",sep=""),overwrite=TRUE)
}

#-----------------------------------------------------------------
#IF MORE THAN ONE FEATURE-  BIND ALL shp files into single shp
WKT_layer_code <- as.character(WKT_layer$code)
WKT_feature.1 <- readOGR(paste(output_location,WKT_layer_code[1],'.shp',sep='')) 
WKT_feature.2 <- readOGR(paste(output_location,WKT_layer_code[2],'.shp',sep='')) 
WKT_BIND <- bind(WKT_feature.1,WKT_feature.2)
  
#x <- 1
for (x in 3:length(WKT_layer_code)){
  print(paste("Joining shape ",x," of ",length(WKT_layer$code),sep=''))
  WKT_layer_code_x <- WKT_layer_code[x]
  WKT.shp <- readOGR(paste(output_location,WKT_layer_code_x,'.shp',sep='')) 
  WKT_BIND <- bind(WKT_BIND,WKT.shp)
}
length(WKT_BIND)  

raster::shapefile(WKT_BIND, paste(output_location,output_file,sep=""),overwrite=TRUE)


#REMAINING STEPS IF GDB IS DESIRED
# 1) Load resulting .shp file in arcmap 
# 2) save as gdb
#-----------------------------------------------------------------
