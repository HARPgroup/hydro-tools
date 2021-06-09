library(rgeos) #readWKT()
library(rgdal) #readOGR()
library(raster) #bind()
library('httr')
library('sqldf')
library('dplyr')
library('tidyr')
library(maptools)
#####################################################################################
# USER INPUTS
#####################################################################################
#WKT_layer <- read.csv('C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/hydro-tools/GIS_LAYERS/MinorBasins.csv')

#REPLACE WITH NWIS WATER QUANTITY WD EXPORT ANNUAL SECTION
#load variables
syear = 1982
eyear = 2020

##########################################################################
#LOAD CONFIG FILE
source(paste("/var/www/R/config.local.private", sep = ""))
localpath <- paste(github_location,"/USGS_Consumptive_Use", sep = "")

#LOAD from_vahydro() FUNCTION
source(paste(localpath,"/Code/VAHydro to NWIS/from_vahydro.R", sep = ""))
datasite <- "http://deq1.bse.vt.edu/d.dh"

# RETRIEVE WITHDRAWAL DATA
wd_annual_data <- list()

## year range
year_range <- format(seq(as.Date(paste0(syear,"/1/1")), as.Date(paste0(eyear,"/1/1")), "years"), format="%Y")

for (y in year_range) {
  print(paste0("PROCESSING YEAR: ", y))
  startdate <- paste(y, "-01-01",sep='')
  enddate <- paste(y, "-12-31", sep='')
  
  #with power
  export_view <- paste0("ows-awrr-map-export/wd_mgy?ftype_op=%3D&ftype=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498")
  output_filename <- "wd_mgy_export.csv"
  wd_annual <- from_vahydro(datasite,export_view,localpath,output_filename)
  
  wd_annual_data <- rbind(wd_annual_data, wd_annual)
}

############################################
#remove duplicates - GROUP BY USING MAX
wd_ann <- sqldf('SELECT "MP_hydroid","Hydrocode","Source.Type","MP.Name","Facility_hydroid","Facility","Use.Type","Year",max("Water.Use.MGY") AS "Water.Use.MGY","Latitude","Longitude","Locality","FIPS.Code" 
               FROM wd_annual_data
               WHERE Facility != "DALECARLIA WTP"
               GROUP BY "MP_hydroid","Hydrocode","Source.Type","MP.Name","Facility_hydroid","Facility","Use.Type","Year","Latitude","Longitude","Locality","FIPS.Code"
                ORDER BY "Water.Use.MGY" DESC ')

#rename columns & CONVERT LAT/LON COLUMNS TO WKT COLUMN
wd_mgy <- sqldf('SELECT MP_hydroid AS MP_ID,
                          Hydrocode AS Hcode,
                          "Source.Type" AS Source_Type,
                          "MP.Name" AS MP_Name,
                          Facility_hydroid AS Fac_ID,
                          Facility AS Fac_Name,
                          "USE.Type" AS UseType,
                          Year AS MGY,
                          "Water.Use.MGY" AS USE_MGY,
                          "POINT "||"("||Longitude||" "||Latitude||")" AS geom,
                          Latitude AS Lat,
                          Longitude AS Lon,
                          "FIPS.Code" AS FIPS
                       FROM wd_ann
                       ORDER BY Year
                       ') 

#place into export data frame
wd_mgy_export <- spread(data = wd_mgy, key = MGY, value = USE_MGY,sep = "_")

#save file
write.csv(wd_mgy_export,paste0(export_path,"withdrawal_annual.csv"), row.names = FALSE)


output_location <- paste0(export_path,"shp_output/")
output_file <- paste0("mp_wd_annual_",syear,"-",eyear,".shp")
#####################################################################################
#####################################################################################
wd_mgy_export <- read.csv(paste0(export_path,"withdrawal_annual.csv"))
WKT_layer <- wd_mgy_export
WKT_layer$id <- as.numeric(rownames(WKT_layer))
WKT_layer.list <- list()

#i <- 1
for (i in 1:length(WKT_layer$MP_ID)) {
#for (i in 1:5) {
  print(paste("i = ",i," of ",length(WKT_layer$MP_ID),sep=''))
  print(as.character(WKT_layer$MP_ID[i]))
  
  
  if (isTRUE(as.character(WKT_layer$geom[i]) == "")) {
    WKT_layer_geom <- "NA"
  } 
  else if (is.na(as.character(WKT_layer$geom[i]))) {
    WKT_layer_geom <- "NA"
  } 
  else {
    WKT_layer_geom <- readWKT(WKT_layer$geom[i])
    WKT_layerProjected <- SpatialPointsDataFrame(WKT_layer_geom, data.frame('id'), match.ID = TRUE)
  }
  
  
  #WKT_layer_name <- as.character(WKT_layer$name[i])
  WKT_layer_MP_ID <- as.character(WKT_layer$MP_ID[i])
  #WKT_layerProjected@data$id <- as.character(i)
  #WKT_layerProjected@data$MP_Name <- as.character(WKT_layer$MP_Name[i])
  #WKT_layerProjected@data$MP_ID <- as.character(WKT_layer$MP_ID[i])
  
  
  #SPECIFY ALL COLUMNS WE ARE KEEPING
  for(y in 1:length(names(WKT_layer))) {                                   # Head of for-loop
    WKT_layerProjected@data$newcol <- as.character(WKT_layer[i,y])
    colnames(WKT_layerProjected@data)[ncol(WKT_layerProjected@data)] <- names(WKT_layer[y])  # Rename column name
  }
  
  
  #WKT_layer.list[[i]] <- WKT_layerProjected

suppressWarnings(raster::shapefile(WKT_layerProjected, paste(output_location,"features/",i,"_",WKT_layer_MP_ID,".shp",sep=""),overwrite=TRUE))
         
}

#-----------------------------------------------------------------
#IF MORE THAN ONE FEATURE-  BIND ALL shp files into single shp
WKT_layer_MP_ID <- as.character(WKT_layer$MP_ID)
WKT_feature.1 <- readOGR(paste(output_location,"features/","1_",WKT_layer_MP_ID[1],'.shp',sep='')) 
WKT_feature.2 <- readOGR(paste(output_location,"features/","2_",WKT_layer_MP_ID[2],'.shp',sep='')) 
WKT_BIND <- bind(WKT_feature.1,WKT_feature.2)
  
#x <- 3
for (x in 3:length(WKT_layer_MP_ID)){
  print(paste("Joining shape ",x," of ",length(WKT_layer$MP_ID),sep=''))
  WKT_layer_MP_ID_x <- WKT_layer_MP_ID[x]
  WKT.shp <- readOGR(paste(output_location,"features/",WKT_layer$id[x],"_",WKT_layer_MP_ID_x,'.shp',sep='')) 
  WKT_BIND <- bind(WKT_BIND,WKT.shp)
}
length(WKT_BIND)  

raster::shapefile(WKT_BIND, paste(output_location,output_file,sep=""),overwrite=TRUE)


#REMAINING STEPS IF GDB IS DESIRED
# 1) Load resulting .shp file in arcmap 
# 2) save as gdb
#-----------------------------------------------------------------
