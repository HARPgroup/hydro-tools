library(rgeos) #readWKT()
library(rgdal) #readOGR()
library(raster) #bind()
library('httr')
library('sqldf')
library('dplyr')
library('tidyr')

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
wd_mgy <- sqldf('SELECT MP_hydroid,
                          Hydrocode,
                          "Source.Type" AS Source_Type,
                          "MP.Name" AS MP_Name,
                          Facility_hydroid,
                          Facility AS Facility_Name,
                          "USE.Type" AS Use_Type,
                          Year,
                          "MGY" AS "Water Use Unit",
                          "Water.Use.MGY" AS MGY,
                          "POINT "||"("||Longitude||" "||Latitude||")" AS geom,
                          Latitude,
                          Longitude,
                          "FIPS.Code" AS FIPS_code
                       FROM wd_ann
                       ORDER BY Year
                       ') 

#place into export data frame
wd_mgy_export <- spread(data = wd_mgy, key = Year, value = MGY,sep = "_")

#save file
write.csv(wd_mgy_export,paste0(export_path,"withdrawal_annual.csv"), row.names = FALSE)


output_location <- paste0(export_path,"shp_output/")
output_file <- "MinorBasins.shp"
#####################################################################################
#####################################################################################
WKT_layer <- wd_mgy_export
WKT_layer$id <- as.numeric(rownames(WKT_layer))
WKT_layer.list <- list()

#i <- 1
for (i in 1:length(WKT_layer$MP_hydroid)) {
  print(paste("i = ",i," of ",length(WKT_layer$MP_hydroid),sep=''))
  print(as.character(WKT_layer$MP_hydroid[i]))
  if (WKT_layer$geom[i] == "") {
    WKT_layer_geom <- "NA"
  } else {
    WKT_layer_geom <- readWKT(WKT_layer$geom[i])
    WKT_layerProjected <- SpatialPointsDataFrame(WKT_layer_geom, data.frame('id'), match.ID = TRUE)
  }
  #WKT_layer_name <- as.character(WKT_layer$name[i])
  WKT_layer_MP_hydroid <- as.character(WKT_layer$MP_hydroid[i])
  WKT_layerProjected@data$id <- as.character(i)
  WKT_layerProjected@data$MP_Name <- as.character(WKT_layer$MP_Name[i])
  WKT_layerProjected@data$MP_hydroid <- as.character(WKT_layer$MP_hydroid[i])
  #EXPLICITLY SPECIFY ALL COLUMNS WE ARE KEEPING (MP NAME, FAC NAME, HYDROIDs, USE TYPE, LOCALITY, ETC)
  WKT_layer.list[[i]] <- WKT_layerProjected

raster::shapefile(WKT_layerProjected, paste(output_location,"features/",i,"_",WKT_layer_MP_hydroid,".shp",sep=""),overwrite=TRUE)
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
