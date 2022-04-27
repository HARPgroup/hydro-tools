library(rgeos) #readWKT()
library(rgdal) #readOGR()
library(raster) #bind()
library('httr')
library('sqldf')
library('dplyr')
library('tidyr')
library(maptools)
library("beepr")

# USER INPUTS #########################################################################
#WKT_layer <- read.csv('C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/hydro-tools/GIS_LAYERS/MinorBasins.csv')

#load variables
syear = 1982
eyear = 2020
five.year.range <- c(2016, 2017, 2018, 2019, 2020)

## LOAD CONFIG FILE ###################################################################
source(paste("/var/www/R/config.local.private", sep = ""))
localpath <- paste(github_location,"/USGS_Consumptive_Use", sep = "")

#LOAD from_vahydro() FUNCTION
source(paste(localpath,"/Code/VAHydro to NWIS/from_vahydro.R", sep = ""))
datasite <- "http://deq1.bse.vt.edu:81/d.dh"

#PART 1 - ANNUAL ####################################################################
### RETRIEVE ANNUAL WITHDRAWAL DATA #################################################
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

firstcol = which(colnames(wd_mgy_export)=="MGY_2016")
lastcol = which(colnames(wd_mgy_export)=="MGY_2020")

wd_mgy_export$multi_yr_avg <- round((rowMeans(wd_mgy_export[firstcol:lastcol], na.rm = TRUE, dims = 1)),2)
#save file
write.csv(wd_mgy_export,paste0(export_path,"withdrawal_annual.csv"), row.names = FALSE)


output_location <- paste0(export_path,"shp_output/")
output_file <- paste0("mp_wd_annual_",syear,"-",eyear,".shp")
WKT_layer <- wd_mgy_export

#### CSV TO SHAPEFILE ############################################################
# Read the .csv file
#wd_mgy_export <- read.csv(paste0(export_path,"withdrawal_annual_",syear,"-",eyear,".csv"), stringsAsFactors = F)
WKT_layer$id <- as.numeric(rownames(WKT_layer))

# look at the data structure
str(WKT_layer)

# view column names
names(WKT_layer)

# SpatialPointsDataFrame does not accept NA values in coordinate fields
r <- sqldf('SELECT CASE
              WHEN Lat = ""
              THEN 99
              WHEN Lat IS NULL
              THEN 99
              ELSE Lat
              END AS Lat,
              CASE
              WHEN Lon = ""
              THEN 99
              WHEN Lon IS NULL
              THEN 99
              ELSE Lon
              END AS Lon,
              *
           FROM WKT_layer
           ')

# first, convert the data.frame to spdf
coordinates(r) <- ~Lon+Lat

# second, assign the CRS in one of two ways
crs(r) <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs 
                 +ellps=WGS84 +towgs84=0,0,0"
plot(r, 
     main=paste0("Map of Withdrawal Points: ",syear,"-",eyear))
str(r)
# write a shapefile
writeOGR(r, "C:/Users/maf95834/Documents/shp_output",
         paste0("mp_wd_annual_",syear,"-",eyear), driver="ESRI Shapefile", overwrite_layer = T)



#PART 2 - MONTHLY ####################################################################
### RETRIEVE MONTHLY WITHDRAWAL DATA #################################################
#load variables
syear = 2020
eyear = 2020
wd_monthly_data <- list()

## year range
year_range <- format(seq(as.Date(paste0(syear,"/1/1")), as.Date(paste0(eyear,"/1/1")), "years"), format="%Y")

for (y in year_range) {
  print(paste0("PROCESSING YEAR: ", y))
  startdate <- paste(y, "-01-01",sep='')
  enddate <- paste(y, "-12-31", sep='')
  
  #with power
  export_view <- paste0("ows-annual-report-map-exports-monthly-export/wd_mgm?ftype_op=%3D&ftype=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498")
  output_filename <- "wd_mgm_export.csv"
  wd_monthly <- from_vahydro(datasite,export_view,localpath,output_filename)
  
  wd_monthly_data <- rbind(wd_monthly_data, wd_monthly)
}

#remove duplicates - GROUP BY USING MAX
wd_mon <- sqldf('SELECT "MP_hydroid","Hydrocode","Source.Type","MP.Name","Facility_hydroid","Facility","Use.Type","Year","Month", max("Water.Use.MGM") AS "Water.Use.MGM","Latitude", "Longitude","Locality","FIPS.Code" 
               FROM wd_monthly_data
               WHERE Facility != "DALECARLIA WTP"
               GROUP BY "MP_hydroid","Hydrocode","Source.Type","MP.Name","Facility_hydroid", "Facility","Use.Type","Year","Month","Latitude","Longitude","Locality","FIPS.Code"
                ORDER BY "Water.Use.MGM" DESC ')
#save file
write.csv(wd_mon,paste0(export_path,"withdrawal_monthly_",syear,"-",eyear,".csv"), row.names = FALSE)

#rename columns & CONVERT LAT/LON COLUMNS TO WKT COLUMN
wd_mgm <- sqldf('SELECT MP_hydroid AS MP_ID,
                          Hydrocode AS Hcode,
                          "Source.Type" AS Source_Type,
                          "MP.Name" AS MP_Name,
                          Facility_hydroid AS Fac_ID,
                          Facility AS Fac_Name,
                          "Use.Type" AS UseType,
                          Year,
                          Month,
                          CASE 
                          WHEN Month = 1
                          THEN "Jan"
                          WHEN Month = 2
                          THEN "Feb"
                          WHEN Month = 3
                          THEN "Mar"
                          WHEN Month = 4
                          THEN "Apr"
                          WHEN Month = 5
                          THEN "May"
                          WHEN Month = 6
                          THEN "Jun"
                          WHEN Month = 7
                          THEN "Jul"
                          WHEN Month = 8
                          THEN "Aug"
                          WHEN Month = 9
                          THEN "Sep"
                          WHEN Month = 10
                          THEN "Oct"
                          WHEN Month = 11
                          THEN "Nov"
                          WHEN Month = 12
                          THEN "Dec"
                          ELSE "No Date"
                          END AS Month2,
                          "Water.Use.MGM" AS USE_MGM,
                          "POINT "||"("||Longitude||" "||Latitude||")" AS geom,
                          Latitude AS Lat,
                          Longitude AS Lon,
                          "FIPS.Code" AS FIPS
                       FROM wd_mon
                       ORDER BY MP_hydroid, Year, Month
                       ') 

#transform from long to wide table
wd_mgm_export <- pivot_wider(data = wd_mgm, id_cols = c("MP_ID","Hcode", "Source_Type", "MP_Name", "Fac_ID", "Fac_Name","UseType","geom","Lat","Lon", "FIPS"), names_from = c("Month2", "Year"), values_from = "USE_MGM")

#save file
write.csv(wd_mgm_export,paste0(export_path,"withdrawal_monthly_",syear,"-",eyear,".csv"), row.names = FALSE)

output_location <- paste0(export_path,"shp_output/")
output_file <- paste0("mp_wd_monthly_",syear,"-",eyear,".shp")
WKT_layer <- wd_mgm_export

### CSV TO SHAPEFILE --------------------------------------------------------------------------
# Read the .csv file
#wd_mgm_export <- read.csv(paste0(export_path,"withdrawal_monthly_",syear,"-",eyear,".csv"), stringsAsFactors = F)
WKT_layer$id <- as.numeric(rownames(WKT_layer))

# look at the data structure
str(WKT_layer)

# view column names
names(WKT_layer)

# SpatialPointsDataFrame does not accept NA values in coordinate fields
r <- sqldf('SELECT CASE
              WHEN Lat = ""
              THEN 99
              WHEN Lat IS NULL
              THEN 99
              ELSE Lat
              END AS Lat,
              CASE
              WHEN Lon = ""
              THEN 99
              WHEN Lon IS NULL
              THEN 99
              ELSE Lon
              END AS Lon,
              *
           FROM WKT_layer
           ')

# first, convert the data.frame to spdf
coordinates(r) <- ~Lon+Lat

# second, assign the CRS in one of two ways
crs(r) <- "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs 
                 +ellps=WGS84 +towgs84=0,0,0"
plot(r, 
     main=paste0("Map of Withdrawal Points: ",syear,"-",eyear))
str(r)
# write a shapefile
writeOGR(r, "C:/Users/maf95834/Documents/shp_output",
         paste0("mp_wd_monthly_",syear,"-",eyear), driver="ESRI Shapefile", overwrite_layer = T)
print(paste0("PROCESS COMPLETE: ",syear," - ",eyear," MONTHLY GIS LAYER"))
#REMAINING STEPS IF GDB IS DESIRED
# 1) In ArcMap - Load resulting .shp file in arcmap 
# 2) Save as gdb - import multiple feature class files to .gdb 
# 3) Set the coordinate reference system in the  data layer's property page in ArcCatalog
# 4) Export Data Table to .gdb for all layers
# 5) Clip the layers to the VA extent
# Final .gdb should have 6 clipped layers and 6 complete data tables
