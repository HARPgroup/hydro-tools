# Annual Reporting - Missing Reporters - Check Submission Status
# MUST BE ON VPN
library("hydrotools")
library("tidyr")
library("dplyr")
library("sqldf")
options(scipen = 999)

#Generate REST token for authentication
rest_uname = FALSE
rest_pw = FALSE
basepath ='/var/www/R'
source(paste0(basepath,'/config.R'))
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

#set reporting year
ryear <- 2023

##### LOAD DATA -------------------------------------------------------------------------------------------------
#Note: the :81/d.dh is the modeling version of the site but accesses the same database data as the d.dh

#load all facilities with report status since 2015 to current
tsdef_url <- paste0(site,"/vwuds-eblast-not-submitted-export?rid%5B0%5D=12&propcode_op=%3D&propcode=&propvalue_1_op=%21%3D&propvalue_1%5Bvalue%5D=&propvalue_1%5Bmin%5D=&propvalue_1%5Bmax%5D=&uid_raw=&mail_op=not&mail=&startdate_op=between&startdate%5Bvalue%5D=2015-01-01&startdate%5Bmin%5D=2014-01-01&startdate%5Bmax%5D=",ryear,"-01-01&name_op=%3D&name=")

facility_report_status_data <- ds$auth_read(tsdef_url, content_type = "text/csv", delim = ",")

#load in watershed consumptive use fractions
cu <- read.csv("U:/OWS/foundation_datasets/wsp/wsp2020/metrics_watershed_consumptive_use_frac.csv", stringsAsFactors = F)

#load in MGY from Annual Map Exports view
tsdef_url <- paste0(site,"/ows-awrr-map-export/wd_mgy?ftype_op=%3D&ftype=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=2014-01-01&tstime%5Bmax%5D=",ryear,"-12-31&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=91200&dh_link_admin_reg_issuer_target_id%5B1%5D=77498")

mp_MGY <- ds$auth_read(tsdef_url, content_type = "text/csv", delim = ",")

#load planner coverage areas
plannerAreas <- read.csv("U:\\OWS\\GIS\\WSP\\PlannerCoverageAreaTable.csv")

##### MANIPULATE DATA ---------------------------------------------------------------------------------------------
#transform long report status table to wide table with column for each year
facility_report_status <- pivot_wider(data = facility_report_status_data, id_cols = c("Facility","Facility_Name", "Facility Use Type", "Latitude", "Longitude","UserName","UserEmail","Onetime_login_URL","Firstname","Lastname", "Five_yr_avg_MGY", "OWS Planner", "Reporting_Method"), names_from = "Reporting_Year", values_from = "Submittal_ID", values_fn = length, names_sort = TRUE, names_prefix = "Submittal_")
#facility_report_status <- facility_report_status[,-grep("12-31",names(facility_report_status))]

a <- mp_MGY %>% 
  dplyr::group_by(MP_hydroid, Year) %>% 
  dplyr::mutate(dupe = n()>1) %>%
  dplyr::filter(dupe == T)

#group by fac, sum the MGY 
facility_group <- sqldf('SELECT "Facility_hydroid", "Facility" AS "Fac_Name", "Use Type","FIPS Code", "Locality", "Year", SUM("Water Use MGY") AS "MGY", "OWS Planner" as "OWS Planner2"
                        FROM mp_MGY
                        GROUP BY "Facility_hydroid", "Year"')

#transform long table to wide table
facility_MGY <- pivot_wider(data = facility_group, id_cols = c("Facility_hydroid", "Fac_Name", "Use Type" ,"FIPS Code", "Locality", "OWS Planner2"), names_from = "Year", values_from = "MGY", names_sort = TRUE, names_prefix = "MGY_")

#join MGY to report status table
facility_status_MGY <- sqldf('SELECT *
                             FROM facility_MGY as b
                             LEFT OUTER JOIN facility_report_status as a
                             ON a.Facility = b.Facility_hydroid')

write.csv(facility_status_MGY, paste0(export_path,"xxannual_reporter_submission_status.csv"), row.names = F)

##### run spatial containment FOUNDATION3_CONTAINING_POLYGONS.R --------------------------------------------------
# source(paste0(github_location,"/vahydro/R/wsp/wsp2020/FOUNDATION3_CONTAINING_POLYGONS.R"))

#LOAD FUNCTIONS AND GDB FILES
source("/var/www/R/config.local.private"); 
source(paste(hydro_tools_location,'/GIS_functions/GIS_functions.R', sep = ''));

MinorBasins_path <- paste(github_location,'/HARParchive/GIS_layers/MinorBasins.gdb', sep = '')
MinorBasins_layer <- 'MinorBasins'

VAHydro_RSegs_path <- paste(github_location,'/HARParchive/GIS_layers/VAHydro_RSegs.gdb', sep = '')
VAHydro_RSegs_layer <- 'VAHydro_RSegs'

# tidal_boundary_path <- 'hydro-tools/GIS_LAYERS/'
# tidal_boundary_layer <- 

#LOAD FIPS CENTROIDS
fips_centroids <- read.csv(paste("https://deq1.bse.vt.edu/d.dh/usafips_centroid_export",sep=""))

# join fips centroids - explicitly name columns
fips_join <- sqldf('SELECT a.*,
                  b.fips_hydroid,
                  b.fips_name,
                  b.fips_latitude,
                  b.fips_longitude,
                  b.fips_centroid
                  FROM facility_status_MGY AS a
                  LEFT OUTER JOIN fips_centroids AS b
                  ON (a."FIPS Code" = b.fips_code)')  

# #Remove duplicate fips_code column
# fips_join <- fips_join[,-which(colnames(fips_join)=="fips_code..27")[1]]


#Set geoms equal to fips centroid if NA or outside of VA bounding box 
data_sp <- sqldf("SELECT *,
              CASE
                WHEN Latitude IS NULL THEN fips_latitude
                WHEN Latitude < 35 THEN fips_latitude
                WHEN Latitude > 41 THEN fips_latitude
                ELSE Latitude
              END AS corrected_latitude,
              CASE
                WHEN Longitude IS NULL THEN fips_longitude
                WHEN Longitude > -75 THEN fips_longitude
                WHEN Longitude < -84 THEN fips_longitude
                ELSE Longitude
              END AS corrected_longitude
              FROM fips_join")

## This section adds in MinorBasin columns
coordinates(data_sp) <- c("corrected_longitude", "corrected_latitude") #sp_contain_mb() requires a coordinates column
data_sp_cont <- sp_contain_mb(MinorBasins_path,MinorBasins_layer,data_sp)
data_sp_cont <- data.frame(data_sp_cont)

##This section adds in Riverseg columns
coordinates(data_sp_cont) <- c("corrected_longitude", "corrected_latitude") #sp_contain_vahydro_rseg() requires a coordinates column
data_sp_cont <- sp_contain_vahydro_rseg(VAHydro_RSegs_path,VAHydro_RSegs_layer,data_sp_cont)
data_sp_cont <- data.frame(data_sp_cont)

########## End Spatial Containment ##############


data_sp_cont <- sqldf('SELECT *, substr(VAHydro_RSeg_Code, 17)"riverseg" 
                       FROM data_sp_cont')

#Append CU Frac column
data_sp_cont_cu <- sqldf('SELECT a.*, b.runid_13 AS "Current Consumptive Use Frac"
                         FROM data_sp_cont as a
                         LEFT OUTER JOIN cu as b
                         ON a.riverseg = b.riverseg') 

#reassign Planner names if Planner assignment columns are yielding NAs
data_addPlanners <- sqldf('SELECT a.*, b.Planner
                     FROM data_sp_cont_cu AS a
                     LEFT OUTER JOIN plannerAreas AS b
                     on a."FIPS.Code" = b.FIPS')

#Only show necessary columns
#note for next reporting cycle add columns: "MGY_2024",  a."Submittal_2024.01.01" AS "Submittal_2024",
#note for next reporting  cycle, ORDER BY MGY_2024
output_reporters <- sqldf('
SELECT "Facility_hydroid", "Fac_Name", "Use.Type", "Five_yr_avg_MGY", "Planner", "Reporting_Method",
"UserName", "UserEmail", "Firstname", "Lastname", "Locality",
"MGY_2023",  a."Submittal_2023.01.01" AS "Submittal_2023",
"MGY_2022",  a."Submittal_2022.01.01" AS "Submittal_2022", 
"MGY_2021",  a."Submittal_2021.01.01" AS "Submittal_2021", 
"MGY_2020",  a."Submittal_2020.01.01" AS "Submittal_2020", 
"MGY_2019",  a."Submittal_2019.01.01" AS "Submittal_2019", 
"MGY_2018",  a."Submittal_2018.01.01" AS "Submittal_2018", 
"MGY_2017",  a."Submittal_2017.01.01" AS "Submittal_2017", 
"MGY_2015",  a."Submittal_2015.01.01" AS "Submittal_2015", 
"MGY_2016",  a."Submittal_2016.01.01" AS "Submittal_2016", 
"FIPS.Code", 
"Latitude", "Longitude",  "corrected_latitude", "corrected_longitude", 
"MinorBasin_Name", "VAHydro_RSeg_Name", 
"riverseg", "Current Consumptive Use Frac"

FROM data_addPlanners AS a

ORDER BY "MGY_2023" DESC')

write.csv(output_reporters, paste0(export_path,paste0("annual_reporter_submission_status_",Sys.Date(),".csv")), row.names = F)

dim(output_reporters)

cols <- c(7:30)
sapply(output_reporters[,cols],function(x) {sum(is.na(x))})
