# Annual Reporting - Check Submission Status
# MUST BE ON VPN
library("hydrotools")
library("tidyr")
library("dplyr")
options(scipen = 999)

#Generate REST token for authentication
rest_uname = FALSE
rest_pw = FALSE
basepath ='/var/www/R'
source(paste0(basepath,'/config.R'))
ds <- RomDataSource$new(site)
ds$get_token()

##### LOAD DATA -------------------------------------------------------------------------------------------------
#load all facilities with report status since 2015 to current
tsdef_url <- "http://deq1.bse.vt.edu:81/d.dh/vwuds-eblast-not-submitted-export?rid%5B0%5D=12&propcode_op=%3D&propcode=email&propvalue_1_op=%21%3D&propvalue_1%5Bvalue%5D=&propvalue_1%5Bmin%5D=&propvalue_1%5Bmax%5D=&uid_raw=&mail_op=not&mail=null.email&startdate_op=between&startdate%5Bvalue%5D=2015-01-01&startdate%5Bmin%5D=2014-01-01&startdate%5Bmax%5D=2021-01-01&name_op=%3D&name="

facility_report_status_data <- ds$auth_read(tsdef_url, content_type = "text/csv", delim = ",")

#load in watershed consumptive use fractions
cu <- read.csv("U:/OWS/foundation_datasets/wsp/wsp2020/metrics_watershed_consumptive_use_frac.csv", stringsAsFactors = F)

#load in MGY from Annual Map Exports view
tsdef_url <- "http://deq1.bse.vt.edu:81/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=%3D&ftype=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=2014-01-01&tstime%5Bmax%5D=2021-12-31&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=91200&dh_link_admin_reg_issuer_target_id%5B1%5D=77498"

mp_MGY <- ds$auth_read(tsdef_url, content_type = "text/csv", delim = ",")

#trial, dont need
#tsdef_url <- "http://deq1.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=%3D&ftype=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=2014-01-01&tstime%5Bmax%5D=2021-12-31&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=91200&dh_link_admin_reg_issuer_target_id%5B1%5D=77498"
#mp_MGY2 <- ds$auth_read(tsdef_url, content_type = "text/csv", delim = ",")

#getOption('timeout')
#options(timeout=100000)
##"ows-awrr-map-export/wd_mgy?ftype_op=%3D&ftype=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=2014-01-01&tstime%5Bmax%5D=2021-12-31&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=91200&dh_link_admin_reg_issuer_target_id%5B1%5D=77498"
##"ows-awrr-map-export/wd_mgy?ftype_op=%3D&ftype=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=1982-01-01&tstime%5Bmax%5D=",eyear,"-12-31&bundle%5B0%5D=well&bundle%5B1%5D=intake")

##### MANIPULATE DATA ---------------------------------------------------------------------------------------------
#transform long report status table to wide table with column for each year
facility_report_status <- pivot_wider(data = facility_report_status_data, id_cols = c("Facility","Facility_Name", "Facility Use Type", "Latitude", "Longitude","UserName","UserEmail","Onetime_login_URL","Firstname","Lastname", "Five_yr_avg_MGY", "OWS Planner"), names_from = "Reporting_Year", values_from = "Submittal_ID", values_fn = length, names_sort = TRUE, names_prefix = "Submittal_")

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

xdata_sp_cont <- sqldf('SELECT *, substr(VAHydro_RSeg_Code, 17)"riverseg" 
                       FROM data_sp_cont')

#Append CU Frac column
data_sp_cont_cu <- sqldf('SELECT a.*, b.runid_13 AS "Current Consumptive Use Frac"
                         FROM xdata_sp_cont as a
                         LEFT OUTER JOIN cu as b
                         ON a.riverseg = b.riverseg') 

#dput(names(data_sp_cont_cu))  #print df names in a comma separated list

#Only show necessary columns
data_sp_cont_cu <- sqldf('SELECT "Facility_hydroid", "Fac_Name", "Use.Type", 
"MGY_2014",  a."Submittal_2014.01.01" AS "Submittal_2014", 
"MGY_2015",  a."Submittal_2015.01.01" AS "Submittal_2015", 
"MGY_2016",  a."Submittal_2016.01.01" AS "Submittal_2016", 
"MGY_2017",  a."Submittal_2017.01.01" AS "Submittal_2017", 
"MGY_2018",  a."Submittal_2018.01.01" AS "Submittal_2018", 
"MGY_2019",  a."Submittal_2019.01.01" AS "Submittal_2019", 
"MGY_2020",  a."Submittal_2020.01.01" AS "Submittal_2020", 
"MGY_2021",  a."Submittal_2021.01.01" AS "Submittal_2021", 
"FIPS.Code", "Locality", 
"Facility", "Facility_Name", "Latitude", "Longitude", "UserName", "UserEmail",
"Firstname", "Lastname", "Five_yr_avg_MGY", "OWS.Planner", "corrected_latitude", "corrected_longitude", 
"MinorBasin_Name", "VAHydro_RSeg_Name", 
"riverseg", "Current Consumptive Use Frac"
                         FROM data_sp_cont_cu AS a
                         ORDER BY "MGY_2021" DESC', method = data.frame) 


write.csv(data_sp_cont_cu, paste0(export_path,paste0("annual_reporter_submission_status_",Sys.Date(),".csv")), row.names = F)
