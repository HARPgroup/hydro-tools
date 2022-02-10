#Annual Reporting - Check Submission Status

library("hydrotools")
library("tidyr")
options(scipen = 999)

#Generate REST token for authentication
rest_uname = FALSE
rest_pw = FALSE
basepath ='/var/www/R'
source(paste0(basepath,'/config.R'))
tsdef_url <- "http://deq1.bse.vt.edu:81/d.dh/vwuds-eblast-not-submitted-export?rid%5B0%5D=12&propcode_op=%3D&propcode=email&propvalue_1_op=%21%3D&propvalue_1%5Bvalue%5D=&propvalue_1%5Bmin%5D=&propvalue_1%5Bmax%5D=&uid_raw=&mail_op=not&mail=null.email&startdate_op=between&startdate%5Bvalue%5D=2015-01-01&startdate%5Bmin%5D=2014-01-01&startdate%5Bmax%5D=2021-01-01&name_op=%3D&name="
ds <- RomDataSource$new(site)
ds$get_token()

#load all facilities with report status since 2015 to current
facility_report_status_data <- ds$auth_read(tsdef_url, content_type = "text/csv", delim = ",")

#transform long table to wide table with column for each year - order by 5 year avg
facility_report_status <- pivot_wider(data = facility_report_status_data, id_cols = c("Facility","Facility_Name", "Facility Use Type", "Latitude", "Longitude","UserName","UserEmail","Onetime_login_URL","Firstname","Lastname", "Five_yr_avg_MGY", "OWS Planner", "Locality", "fips_code"), names_from = "Reporting_Year", values_from = "Submittal_ID", values_fn = length, names_sort = TRUE)


write.csv(facility_report_status, paste0(export_path,"xannual_reporter_submission_status.csv"), row.names = F)

##### MUST BE ON VPN AFTER THIS LINE

##### run spatial containment FOUNDATION3_CONTAINING_POLYGONS.R

#load in watershed consumptive use fractions
cu <- read.csv("U:/OWS/foundation_datasets/wsp/wsp2020/metrics_watershed_consumptive_use_frac.csv", stringsAsFactors = F)

xdata_sp_cont <- sqldf('SELECT *, substr(VAHydro_RSeg_Code, 17)"riverseg" 
                       FROM data_sp_cont')

#Append CU Frac column
data_sp_cont_cu <- sqldf('SELECT a.*, b.runid_13 AS "Current Consumptive Use Frac"
                         FROM xdata_sp_cont as a
                         LEFT OUTER JOIN cu as b
                         ON a.riverseg = b.riverseg') 

write.csv(data_sp_cont_cu, paste0(export_path,"annual_reporter_submission_status.csv"), row.names = F)

#save script here = ~\Github\hydro-tools\OWS\AnnualReporting\annual_reporter_submission_status.R