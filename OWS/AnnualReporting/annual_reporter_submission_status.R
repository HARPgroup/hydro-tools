#Annual Reporting - Check Submission Status

library("hydrotools")
library("tidyr")

#Generate REST token for authentication
rest_uname = FALSE
rest_pw = FALSE
basepath ='/var/www/R'
source(paste0(basepath,'/config.R'))
tsdef_url <- "http://deq1.bse.vt.edu:81/d.dh/vwuds-eblast-not-submitted-export?rid%5B0%5D=12&propcode_op=%3D&propcode=email&propvalue_1_op=%21%3D&propvalue_1%5Bvalue%5D=&propvalue_1%5Bmin%5D=&propvalue_1%5Bmax%5D=&uid_raw=&mail_op=not&mail=null.email&startdate_op=between&startdate%5Bvalue%5D=2015-01-01&startdate%5Bmin%5D=2014-01-01&startdate%5Bmax%5D=2021-01-01&name_op=%3D&name="
ds <- RomDataSource$new(site)
ds$get_token()

test <- ds$auth_read(tsdef_url, content_type = "text/csv", delim = ",")

#load all facilities with report status since 2015 to current
facility_report_status <- test
#transform long table to wide table with column for each year - order by 5 year avg
data_all <- pivot_wider(data = facility_report_status, id_cols = c("Facility","Facility_Name","UserName","UserEmail","Onetime_login_URL","Firstname","Lastname", "Five_yr_avg_MGY"), names_from = "Reporting_Year", values_from = "Report_Status", values_fn = length)

write.csv(data_all, paste0(export_path,"xannual_reporter_submission_status.csv"), row.names = F)


#save script here = ~\Github\hydro-tools\OWS\AnnualReporting\annual_reporter_submission_status.R