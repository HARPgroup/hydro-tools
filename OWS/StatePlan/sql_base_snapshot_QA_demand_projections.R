require("dplyr")
require('httr')
require("sqldf")

#pulls directly from map export view BUT locality = NA for all rows

 y = 2018
   print(y)
   startdate <- paste(y, "-01-01",sep='')
   enddate <- paste(y, "-12-31", sep='')
 vwp <- '91200'
 gwp <- '65668'
 vwuds <- '77498'
 
 #no filter on permit authority
   localpath <- tempdir()
   filename <- paste("data.all_",y,".csv",sep="")
   destfile <- paste(localpath,filename,sep="\\")
   download.file(paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=hydropower&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=2018-01-01&tstime%5Bmax%5D=2018-12-31&bundle%5B%5D=well&bundle%5B%5D=intake&dh_link_admin_reg_issuer_target_id%5B%5D=65668&dh_link_admin_reg_issuer_target_id%5B%5D=91200&dh_link_admin_reg_issuer_target_id%5B%5D=77498",sep=""), destfile = destfile, method = "libcurl")
   data.all <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")

   data_all <- data.all
 #VWP
   localpath <- tempdir()
   filename <- paste("data.vwp_",y,".csv",sep="")
   destfile <- paste(localpath,filename,sep="\\")
   download.file(paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=hydropower&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=",vwp,sep=""), destfile = destfile, method = "libcurl")
   data_vwp <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
 #GWP
   localpath <- tempdir()
   filename <- paste("data.gwp_",y,".csv",sep="")
   destfile <- paste(localpath,filename,sep="\\")
   download.file(paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=hydropower&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&dh_link_admin_reg_issuer_target_id%5B0%5D=",gwp,sep=""), destfile = destfile, method = "libcurl")
   data_gwp <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")

# path <- "directory location"
# #pulls in MP level data from map exports view 
# data_all <- read.csv(file = paste0(path, "data_all.csv"), header=TRUE, sep=",")
# data_vwp <- read.csv(file = paste0(path, "data_vwp.csv"), header=TRUE, sep=",")
# data_gwp <- read.csv(file = paste0(path, "data_gwp.csv"), header=TRUE, sep=",")


data_base <- sqldf("select a.*, case when b.MP_hydroid IS NOT NULL 
                  THEN 'GWP'
                  ELSE NULL END as GWP_permit, 
                  case when c.MP_hydroid IS NOT NULL
                  THEN 'VWP'
                  ELSE NULL END as VWP_permit
                  FROM data_all as a
                  left outer join data_gwp as b
                  on (a.MP_hydroid = b.MP_hydroid)
                   left outer join data_vwp as c
                   on (a.MP_hydroid = c.MP_hydroid)")

count_has_permit <- sqldf("Select count(MP_hydroid)
                      from data_base
                      where GWP_permit IS NOT NULL")

data_base_facility <- sqldf("SELECT Facility_hydroid, GWP_permit, VWP_permit
                            FROM data_base
                            GROUP BY Facility_hydroid")
write.csv(data_base, file = paste0(path, "data_base.csv"))
  
#here's a rough base for how to structure 2 dfs and join them on a common field
  # df <-sqldf("Select *
  #           FROM permit_list as a
  #           left outer join other_permit_list as b
  #          on (a.permit_id = b.permit_id)")
  # 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#   
#   
#   
#   #remove duplicates (keeps one row)
#   data <- distinct(data, HydroID, .keep_all = TRUE)
#   #exclude dalecarlia
#   data <- data[-which(data$Facility=='DALECARLIA WTP'),]
#   
#   if (length(which(data$Use.Type=='facility')) > 0) {
#     data <- data[-which(data$Use.Type=='facility'),]
#   }
#   #rename columns
#   colnames(data) <- c('HydroID', 'Hydrocode', 'Source_Type',
#                       'MP_Name', 'Facility', 'Use_Type', 'Year',
#                       'mgy', 'mgd', 'lat', 'lon', 'locality')
#   #make use type values lowecase
#   data$Use_Type <- str_to_lower(data$Use_Type)
#   #change 'Well' and 'Surface Water Intake' values in source_type column to match report headers
#   levels(data$Source_Type) <- c(levels(data$Source_Type), "Groundwater", "Surface Water")
#   data$Source_Type[data$Source_Type == 'Well'] <- 'Groundwater'
#   data$Source_Type[data$Source_Type == 'Surface Water Intake'] <- 'Surface Water'
#   data$Use_Type[data$Use_Type == 'industrial'] <- 'manufacturing'
#   data$mgd = data$mgy / 365.0
# 
#   
#   download.file(paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200",sep=""), destfile = destfile, method = "libcurl")  
#   datap <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
#   head(data_pi)
# sqldf("select count(*) from datap")
#   
# qpi = "SELECT a.*, CASE WHEN b.hydroid is not NULL THEN 1 ELSE 0 END as has_permit
#   from data as a left outer join datap as b
#   on a.hydroid = b.hydroid "
# 
# data_pi = sqldf(
#     qpi
#   )
# sqldf("select has_permit, ROUND(sum(mgd),2) AS mgd, count(*) from data_pi group by has_permit")
# sqldf(
#   "select Source_type, Use_Type, 
#   ROUND(sum(mgd),2) AS mgd, count(*) 
#   from data_pi 
#   group by Source_type, Use_Type"
# )
# 
# #the percent column needs to be calculated ...but that was saved for another day
# permit_srctype <- sqldf(
#   "select Source_type, has_permit, 
#   ROUND(sum(mgd),2) AS mgd, count(*) 
#   from data_pi 
#   group by Source_type, has_permit"
# )
# 
# 
# 
# permit_src_use <- sqldf(
#   "select Source_type as 'Source Type', Use_Type as 'Category', has_permit as 'Description', ROUND(sum(mgd),2) AS 'MGD',  ROUND(sum(mgy),2) as 'MGY' 
#   from data_pi 
#   group by Source_type, Use_Type, has_permit"
# )
# permit_src_use$Description[permit_src_use$Description == '0'] <- 'Unpermitted'
# permit_src_use$Description[permit_src_use$Description == '1'] <- 'Permitted'
# CapStr <- function(y) {
#   c <- strsplit(y, " ")[[1]]
#   paste(toupper(substring(c, 1,1)), substring(c, 2),
#         sep="", collapse=" ")
# }
# permit_src_use[,2] <-sapply(permit_src_use[,2], CapStr)
# 
# kable(permit_src_use, "latex", booktabs = T, align = 'c') %>%
#   kable_styling(latex_options = c("striped", "scale_down")) %>%
#   #column_spec(column = 2, bold = TRUE) 
#   column_spec(4, width = "5em") %>%
#   column_spec(5, width = "5em")
# 
# 
# 
# kable(permit_srctype, "latex", booktabs = T, align = 'c') %>%
#   kable_styling(latex_options = c("striped", "scale_down")) %>%
#   column_spec(4, width = "5em") %>%
#   column_spec(5, width = "5em")
# 
# 
# #sqldf("select a.Use_Type, a.Source_Type, a.mgd, b.mgd from y2017 as a left outer join y2018 as b on (a.Use_Type = b.Use_Type and a.Source_type = b.Source_Type)")
# #names(cat_table) <- c('cat', 'use_type', 'y1', 'y2', 'y3')
# 
# #year_frame <- arrange(year_table, Source_Type, Use_Type)
# # checked 2017 and 2015 SW ag permitted MPs - all farm ponds that just don't consistently report every year 
# # sqldf(
# #   "select HydroID, Source_Type, Use_Type, mgd, mgy
# #   from data_pi 
# #   WHERE Source_Type like 'Surface Water' 
# #   AND Use_Type = 'agriculture'
# #   AND has_permit = 1"
# # )
# 
# 
# #ardp <- 'U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2019 Report\\Water Use Exports\\All Withdrawals By Use Type\\Water Use Exports By Type and Permit_Use For AWRR\\'
# 
# # Merge these 3
# # read vwuds tab, add 'ptype' <= 'vwuds' 
# # read VWP tab, add 'ptype' <= 'vwp' 
# # Read GWP file, add 'ptype' <= 'gwp' 
# 
# 
# #source <- "ALL SW VWP AND VWUDS SEPARATED CLEANED.xlsx"
# #folder <- ardp
# 
# #sheet <- "VWUDS"
# #rawdata <- read_excel(paste(folder,source,sep=''),sheet)
# #data <- rawdata
# 
