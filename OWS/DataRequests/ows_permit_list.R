# THIS SCRIPT ADDS A COLUMN OF FACILITY 5-yr AVG TO THE EXPORT OF VIEW ows_permit_list
# Source files:
ows_permit_list.path = "http://deq1.bse.vt.edu:81/d.dh/ows-list-permits-export"
mp_all_wide.path = paste0(foundation_location,"/OWS/foundation_datasets/awrr/2022/mp_all_wide_2017-2021.csv")

source(paste0(basepath,'/config.local.private'))
#Output file path: 
export.path = "C:/Users/nrf46657/Desktop/GitHub/hydro-tools/OWS/DataRequests/ows_permit_list_5yr_avg.csv"

#-----------------------------------------------------------------------------------
# READ FILE FROM VAHYDRO CONTAINING PERMIT DATA (INCLUDES PERMIT LIMITS ETC.)
localpath <- tempdir()
filename <- paste("ows-list-permits-export.csv",sep="")
destfile <- paste(localpath,filename,sep="\\")  
download.file(paste(ows_permit_list.path,sep=""), destfile = destfile, method = "libcurl")  
ows_permit_list <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")

#-----------------------------------------------------------------------------------
# READ FILE FROM FOUNDATION DATASET CONTAINING 5-YR AVG DATA
mp_all_wide <- read.csv(mp_all_wide.path)  
# colnames(mp_all_wide)

# SUM 5YR AVG TO FACILITY
fac_sum_query <- paste("SELECT a.Facility_hydroid, a.Locality, sum(a.fiveyr_avg_mgy) as fac_fiveyr_avg_mgy
                    FROM
                    mp_all_wide as a
                    GROUP BY a.Facility_hydroid
                   ",sep="")
fac_sum <- sqldf(fac_sum_query)

#-----------------------------------------------------------------------------------
# JOIN 5YR AVG TO COLUMN TO ows_permit_list
join_query <- paste("SELECT a.*, b.*
                    FROM
                    ows_permit_list as a
                    LEFT OUTER JOIN fac_sum as b
                    on a.'VA.Hydro.Facility.ID' = b.Facility_hydroid
                   ",sep="")
data_join <- sqldf(join_query)

write.csv(data_join,export.path, row.names = FALSE)

#-----------------------------------------------------------------------------------
