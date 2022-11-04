
library(sqldf) #used for subsetting and filtering 

wds <- read.csv("http://deq2.bse.vt.edu/files/wsp/metrics_facility_wd_mgd.csv", header=TRUE, sep=",")

# Query using sqldf
sqldf("select * from wds WHERE runid_11 > 100.0")
sqldf("select * from wds WHERE propname LIKE '%north anna%' ")

sqldf("
      select * from wds 
      WHERE runid_11 > 1.0 
      and runid_11 < 50.0 
      and propname LIKE '%James%' 
    ")

# populate a new data frame using sqldf
big_wds <- sqldf("select * from wds WHERE runid_11 > 100.0")
north_anna_wds <- sqldf("select * from wds WHERE propname LIKE '%north anna%' ")

