#library("readxl")
library("dplyr")
library('httr')
library("sqldf")

y <- 2019
eyear <- y
  print(y)
  startdate <- paste(y, "-01-01",sep='')
  enddate <- paste(y, "-12-31", sep='')
  
  localpath <- tempdir()
  filename <- paste("data.all_",y,".csv",sep="")
  destfile <- paste(localpath,filename,sep="\\")  
  download.file(paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), destfile = destfile, method = "libcurl")  
  data.all <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
  
  #has 3 issuing authorities, does not include power
  #  data.all <- read.csv(file=paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), header=TRUE, sep=",")
  
  data <- data.all
  
  #remove duplicates (keeps one row)
  data <- distinct(data, MP_hydroid, Year, .keep_all = TRUE)
  #exclude dalecarlia
  data <- data[-which(data$Facility=='DALECARLIA WTP'),]
  
  if (length(which(data$Use.Type=='facility')) > 0) {
    data <- data[-which(data$Use.Type=='facility'),]
  }
  #rename columns
  # colnames(data) <- c('HydroID', 'Hydrocode', 'Source_Type',
  #                     'MP_Name', 'Facility', 'Use_Type', 'Year',
  #                     'mgy', 'mgd', 'lat', 'lon', 'locality')
  
  colnames(data) <- c('HydroID',
                      'Hydrocode',
                      'Source_Type',
                      'MP_Name',
                      'Facility_HydroID', 
                      'Facility',
                      'Use_Type', 
                      'Year',
                      'mgy',
                      'mgd',
                      'lat',
                      'lon',
                      'FIPS',
                      'locality')
  
  data$mgd <- data$mgy/365
  sum(data$mgy)
  #make use type values lowercase
  data$Use_Type <- str_to_lower(data$Use_Type)
  #change 'Well' and 'Surface Water Intake' values in source_type column to match report headers
  levels(data$Source_Type) <- c(levels(data$Source_Type), "Groundwater", "Surface Water")
  data$Source_Type[data$Source_Type == 'Well'] <- 'Groundwater'
  data$Source_Type[data$Source_Type == 'Surface Water Intake'] <- 'Surface Water'
  
  
  data$Use_Type[data$Use_Type == 'industrial'] <- 'manufacturing'
  
  download.file(paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200",sep=""), destfile = destfile, method = "libcurl")  
  datap <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
  
  sqldf("select count(*) from datap")
  
qpi = "SELECT a.*, CASE WHEN b.MP_hydroid is not NULL THEN 1 ELSE 0 END as has_permit
  from data as a left outer join datap as b
  on a.HydroID = b.MP_hydroid "

data_pi = sqldf(
    qpi
  )

sqldf("select has_permit, ROUND(sum(mgd),2) AS mgd, count(*) from data_pi group by has_permit")
sqldf(
  "select Source_type, Use_Type, 
  ROUND(sum(mgd),2) AS mgd, count(*) 
  from data_pi 
  group by Source_type, Use_Type"
)

#the percent column needs to be calculated ...but that was saved for another day
permit_srctype <- sqldf(
  "select Source_type, has_permit, 
  ROUND(sum(mgd),2) AS mgd, count(*) 
  from data_pi 
  group by Source_type, has_permit"
)

permit_src_use <- sqldf(
  "select Source_type, Use_Type, has_permit, ROUND(sum(mgd),2) AS mgd, count(*) 
  from data_pi 
  group by Source_type, Use_Type, has_permit"
)

#FORMAT Table 2: 2018 Permitted and Unpermitted (Excluded) Withdrawals (MGD)
table2_bysrc <- sqldf('SELECT Source_Type AS "Source Type", CASE 
                    WHEN has_permit = 1 
                    THEN "Permitted"
                    WHEN has_permit = 0 
                    THEN "Unpermitted"
                    END AS "Withdrawal Type",
                    mgd AS "Withdrawal Amount"
                    FROM permit_srctype
                    ORDER BY Source_Type, has_permit desc
                    ')
gw_perm_pct <- round((table2_bysrc[1,3] / sqldf('SELECT sum("Withdrawal Amount")
                               FROM table2_bysrc
                               GROUP BY "Source Type"')[1,]) * 100,2)
gw_unperm_pct <- round((table2_bysrc[2,3] / sqldf('SELECT sum("Withdrawal Amount")
                               FROM table2_bysrc
                               GROUP BY "Source Type"')[1,]) * 100,2)
sw_perm_pct <- round((table2_bysrc[3,3] / sqldf('SELECT sum("Withdrawal Amount")
                               FROM table2_bysrc
                               GROUP BY "Source Type"')[2,]) * 100,2)
sw_unperm_pct <- round((table2_bysrc[4,3] / sqldf('SELECT sum("Withdrawal Amount")
                               FROM table2_bysrc
                               GROUP BY "Source Type"')[2,]) * 100,2)

table2_total <- sqldf('SELECT "Total" AS "Source Type", CASE 
                    WHEN has_permit = 1 
                    THEN "Permitted"
                    WHEN has_permit = 0 
                    THEN "Unpermitted"
                    END AS "Withdrawal Type",
                    sum(mgd) AS "Withdrawal Amount"
                      FROM permit_srctype
                      GROUP BY has_permit
                      ORDER BY Source_Type, has_permit desc')
tot_perm_pct <- round((table2_total[1,3] / sqldf('SELECT sum("Withdrawal Amount")
                               FROM table2_total')) * 100,2)
tot_unperm_pct <-  round((table2_total[2,3] / sqldf('SELECT sum("Withdrawal Amount")
                               FROM table2_total')) * 100,2)

pct <- rbind(gw_perm_pct,gw_unperm_pct,sw_perm_pct,sw_unperm_pct,tot_perm_pct,tot_unperm_pct)
table2 <- cbind(rbind(table2_bysrc,table2_total),pct)


kable(table2[2:4],'latex', booktabs = T, align =  c('l','c','c'),
      caption = paste(eyear, "Permitted and Unpermitted (Excluded) Withdrawals (MGD)",sep=" "),
      label = paste(eyear, "Permitted and Unpermitted (Excluded) Withdrawals (MGD)",sep=" "),
      col.names = c( 'Withdrawal Type',
                     paste(eyear,"Withdrawal Amount",sep = ' '),
        '% of Total By Source Type')) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  pack_rows("Groundwater", 1, 2, hline_before = T, hline_after = F) %>%
  pack_rows("Surface Water", 3, 4, hline_before = T, hline_after = F) %>%
  pack_rows("Total (GW + SW)", 5, 6, hline_before = T, hline_after = F)


#FORMAT Table 3: 2018 Permitted and Unpermitted (Excluded) By Use Type Withdrawals (MGD)
kable(permit_src_use, "latex", booktabs = T, align = 'c') %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  #column_spec(column = 2, bold = TRUE) 
  column_spec(4, width = "5em") %>%
  column_spec(5, width = "5em")













#sqldf("select a.Use_Type, a.Source_Type, a.mgd, b.mgd from y2017 as a left outer join y2018 as b on (a.Use_Type = b.Use_Type and a.Source_type = b.Source_Type)")
#names(cat_table) <- c('cat', 'use_type', 'y1', 'y2', 'y3')

#year_frame <- arrange(year_table, Source_Type, Use_Type)




#ardp <- 'U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2019 Report\\Water Use Exports\\All Withdrawals By Use Type\\Water Use Exports By Type and Permit_Use For AWRR\\'

# Merge these 3
# read vwuds tab, add 'ptype' <= 'vwuds' 
# read VWP tab, add 'ptype' <= 'vwp' 
# Read GWP file, add 'ptype' <= 'gwp' 


#source <- "ALL SW VWP AND VWUDS SEPARATED CLEANED.xlsx"
#folder <- ardp

#sheet <- "VWUDS"
#rawdata <- read_excel(paste(folder,source,sep=''),sheet)
#data <- rawdata

