#library("readxl")
library("dplyr")
library('httr')
library("sqldf")
options(timeout=24000)
a <- c('agricultural', 'municipal', 'mining', 'commercial', 'manufacturing', 'irrigation')
b <- c('Surface Water Intake', 'Well', 'Total (GW + SW)')
cat_table <- expand.grid(a,b)
syear = 2018
eyear = 2018
s_cols = FALSE


for (y in year.range) {
  
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
  data <- distinct(data, HydroID, .keep_all = TRUE)
  #exclude dalecarlia
  data <- data[-which(data$Facility=='DALECARLIA WTP'),]
  
  if (length(which(data$Use.Type=='facility')) > 0) {
    data <- data[-which(data$Use.Type=='facility'),]
  }
  assign(paste("yd", y, sep=''), data)
  
  #rename columns
  colnames(data) <- c('HydroID', 'Hydrocode', 'Source_Type',
                      'MP_Name', 'Facility', 'Use_Type', 'Year',
                      'mgy', 'mgd', 'lat', 'lon', 'locality')
  #make use type values lowecase
  data$Use_Type <- str_to_lower(data$Use_Type)
  #change 'Well' and 'Surface Water Intake' values in source_type column to match report headers
  levels(data$Source_Type) <- c(levels(data$Source_Type), "Groundwater", "Surface Water")
  data$Source_Type[data$Source_Type == 'Well'] <- 'Groundwater'
  data$Source_Type[data$Source_Type == 'Surface Water Intake'] <- 'Surface Water'
  data$mgd = data$mgy / 365.0
  
  data$Use_Type[data$Use_Type == 'industrial'] <- 'manufacturing'
  
  
  catsourcesum <- data %>% group_by(Use_Type, Source_Type)
  
  catsourcesum <- catsourcesum %>% summarise(
    mgd = sum(mgd),
    mgy = sum(mgy)
  )
  catsourcesum$mgd = round(catsourcesum$mgy / 365.0,2)
  catsourcesum <- arrange(catsourcesum, Source_Type, Use_Type)
  
  
  catsum <- catsourcesum
  catsum$Source_Type <- "Total (GW + SW)"
  catsum <- catsum %>% group_by(Use_Type, Source_Type)
  
  catsum <- catsum %>% summarise(
    mgd = sum(mgd),
    mgy = sum(mgy)
  )
  catsum <- arrange(catsum, Source_Type, Use_Type)
  
  
  year_table <- rbind(catsourcesum, catsum)
  year_table <- arrange(year_table, Source_Type, Use_Type)
  assign(paste("y", y, sep=''), year_table)

  tname = paste("tbl", y, sep='')
  rawname = paste("raw", y, sep='')
  assign(tname, year_table)
  assign(rawname, data)
  if (is.logical(s_cols)) {
    s_cols = paste(
      "select ", tname, ".Use_Type, ", 
      tname, ".Source_Type, ", 
      tname, ".mgd as wd", y,
      sep=''
    )
    t_names = paste("FROM ", tname)
    btname = tname
  } else {
    s_cols <- paste(s_cols, ", ", tname, ".mgd as wd", y, sep='')
    t_names <- paste(
      t_names, 
      " left outer join ", 
      tname, " on (", btname, ".Use_Type", " = ",
      tname, ".Use_Type",
      " AND ", btname, ".Source_Type", " = ",
      tname, ".Source_Type",
      " )",
      sep = ''
    )
  }

}

q = paste(s_cols, t_names)
print(q)
sqldf(q)

sqldf("select count(*) from raw2018 ")
sqldf("select Source_Type, sum(mgd) from raw2018 group by Source_Type")
sqldf("select Source_Type, Use_Type, sum(mgd), count(*) from raw2018 group by Source_Type, Use_Type")
ttab <- sqldf("select * from raw2018 limit 100")
q = "SELECT a.hydroid, CASE WHEN b.hydroid is not NULL THEN 'permit' END as permitted
  from raw2018 as a left outer join ttab as b
  on a.hydroid = b.hydroid 
  where b.hydroid is not null"
sqldf(
  q
)

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

