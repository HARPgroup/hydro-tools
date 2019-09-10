#library("readxl")
library("dplyr")
library('httr')
options(timeout=24000)
a <- c('agricultural', 'industrial', 'municipal', 'mining', 'commercial', 'manufacturing', 'irrigation')
b <- c('Surface Water Intake', 'Well', 'Total (GW + SW)')
cat_table <- expand.grid(a,b)

for (y in 2015:2015) {
  print(y)
  startdate <- paste(y, "-01-01",sep='')
  enddate <- paste(y, "12-31", sep='')
  #data.vwuds <- read.csv(file=paste("https://deq1.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=77498",sep=""), header=TRUE, sep=",")
  
  #has 3 issuing authorities, does not include power
  data.all <- read.csv(file=paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), header=TRUE, sep=",")
  
  data <- data.all
  
  #remove duplicates (keeps one row)
  data <- distinct(data, HydroID, .keep_all = TRUE)
  #exclude dalecarlia
  data <- data[-which(data$Facility=='DALECARLIA WTP'),]
  #rename columns
  colnames(data) <- c('HydroID', 'Hydrocode', 'Source_Type',
                      'MP_Name', 'Facility', 'Use_Type', 'Year',
                      'mgy', 'mgd', 'lat', 'lon', 'locality')
  #make use type values lowecase
  data$Use_Type <- str_to_lower(data$Use_Type)
  #change 'Well' and 'Surface Water Intake' values in source_type column to match report headers
  
  # wells <- which(data$Source_Type == 'Well',arr.ind = TRUE)
  # data[data$Source_Type == 'Well','Source_Type'] <- 'Groundwater'
  # data$Source_Type[data$Source_Type == 'Well'] <- 'Groundwater'
  # 
  catsum = data
  catsum$Source_Type <- "Total (GW + SW)"
  catsum <- catsum %>% group_by(Use_Type, Source_Type)
  
  
  catsum <- catsum %>% summarise(
    mgd = sum(mgd),
    mgy = sum(mgy)
  )
  
  catsourcesum <- data %>% group_by(Use_Type, Source_Type)
  
  catsourcesum <- catsourcesum %>% summarise(
    mgd = sum(mgd),
    mgy = sum(mgy)
  )
  year_table = rbind(catsourcesum, catsum)
  if(cat_table == FALSE){
    cat_table = year_table[,-4]
  }else{
    yr = as.character(y)
    cat_table <- cbind(cat_table, newcol = year_table$mgd)
  }
  

}

names(cat_table) <- c('cat', 'use_type', 'y1', 'y2', 'y3')

year_frame <- arrange(year_table, Source_Type, Use_Type)




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

