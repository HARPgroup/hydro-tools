#library("readxl")
library("dplyr")
library('httr')
library('stringr')


a <- c('agricultural', 'municipal', 'mining', 'commercial', 'manufacturing', 'irrigation')
b <- c('Surface Water', 'Groundwater', 'Total (GW + SW)')
cat_table<- data.frame(expand.grid(a,b))

colnames(cat_table) <- c('Use_Type', 'Source_Type')

year.range <- 2014:2018

for (y in year.range) {
  #y<-2014
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
  

  data$Use_Type[data$Use_Type == 'industrial'] <- 'manufacturing'
  
  
  catsum <- data
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
  year_table <- rbind(catsourcesum, catsum)
  #year_table <- arrange(year_table, Source_Type, Use_Type)
  #cat_table <- arrange(cat_table, Source_Type, Use_Type)
 
    cat_table <- cbind(cat_table, year_table[,3])


}

names(cat_table) <- c('Category', 'Source Type', year.range)


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

