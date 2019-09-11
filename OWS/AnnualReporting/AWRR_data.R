#library("readxl")
library("dplyr")
library('httr')
library('stringr')
library("kableExtra")

a <- c(
  'agricultural', 
  'commercial', 
  'irrigation',
  'manufacturing',  
  'mining', 
  'municipal'
)
b <- c('Groundwater', 'Surface Water', 'Total (GW + SW)')
cat_table<- data.frame(expand.grid(a,b))

colnames(cat_table) <- c('Use_Type', 'Source_Type')
cat_table <- arrange(cat_table, Source_Type, Use_Type )
#cat_table = FALSE
year.range <- 2014:2018

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
  
  
  catsourcesum <- data %>% group_by(Use_Type, Source_Type)
  
  catsourcesum <- catsourcesum %>% summarise(
    mgd = sum(mgd),
    mgy = sum(mgy)
  )
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
  if (is.logical(cat_table)) {
    cat_table = year_table[,1:3]
  } else {
    cat_table <- cbind(cat_table, year_table[,3])
  }
  

}

#cat_table_raw <- cat_table <- cat_table_raw

cat_table <- data.frame(cat_table[2],cat_table[1],cat_table[3:(length(year.range)+2)])
names(cat_table) <- c('Source Type', 'Category', year.range)

multi_yr_avg <- signif((rowMeans(cat_table[3:(length(year.range)+2)], na.rm = FALSE, dims = 1)),2)
#names(multi_yr_avg) <- paste(length(year.range)," Year Avg.",sep="")
cat_table <- cbind(cat_table,multi_yr_avg)
pct_chg <- signif(((cat_table["2018"]-cat_table["multi_yr_avg"])/cat_table["multi_yr_avg"])*100, 1)
names(pct_chg) <- '% Change 2018 to Avg.'
cat_table <- cbind(cat_table,'pct_chg' = pct_chg)

##############################################################
# ADD BOTTOM ROW OF TOTALS TO TABLE

multi_yr_avg.sums <- mean(c(sum(cat_table.total[3]),
                            sum(cat_table.total[4]),
                            sum(cat_table.total[5]),
                            sum(cat_table.total[6]),
                            sum(cat_table.total[7])))

total_pct_chg <- signif(((sum(cat_table.total[7])-multi_yr_avg.sums)/multi_yr_avg.sums)*100, 1)


cat_table.total <- cat_table[c(13:18),]
catsum.sums <- data.frame(Source_Type="",
                          Category="Total (GW + SW)",
                          mgd=sum(cat_table.total[3]),
                          mgd=sum(cat_table.total[4]),
                          mgd=sum(cat_table.total[5]),
                          mgd=sum(cat_table.total[6]),
                          mgd=sum(cat_table.total[7]),
                          mgd=multi_yr_avg.sums,
                          mgd=total_pct_chg 
)


colnames(catsum.sums) <- c('Source Type', 'Category','2014','2015','2016','2017','2018','multi_yr_avg','% Change 2018 to Avg.')
cat_table <- rbind(cat_table,catsum.sums)
##############################################################

print(cat_table)


kable(cat_table, "latex", booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>% 
  column_spec(7, width = "5em") %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em") %>%
  column_spec(10, width = "4em")


#names(multi_yr_avg) <- paste(length(year.range)," Year Avg.",sep="")

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

