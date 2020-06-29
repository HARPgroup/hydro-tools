library("dplyr")
library('httr')
library('stringr')
library("kableExtra")
library('tidyr')

syear = 2015
eyear = 2019

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
cat_table <- arrange(cat_table, Source_Type, Use_Type)

#cat_table = FALSE
year.range <- syear:eyear

multi_yr_data <- list()

for (y in year.range) {
  
  print(y)
  startdate <- paste(y, "-01-01",sep='')
  enddate <- paste(y, "-12-31", sep='')
  
  localpath <- tempdir()
  filename <- paste("data.all_",y,".csv",sep="")
  destfile <- paste(localpath,filename,sep="\\")  
  download.file(paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), destfile = destfile, method = "libcurl")  
  data.year <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
  
  #has 3 issuing authorities, does not include power
#  data.all <- read.csv(file=paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), header=TRUE, sep=",")
  
  data <- data.year
  
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
  
  #combine each year of data into a single table
  multi_yr_data <- rbind(multi_yr_data, data)
  
  #begin summary table 1 manipulation
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
  if (is.logical(cat_table)) {
    cat_table = year_table[,1:3]
  } else {
    cat_table <- cbind(cat_table, year_table[,3])
  }
  

}

#cat_table_raw <- cat_table <- cat_table_raw

cat_table <- data.frame(cat_table[2],cat_table[1],cat_table[3:(length(year.range)+2)])
names(cat_table) <- c('Source Type', 'Category', year.range)

multi_yr_avg <- round((rowMeans(cat_table[3:(length(year.range)+2)], na.rm = FALSE, dims = 1)),2)
#names(multi_yr_avg) <- paste(length(year.range)," Year Avg.",sep="")
cat_table <- cbind(cat_table,multi_yr_avg)



##Groundwater Total##
gw_table <- cat_table[cat_table$"Source Type" == 'Groundwater',]
gw_sums <- data.frame(Source_Type="",
                      Category="Total Groundwater",
                      mgd=sum(gw_table[3]),
                      mgd=sum(gw_table[4]),
                      mgd=sum(gw_table[5]),
                      mgd=sum(gw_table[6]),
                      mgd=sum(gw_table[7]),
                      mgd=sum(gw_table[8])
)
colnames(gw_sums) <- c('Source Type', 'Category',year.range,'multi_yr_avg')
##Surface Water Total##
sw_table <- cat_table[cat_table$"Source Type" == 'Surface Water',]
sw_sums <- data.frame(Source_Type="",
                      Category="Total Surface Water",
                      mgd=sum(sw_table[3]),
                      mgd=sum(sw_table[4]),
                      mgd=sum(sw_table[5]),
                      mgd=sum(sw_table[6]),
                      mgd=sum(sw_table[7]),
                      mgd=sum(sw_table[8])
)
colnames(sw_sums) <- c('Source Type', 'Category',year.range,'multi_yr_avg')
cat_table <- rbind(cat_table,gw_sums, sw_sums)


pct_chg <- round(((cat_table[paste(eyear)]-cat_table["multi_yr_avg"])/cat_table["multi_yr_avg"])*100, 1)
names(pct_chg) <- paste('% Change',eyear,'to Avg.')
cat_table <- cbind(cat_table,'pct_chg' = pct_chg)

##############################################################
# ADD BOTTOM ROW OF TOTALS TO TABLE
cat_table.total <- cat_table[c(13:18),]
multi_yr_avg.sums <- mean(c(sum(cat_table.total[3]),
                            sum(cat_table.total[4]),
                            sum(cat_table.total[5]),
                            sum(cat_table.total[6]),
                            sum(cat_table.total[7])))

total_pct_chg <- round(((sum(cat_table.total[7])-multi_yr_avg.sums)/multi_yr_avg.sums)*100, 1)


#cat_table.total <- cat_table[c(13:18),]
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


colnames(catsum.sums) <- c('Source Type', 'Category',year.range,'multi_yr_avg', paste('% Change',eyear,'to Avg.'))
cat_table <- rbind(cat_table,catsum.sums)

#make Category values capital
cat_table$Category <- str_to_title(cat_table$Category)
print(cat_table)

################### MAY QA CHECK ##########################################
kable(cat_table, booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em") %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2020 Report\\May_QA\\summary_table_vahydro_",eyear+1,"_",Sys.Date(),".html",sep = ''))

################### TABLE 1 : Summary ##########################################
table1_latex <- kable(cat_table[2:9],'latex', booktabs = T,
      caption = paste("Summary of Virginia Water Withdrawals by Use Category and Source Type",syear,"-",eyear,"(MGD)",sep=" "),
      label = paste("Summary of Virginia Water Withdrawals by Use Category and Source Type",syear,"-",eyear,"(MGD)",sep=" "),
      col.names = c(
                    'Category',
                    year.range,
                    paste((eyear-syear)+1,"Year Avg."),
                    paste('% Change', eyear,'to Avg.', sep = ' '))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(1, width = "10em") %>%
  pack_rows("Surface Water", 1, 6, hline_before = T, hline_after = F) %>%
  pack_rows("Groundwater", 7, 12, hline_before = T, hline_after = F) %>%
  pack_rows("Total (GW + SW)", 13, 18, hline_before = T, hline_after = F) %>%
  pack_rows("Total", 19, 20, hline_before = T, hline_after = F) %>%
  row_spec(21, bold=T, extra_css = "border-top: 1px solid") 

#CUSTOM LATEX CHANGES
#insert hold position header
table1_tex <- gsub(pattern = "{table}[t]", 
                   repl    = "{table}[ht!]", 
                   x       = table1_latex, fixed = T )
table1_tex

table1_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2020 Report\\overleaf\\summary_table1_",eyear+1,".tex",sep = ''))

################### TABLE 4 : TOP 20 USERS ##########################################
#make Category values capital
multi_yr_data$Use_Type <- str_to_title(multi_yr_data$Use_Type)
multi_yr_data$Facility <- str_to_title(multi_yr_data$Facility)
#transform from long to wide table
data_all <- pivot_wider(data = multi_yr_data,id_cols = c(HydroID,Source_Type, MP_Name, Facility_HydroID, Facility, Use_Type, lat, lon, FIPS), names_from = Year, values_from = mgy)

#avg mgd, order by
data_avg <- sqldf('SELECT HydroID, avg(mgy) as multi_yr_avg
                  FROM multi_yr_data
                  GROUP BY HydroID')
data_all <- sqldf('SELECT a.*,  b.multi_yr_avg, 
                        CASE WHEN Source_Type = "Groundwater"
                        THEN 1
                        END AS GW_type,
                        CASE
                        WHEN Source_Type = "Surface Water"
                        THEN 1
                        END AS SW_Type
                  FROM data_all AS a
                  LEFT OUTER JOIN data_avg AS b
                  ON a.HydroID = b.HydroID')
#group by facility
data_all_fac <- sqldf(paste('SELECT Facility_HydroID, Facility, Source_Type, Use_Type, lat, lon, FIPS, round((sum(',paste('"',eyear,'"', sep = ''),')/365),1) AS mgd, round((sum(multi_yr_avg)/365),1) as multi_yr_avg, sum(GW_type) AS GW_type, sum(SW_type) AS SW_type
                      FROM data_all
                      GROUP BY Facility_HydroID',sep = ''))
#limit 20
top_20 <- sqldf('SELECT Facility_HydroID, Facility, 
                        FIPS AS "City/County", 
                        CASE WHEN GW_Type > 0 AND SW_type IS NULL
                        THEN "GW"
                        WHEN SW_Type > 0 AND GW_type IS NULL
                        THEN "SW"
                        WHEN GW_Type > 0 AND SW_Type > 0
                        THEN "SW/GW"
                        END AS Type,
                        "" AS "Major Source",
                        multi_yr_avg,
                        mgd,
                        Use_Type AS Category
                FROM data_all_fac
                ORDER BY multi_yr_avg DESC
                LIMIT 20')

#KABLE
table4_latex <- kable(top_20,'latex', booktabs = T, align = c('l','l','c','l','c','c','l') ,
                      caption = paste("Top 20 Reported Water Withdrawals in",eyear,"Excluding Power Generation (MGD)",sep=" "),
                      label = paste("Top 20 Reported Water Withdrawals in",eyear,"Excluding Power Generation (MGD)",sep=" "),
                      col.names = c(
                        'Facility',
                        'City/County',
                        'Type',
                        'Major Source',
                        paste((eyear-syear)+1,"Year Avg."),
                        paste(eyear, 'Withdrawal', sep = ' '),
                        'Category')) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(1, width = "12em")

#CUSTOM LATEX CHANGES
#insert hold position header
table4_tex <- gsub(pattern = "{table}[t]", 
                   repl    = "{table}[ht!]", 
                   x       = table4_latex, fixed = T )
table4_tex

table4_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October 2020 Report\\overleaf\\summary_table4_",eyear+1,".tex",sep = ''))