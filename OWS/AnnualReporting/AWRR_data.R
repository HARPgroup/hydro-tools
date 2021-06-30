#library("readxl")
library("dplyr")
library("tidyr")
library('httr')
library('stringr')
library("kableExtra")
library('stringr')
library('ggplot2')
library('sqldf')
options(scipen = 999)

#file_extension <- ".html"
file_extension <- ".tex"

#switch between file types to save in common drive folder; html or latex
if (file_extension == ".html") {
  options(knitr.table.format = "html") #"html" for viewing in Rstudio Viewer pane
  file_ext <- ".html" #view in R or browser
} else {
  options(knitr.table.format = "latex") #"latex" when ready to output to Overleaf
  file_ext <- ".tex" #for easy upload to Overleaf
}
#Kable Styling
latexoptions <- c("scale_down")


syear = 2016
eyear = 2020
year.range <- syear:eyear

############### PULL DIRECTLY FROM VAHYDRO ###################################################
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

multi_yr_data <- list()

for (y in year.range) {
  
  print(y)
  startdate <- paste(y, "-01-01",sep='')
  enddate <- paste(y, "-12-31", sep='')
  
  localpath <- tempdir()
  filename <- paste("data.all_",y,".csv",sep="")
  destfile <- paste(localpath,filename,sep="\\")  
  download.file(paste("https://deq1.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), destfile = destfile, method = "libcurl")  
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

##### ADD BOTTOM ROW OF TOTALS TO TABLE################
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

#save the cat_table to use for data reference - we can refer to that csv when asked questions about the data
write.csv(cat_table, paste("U:\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\Table1_",syear,"-",eyear,".csv",sep = ""), row.names = F)

#save the multi_yr_data to use for data reference - we can refer to that csv when asked questions about the data
write.csv(multi_yr_data, paste("U:\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_all_",syear,"-",eyear,".csv",sep = ""), row.names = F)


######################### IS THERE A STATIC TABLE? READ THAT IN AND BEGIN FROM HERE ###########
cat_table <- read.csv(file = paste("U:\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\Table1_",syear,"-",eyear,".csv",sep = ""))
colnames(cat_table) <- c('Source Type', 'Category',year.range,'multi_yr_avg', paste('% Change',eyear,'to Avg.'))

multi_yr_data <- read.csv(file = paste("U:\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_all_",syear,"-",eyear,".csv",sep = ""))

################### MAY QA CHECK ##########################################
kable(cat_table, booktabs = T) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(8, width = "5em") %>%
  column_spec(9, width = "5em") %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\May_QA\\summary_table_vahydro_",eyear+1,"_",Sys.Date(),".html",sep = ""))

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
  pack_rows("Groundwater", 1, 6, hline_before = T, hline_after = F) %>%
  pack_rows("Surface Water", 7, 12, hline_before = T, hline_after = F) %>%
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
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\overleaf\\summary_table1_",eyear+1,".tex",sep = ''))

################### TABLE 4 : TOP 20 USERS ##########################################
#READ IN FIPS TABLE
fips <- read.csv(file = "C:\\Users\\maf95834\\Documents\\Github\\vahydro\\R\\wsp\\wsp2020\\FoundationDataset\\fips_codes.csv")
#make Category values capital
multi_yr_data$Use_Type <- str_to_title(multi_yr_data$Use_Type)
multi_yr_data$Facility <- str_to_title(multi_yr_data$Facility)
#transform from long to wide table
data_all <- pivot_wider(data = multi_yr_data, id_cols = c(HydroID, Source_Type, MP_Name, Facility_HydroID, Facility,Use_Type, FIPS), names_from = Year, values_from = mgy)

data_all <- sqldf('SELECT a.*, b.name AS Locality
                  FROM data_all a
                  LEFT OUTER JOIN fips b
                  ON a.FIPS = b.code')

#avg mgy, order by
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


#write.csv(data_all, paste("U:\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_all_wide_",syear,"-",eyear,".csv",sep = ""), row.names = F)


#group by facility
data_all_fac <- sqldf(paste('SELECT Facility_HydroID, Facility, Source_Type, Use_Type, Locality, round((sum(',paste('"',eyear,'"', sep = ''),')/365),1) AS mgd, round((sum(multi_yr_avg)/365),1) as multi_yr_avg, sum(GW_type) AS GW_type, sum(SW_type) AS SW_type
                      FROM data_all
                      GROUP BY Facility_HydroID',sep = ''))
#limit 20
top_20 <- sqldf('SELECT Facility_HydroID, Facility, 
                        Locality, 
                        CASE WHEN GW_Type > 0 AND SW_Type IS NULL
                        THEN "GW"
                        WHEN SW_Type > 0 AND GW_Type IS NULL
                        THEN "SW"
                        WHEN GW_Type > 0 AND SW_Type > 0
                        THEN "SW/GW"
                        END AS Type,
                        "" AS "Major Source",
                        multi_yr_avg,
                        mgd,
                        Use_Type AS Category
                FROM data_all_fac
                ORDER BY mgd DESC
                LIMIT 20')

#KABLE
table4_latex <- kable(top_20[2:8],'latex', booktabs = T, align = c('l','l','c','l','c','c','l') ,
                      caption = paste("Top 20 Reported Water Withdrawals in",eyear,"Excluding Power Generation (MGD)",sep=" "),
                      label = paste("Top 20 Reported Water Withdrawals in",eyear,"Excluding Power Generation (MGD)",sep=" "),
                      col.names = c(
                        'Facility',
                        'Locality',
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
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\summary_table4_",eyear,".tex",sep = ''))
################### TOP USERS BY USE TYPE (TABLES 6, 8, 10, 12, 14, 15,  17, 20, ############################

#Table: Highest Reported  Withdrawals in eyear (MGD)
#use_types <- unique(x = data_all$Use_Type)
use_types <- list("Municipal", "Agriculture", "Commercial", "Irrigation", "Mining", "Manufacturing")

for (u in use_types) {
  print(paste('PROCESSING TOP 5 TABLE: ',u),sep = '')
  
  if (u == "Manufacturing") {
    for (s in list("Groundwater","Surface Water")) {
      
      #group by source type from data_all
      #group by facility
      data_all_source <- sqldf(paste('SELECT Facility_HydroID, Facility, Source_Type, Use_Type, Locality, round((sum(',paste('"',eyear,'"', sep = ''),')/365),1) AS mgd, round((sum(multi_yr_avg)/365),1) as multi_yr_avg, sum(GW_type) AS GW_type, sum(SW_type) AS SW_type
                      FROM data_all
                      GROUP BY Facility_HydroID, Source_Type',sep = ''))
      #top5
      top5 <- sqldf(paste('SELECT Facility_HydroID, Facility, 
                        Locality, 
                        CASE 
                        WHEN GW_Type > 0 AND SW_Type IS NULL
                        THEN "GW"
                        WHEN SW_Type > 0 AND GW_Type IS NULL
                        THEN "SW"
                        WHEN GW_Type > 0 AND SW_Type > 0
                        THEN "SW/GW"
                        END AS Type,
                        "" AS "Major Source",
                        multi_yr_avg,
                        mgd,
                        Use_Type AS Category
                FROM data_all_source
                WHERE Source_Type LIKE ',paste('"',s,'"', sep = ''),' 
                AND Use_Type LIKE',paste('"',u,'"', sep = ''),'
                ORDER BY mgd DESC
                LIMIT 5',sep = ''))
      
      #KABLE
      top5_latex <- kable(top5[2:7],'latex', booktabs = T, align = c('l','l','c','l','c','c') ,
                          caption = paste("Highest Reported Manufacturing and Industrial",s,"Withdrawals in",eyear,"(MGD)",sep=" "),
                          label = paste("Highest Reported Manufacturing and Industrial",s,"Withdrawals in",eyear,"(MGD)",sep=" "),
                          col.names = c(
                            'Facility',
                            'Locality',
                            'Type',
                            'Major Source',
                            paste((eyear-syear)+1,"Year Avg."),
                            paste(eyear, 'Withdrawal', sep = ' '))) %>%
        kable_styling(latex_options = c("striped", "scale_down")) %>%
        column_spec(1, width = "12em")
      
      #CUSTOM LATEX CHANGES
      #insert hold position header
      top5_tex <- gsub(pattern = "{table}[t]", 
                       repl    = "{table}[ht!]", 
                       x       = top5_latex, fixed = T )
      top5_tex
      
      top5_tex %>%
        cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\Manufacturing_",s,"_top5_",eyear,".tex",sep = ''))
    }
    
  }
  else if (u == "Municipal") {
    
    top5 <- sqldf(paste('SELECT Facility_HydroID, Facility, 
                        Locality, 
                        CASE 
                        WHEN GW_Type > 0 AND SW_Type IS NULL
                        THEN "GW"
                        WHEN SW_Type > 0 AND GW_Type IS NULL
                        THEN "SW"
                        WHEN GW_Type > 0 AND SW_Type > 0
                        THEN "SW/GW"
                        END AS Type,
                        "" AS "Major Source",
                        multi_yr_avg,
                        mgd,
                        Use_Type AS Category
                FROM data_all_fac
                WHERE Use_Type LIKE',paste('"',u,'"', sep = ''),'
                ORDER BY mgd DESC
                LIMIT 10',sep = ''))
    
    #KABLE
    top5_latex <- kable(top5[2:7],'latex', booktabs = T, align = c('l','l','c','l','c','c') ,
                        caption = paste("Highest Reported Public Water Supply Withdrawals in",eyear,"(MGD)",sep=" "),
                        label = paste("Highest Reported Public Water Supply Withdrawals in",eyear,"(MGD)",sep=" "),
                        col.names = c(
                          'Facility',
                          'Locality',
                          'Type',
                          'Major Source',
                          paste((eyear-syear)+1,"Year Avg."),
                          paste(eyear, 'Withdrawal', sep = ' '))) %>%
      kable_styling(latex_options = c("striped", "scale_down")) %>%
      column_spec(1, width = "12em")
    
    #CUSTOM LATEX CHANGES
    #insert hold position header
    top5_tex <- gsub(pattern = "{table}[t]", 
                     repl    = "{table}[ht!]", 
                     x       = top5_latex, fixed = T )
    top5_tex
    
    top5_tex %>%
      cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\Public_Water_Supply_top5_",eyear,".tex",sep = ''))
    
  } else {
    
    top5 <- sqldf(paste('SELECT Facility_HydroID, Facility, 
                        Locality, 
                        CASE 
                        WHEN GW_Type > 0 AND SW_Type IS NULL
                        THEN "GW"
                        WHEN SW_Type > 0 AND GW_Type IS NULL
                        THEN "SW"
                        WHEN GW_Type > 0 AND SW_Type > 0
                        THEN "SW/GW"
                        END AS Type,
                        "" AS "Major Source",
                        multi_yr_avg,
                        mgd,
                        Use_Type AS Category
                FROM data_all_fac
                WHERE Use_Type LIKE',paste('"',u,'"', sep = ''),'
                ORDER BY mgd DESC
                LIMIT 5',sep = ''))
    
    #KABLE
    top5_latex <- kable(top5[2:7],'latex', booktabs = T, align = c('l','l','c','l','c','c') ,
                        caption = paste("Highest Reported",u,"Withdrawals in",eyear,"(MGD)",sep=" "),
                        label = paste("Highest Reported",u,"Withdrawals in",eyear,"(MGD)",sep=" "),
                        col.names = c(
                          'Facility',
                          'Locality',
                          'Type',
                          'Major Source',
                          paste((eyear-syear)+1,"Year Avg."),
                          paste(eyear, 'Withdrawal', sep = ' '))) %>%
      kable_styling(latex_options = c("striped", "scale_down")) %>%
      column_spec(1, width = "12em")
    
    #CUSTOM LATEX CHANGES
    #insert hold position header
    top5_tex <- gsub(pattern = "{table}[t]", 
                     repl    = "{table}[ht!]", 
                     x       = top5_latex, fixed = T )
    top5_tex
    
    top5_tex %>%
      cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\",u,"_top5_",eyear,".tex",sep = ''))
    
  }}


#cat_table <- cat_table2
###### bySourceType - tables 5,7,9,11,13,16,19 ##################################################
#change avg column name 
colnames(cat_table)[8] <- paste((eyear-syear)+1,"Year Avg.")

#ag
agtable5 <- cat_table[c(1,7,13),-2]
rownames(agtable5) <- c()

ag_tex <- kable(agtable5, booktabs = T, align = c('l','c','c','c','c','c','c','c'),
                caption = paste(syear,"-",eyear,"Agriculture Water Withdrawals by Source Type (MGD)",sep=" "),
                label = paste(syear,"-",eyear,"Agriculture Water Withdrawal Trends",sep=" "),
                col.names = c("Source Type",
                              colnames(agtable5[2:8]))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 

#CUSTOM LATEX CHANGES
#insert hold position header
ag_tex <- gsub(pattern = "{table}[t]", 
               repl    = "{table}[ht!]", 
               x       = ag_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
ag_tex <- gsub(pattern = "{lccccccc}", 
               repl    = "{lccccccp{2cm}}", 
               x       = ag_tex, fixed = T )

ag_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\Agriculture_table",file_ext,sep = ''))
### BAR GRAPH ###################################################################################
#transform wide to long table
agtable5 <- agtable5[-3,-8]
colnames(agtable5)[colnames(agtable5)=="Source Type"] <- "Source"
colnames(agtable5)[colnames(agtable5)==paste((eyear-syear)+1,"Year Avg.")] <- "Average"
agtable5 <- pivot_longer(data = agtable5, cols = paste0(syear):paste0(eyear), names_to = "Year", values_to = "MGD")

#plot bar graph
ggplot(data=agtable5, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(aes(yintercept = agtable5$Average, colour = Source), size = .8, linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=15),  # X axis title
        axis.title.y=element_text(size=15),
        axis.text.x = element_text(size=15, vjust = 1),
        axis.text.y = element_text(size=12),
        plot.margin = unit(c(0,5,1,1), "lines"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12)) + # This widens the right margin
  coord_cartesian(xlim = c(1,5), clip = "off") +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.8) +
  annotate("text", y=agtable5$Average, x=5.85, label = paste(agtable5$Average, " MGD")) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_colour_brewer(palette = "Dark2", direction = -1, name = "5 Year Avg. (MGD)")

filename <- paste("Agriculture",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/overleaf",sep = " "), width=12, height=6)

#### irrig ######################################################################################
#irrig
irrigtable7 <- cat_table[c(3,9,15),-2]
rownames(irrigtable7) <- c()

irrig_tex <- kable(irrigtable7,  booktabs = T, align = c('l','c','c','c','c','c','c','c'),
                   caption = paste(syear,"-",eyear,"Irrigation Water Withdrawals by Source Type (MGD)",sep=" "),
                   label = paste(syear,"-",eyear,"Irrigation Water Withdrawal Trends",sep=" "),
                   col.names = c("Source Type",
                                 colnames(irrigtable7[2:8]))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 

#CUSTOM LATEX CHANGES
#insert hold position header
irrig_tex <- gsub(pattern = "{table}[t]", 
                  repl    = "{table}[ht!]", 
                  x       = irrig_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
irrig_tex <- gsub(pattern = "{lccccccc}", 
                  repl    = "{lccccccp{2cm}}", 
                  x       = irrig_tex, fixed = T )

irrig_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\Irrigation_table",file_ext,sep = ''))

### BAR GRAPH ##################################################################################
#transform wide to long table
irrigtable7 <- irrigtable7[-3,-8]
colnames(irrigtable7)[colnames(irrigtable7)=="Source Type"] <- "Source"
colnames(irrigtable7)[colnames(irrigtable7)==paste((eyear-syear)+1,"Year Avg.")] <- "Average"
irrigtable7 <- pivot_longer(data = irrigtable7, cols = paste0(syear):paste0(eyear), names_to = "Year", values_to = "MGD")

#plot bar graph
ggplot(data=irrigtable7, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(aes(yintercept = irrigtable7$Average, colour = Source), size = .8, linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=15),  # X axis title
        axis.title.y=element_text(size=15),
        axis.text.x = element_text(size=15, vjust = 1),
        axis.text.y = element_text(size=12),
        plot.margin = unit(c(0,5,1,1), "lines"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12)) + # This widens the right margin
  coord_cartesian(xlim = c(1,5), clip = "off") +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.8) +
  annotate("text", y=irrigtable7$Average, x=5.85, label = paste(irrigtable7$Average, " MGD")) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_colour_brewer(palette = "Dark2", direction = -1, name = "5 Year Avg. (MGD)")


filename <- paste("Irrigation",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf",sep = " "), width=12, height=6)

##### commercial####################################################################################
commtable9 <- cat_table[c(2,8,14),-2]
rownames(commtable9) <- c()

comm_tex <- kable(commtable9,  booktabs = T, align = c('l','c','c','c','c','c','c','c'),
                  caption = paste(syear,"-",eyear,"Commercial Water Withdrawals by Source Type (MGD)",sep=" "),
                  label = paste(syear,"-",eyear,"Commercial Water Withdrawal Trends",sep=" "),
                  col.names = c("Source Type",
                                colnames(commtable9[2:8]))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 

#CUSTOM LATEX CHANGES
#insert hold position header
comm_tex <- gsub(pattern = "{table}[t]", 
                 repl    = "{table}[ht!]", 
                 x       = comm_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
comm_tex <- gsub(pattern = "{lccccccc}", 
                 repl    = "{lccccccp{2cm}}", 
                 x       = comm_tex, fixed = T )

comm_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\Commercial_table",file_ext,sep = ''))

### BAR GRAPH ####################################################################################
#transform wide to long table
commtable9 <- commtable9[-3,-8]
colnames(commtable9)[colnames(commtable9)=="Source Type"] <- "Source"
colnames(commtable9)[colnames(commtable9)==paste((eyear-syear)+1,"Year Avg.")] <- "Average"
commtable9 <- pivot_longer(data = commtable9, cols = paste0(syear):paste0(eyear), names_to = "Year", values_to = "MGD")

#plot bar graph
ggplot(data=commtable9, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(aes(yintercept = commtable9$Average, colour = Source), size = .8, linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=15),  # X axis title
        axis.title.y=element_text(size=15),
        axis.text.x = element_text(size=15, vjust = 1),
        axis.text.y = element_text(size=12),
        plot.margin = unit(c(0,5,1,1), "lines"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12)) + # This widens the right margin
  coord_cartesian(xlim = c(1,5), clip = "off") +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.8) +
  annotate("text", y=commtable9$Average, x=5.85, label = paste(commtable9$Average, " MGD")) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_colour_brewer(palette = "Dark2", direction = -1, name = "5 Year Avg. (MGD)")


filename <- paste("Commercial",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf",sep = " "), width=12, height=6)


###mining #########################################################################################
#mining
mintable11 <- cat_table[c(5,11,17),-2]
rownames(mintable11) <- c()

min_tex <- kable(mintable11,  booktabs = T, align = c('l','c','c','c','c','c','c','c'),
                 caption = paste(syear,"-",eyear,"Mining Water Withdrawals by Source Type (MGD)",sep=" "),
                 label = paste(syear,"-",eyear,"Mining Water Withdrawal Trends",sep=" "),
                 col.names = c("Source Type",
                               colnames(mintable11[2:8]))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 

#CUSTOM LATEX CHANGES
#insert hold position header
min_tex <- gsub(pattern = "{table}[t]", 
                repl    = "{table}[ht!]", 
                x       = min_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
min_tex <- gsub(pattern = "{lccccccc}", 
                repl    = "{lccccccp{2cm}}", 
                x       = min_tex, fixed = T )

min_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\Mining_table",file_ext,sep = ''))

### BAR GRAPH #############################################################################
#transform wide to long table
mintable11 <- mintable11[-3,-8]
colnames(mintable11)[colnames(mintable11)=="Source Type"] <- "Source"
colnames(mintable11)[colnames(mintable11)==paste((eyear-syear)+1,"Year Avg.")] <- "Average"
mintable11 <- pivot_longer(data = mintable11, cols = paste0(syear):paste0(eyear), names_to = "Year", values_to = "MGD")

#plot bar graph
ggplot(data=mintable11, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(aes(yintercept = mintable11$Average, colour = Source), size = .8, linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=15),  # X axis title
        axis.title.y=element_text(size=15),
        axis.text.x = element_text(size=15, vjust = 1),
        axis.text.y = element_text(size=12),
        plot.margin = unit(c(0,5,1,1), "lines"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12)) + # This widens the right margin
  coord_cartesian(xlim = c(1,5), clip = "off") +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.8) +
  annotate("text", y=mintable11$Average, x=5.85, label = paste(mintable11$Average, " MGD")) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_colour_brewer(palette = "Dark2", direction = -1, name = "5 Year Avg. (MGD)")
  
#+ annotate("text", y=mintable11$Average-1.8, x=.79, label ="5 Year Avg.") 
#+ annotate("text", y=mintable11$Average-3, x=.79, label = paste('=',mintable11$Average, " MGD"))


filename <- paste("Mining",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf",sep = " "), width=12, height=6)


####manuf #####################################################################################
#manuf
mantable13 <- cat_table[c(4,10,16),-2]
rownames(mantable13) <- c()

man_tex <- kable(mantable13,  booktabs = T, align = c('l','c','c','c','c','c','c','c'),
                 caption = paste(syear,"-",eyear,"Manufacturing and Industrial Water Withdrawals by Source Type (MGD)",sep=" "),
                 label = paste(syear,"-",eyear,"Manufacturing and Industrial Water Withdrawal Trends",sep=" "),
                 col.names = c("Source Type",
                               colnames(mantable13[2:8]))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 

#CUSTOM LATEX CHANGES
#insert hold position header
man_tex <- gsub(pattern = "{table}[t]", 
                repl    = "{table}[ht!]", 
                x       = man_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
man_tex <- gsub(pattern = "{lccccccc}", 
                repl    = "{lccccccp{2cm}}", 
                x       = man_tex, fixed = T )

man_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\Manufacturing_table",file_ext,sep = ''))

### BAR GRAPH #################################################################################
#transform wide to long table
mantable13 <- mantable13[-3,-8]
colnames(mantable13)[colnames(mantable13)=="Source Type"] <- "Source"
colnames(mantable13)[colnames(mantable13)==paste((eyear-syear)+1,"Year Avg.")] <- "Average"
mantable13 <- pivot_longer(data = mantable13, cols = paste0(syear):paste0(eyear), names_to = "Year", values_to = "MGD")

#plot bar graph
ggplot(data=mantable13, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(aes(yintercept = mantable13$Average, colour = Source), size = .8, linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=15),  # X axis title
        axis.title.y=element_text(size=15),
        axis.text.x = element_text(size=15, vjust = 1),
        axis.text.y = element_text(size=12),
        plot.margin = unit(c(0,5,1,1), "lines"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12)) + # This widens the right margin
  coord_cartesian(xlim = c(1,5), clip = "off") +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.8) +
  annotate("text", y=mantable13$Average, x=5.85, label = paste(mantable13$Average, " MGD")) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_colour_brewer(palette = "Dark2", direction = -1, name = "5 Year Avg. (MGD)")


filename <- paste("Manufacturing",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf",sep = " "), width=12, height=6)


#####municipal aka public water supply ########################################################
#muni aka pws
munitable16 <- cat_table[c(6,12,18),-2]
rownames(munitable16) <- c()

muni_tex <- kable(munitable16,  booktabs = T, align = c('l','c','c','c','c','c','c','c'),
                  caption = paste(syear,"-",eyear,"Public Water Supply Water Withdrawals by Source Type (MGD)",sep=" "),
                  label = paste(syear,"-",eyear,"Public Water Supply Water Withdrawal Trends",sep=" "),
                  col.names = c("Source Type",
                                colnames(munitable16[2:8]))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  row_spec(row = 3, bold = TRUE) 

#CUSTOM LATEX CHANGES
#insert hold position header
muni_tex <- gsub(pattern = "{table}[t]", 
                 repl    = "{table}[ht!]", 
                 x       = muni_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
muni_tex <- gsub(pattern = "{lccccccc}", 
                 repl    = "{lccccccp{2cm}}", 
                 x       = muni_tex, fixed = T )

muni_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\Public_Water_supply_table",file_ext,sep = ''))


###BAR GRAPH ############################################################################
#transform wide to long table
munitable16 <- munitable16[-3,-8]
colnames(munitable16)[colnames(munitable16)=="Source Type"] <- "Source"
colnames(munitable16)[colnames(munitable16)==paste((eyear-syear)+1,"Year Avg.")] <- "Average"
munitable16 <- pivot_longer(data = munitable16, cols = paste0(syear):paste0(eyear), names_to = "Year", values_to = "MGD")

#plot bar graph
ggplot(data=munitable16, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  geom_hline(aes(yintercept = munitable16$Average, colour = Source), size = .8, linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=15),  # X axis title
        axis.title.y=element_text(size=15),
        axis.text.x = element_text(size=15, vjust = 1),
        axis.text.y = element_text(size=12),
        plot.margin = unit(c(0,5,1,1), "lines"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12)) + # This widens the right margin
  coord_cartesian(xlim = c(1,5), clip = "off") +
  geom_text(aes(label=MGD),
            position=position_dodge(width=0.9), 
            vjust = -.8) +
  annotate("text", y=munitable16$Average, x=5.85, label = paste(munitable16$Average, " MGD")) +
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_colour_brewer(palette = "Dark2", direction = -1, name = "5 Year Avg. (MGD)")


filename <- paste("Public Water Supply",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf",sep = " "), width=12, height=6)


### POWER
### POWER PULL FROM VAHYDRO - REPLACE WITH POWER FILTER FROM MULTI_YR_DATA AFTER FIXING TOP SECTION TO PULL WITHOUT FILTER ON POWER ####################################
a <- c(
  'fossilpower',
  'nuclearpower'
)
b <- c('Groundwater', 'Surface Water', 'Total (GW + SW)')
cat_table<- data.frame(expand.grid(a,b))

colnames(cat_table) <- c('Use_Type', 'Source_Type')
cat_table <- arrange(cat_table, Source_Type, Use_Type )
#cat_table = FALSE
syear = 2016
eyear = 2020
year.range <- syear:eyear

multi_yr_data <- list()

for (y in year.range) {
  
  print(y)
  startdate <- paste(y, "-01-01",sep='')
  enddate <- paste(y, "-12-31", sep='')
  
  localpath <- tempdir()
  filename <- paste("data.all_",y,".csv",sep="")
  destfile <- paste(localpath,filename,sep="\\") 
  
  #has 3 issuing authorities, ONLY power
  download.file(paste("https://deq1.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=contains&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), destfile = destfile, method = "libcurl")  
  data.power <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
  
  data_power <- data.power
  
  #remove duplicates (keeps one row)
  data_power <- distinct(data_power, MP_hydroid, .keep_all = TRUE)
  
  #remove hydropower
  data_power <- data_power %>% filter(Use.Type != "hydropower")
  
  if (length(which(data_power$Use.Type=='facility')) > 0) {
    data_power <- data_power[-which(data_power$Use.Type=='facility'),]
  }
  #rename columns
  colnames(data_power) <- c('HydroID', 'Hydrocode', 'Source_Type',
                            'MP_Name','Facility_hydroid','Facility', 'Use_Type', 'Year',
                            'mgy', 'mgd', 'lat', 'lon', 'fips','locality')
  
  data_power$mgd <- data_power$mgy/365
  #make use type values lowercase
  data_power$Use_Type <- str_to_lower(data_power$Use_Type)
  #change 'Well' and 'Surface Water Intake' values in source_type column to match report headers
  levels(data_power$Source_Type) <- c(levels(data_power$Source_Type), "Groundwater", "Surface Water")
  data_power$Source_Type[data_power$Source_Type == 'Well'] <- 'Groundwater'
  data_power$Source_Type[data_power$Source_Type == 'Surface Water Intake'] <- 'Surface Water'
  
  #combine each year of data into a single table
  multi_yr_data <- rbind(multi_yr_data, data_power)
  
  #begin summary table 1 manipulation
  catsourcesum <- data_power %>% group_by(Use_Type, Source_Type)
  
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

#cat_table_power <- cat_table <- cat_table_raw

cat_table <- data.frame(cat_table[2],cat_table[1],cat_table[3:(length(year.range)+2)])
names(cat_table) <- c('Source Type', 'Category', year.range)

cat_table$multi_yr_avg <- round((rowMeans(cat_table[3:(length(year.range)+2)], na.rm = FALSE, dims = 1)),2)

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

#### ADD BOTTOM ROW OF TOTALS TO TABLE######################
# ADD BOTTOM ROW OF TOTALS TO TABLE
cat_table.total <- cat_table[c(1:4),]
multi_yr_avg.sums <- mean(c(sum(cat_table.total[3]),
                            sum(cat_table.total[4]),
                            sum(cat_table.total[5]),
                            sum(cat_table.total[6]),
                            sum(cat_table.total[7])))

total_pct_chg <- round(((sum(cat_table.total[7])-multi_yr_avg.sums)/multi_yr_avg.sums)*100, 1)

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

colnames(catsum.sums) <- c('Source Type', 'Category',year.range,'multi_yr_avg',paste('% Change',eyear,'to Avg.'))
cat_table <- rbind(cat_table,catsum.sums)

print(cat_table)
### POWER TABLE 19###########################################################
#Table 19: 20xx-20xx Power Generation Water Withdrawals by Source Type (MGD)
powtable19 <- rbind(cat_table[1:2,],cat_table[7,],cat_table[3:4,],cat_table[8,],cat_table[9,])
rownames(powtable19) <- NULL

powtable19 <- sqldf(paste0('SELECT "Source Type",
                    CASE 
                    WHEN "Category" LIKE "%fossil%"
                    THEN "Fossil"
                    WHEN "Category" LIKE "%nuclear%"
                    THEN "Nuclear"
                    ELSE "Category"
                    END AS "Category",
                     "',year.range[1],'", "',year.range[2],'", "',year.range[3],'", "',year.range[4],'", "',year.range[5],'", "multi_yr_avg", "% Change ',eyear,' to Avg."
                    FROM powtable19'))

pow_tex <- kable(powtable19[2:9], booktabs = T, align = c('l','c','c','c','c','c','c','c'),
                 caption = paste(syear,"-",eyear,"Power Generation Water Withdrawals by Source Type (MGD)",sep=" "),
                 label = paste(syear,"-",eyear,"Power Generation Water Withdrawal Trends(MGD)",sep=" "),
                 col.names = c("Power Type",
                               colnames(powtable19[3:7]),
                               paste((eyear-syear)+1,"Year Avg."),
                               colnames(powtable19[9]))) %>%
  kable_styling(latex_options = c("scale_down")) %>%
  pack_rows("Groundwater", 1, 3,  hline_after = F) %>%
  pack_rows("Surface Water", 4, 6, hline_before = T, hline_after = F) %>%
  row_spec(7, bold=T, extra_css = "border-top: 1px solid")

#CUSTOM LATEX CHANGES
#insert hold position header
pow_tex <- gsub(pattern = "{table}[t]", 
                repl    = "{table}[ht!]", 
                x       = pow_tex, fixed = T )

#make last column name wrap on 2 rows (adjusts column width) 
pow_tex <- gsub(pattern  = "{lccccccc}", 
                repl    = "{lrrrrr>{\\raggedleft\\arraybackslash}p{5em}>{\\raggedleft\\arraybackslash}p{6em}}", 
                x       = pow_tex, fixed = T,useBytes = F )

pow_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\Power_table",file_ext,sep = ''))



pow_wide <- pivot_wider(data = multi_yr_data, id_cols = c(HydroID, Source_Type, MP_Name, Facility_hydroid, Facility,Use_Type, fips), names_from = Year, values_from = mgy)

write.csv(x = pow_wide, file = paste0("U:\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_all_wide_power_",syear,"-",eyear,".csv"))

#JM: pow_facs is not used elsewhere - can probably delete

# pow_facs <- sqldf('SELECT Facility_HydroID, Facility, Use_Type, sum("2016") as "2016", sum("2017") as "2017", sum("2018") as "2018", sum("2019") as "2019", sum("2020") as "2020"
#       FROM pow_wide
#       GROUP BY Facility_hydroid')
# 
# pfacs <- sqldf('SELECT *
#       FROM pow_facs 
#       WHERE "2020" IS NOT NULL')
# sqldf('SELECT Facility_HydroID, Facility, Use_Type, sum("2016") as "2016", sum("2017") as "2017", sum("2018") as "2018", sum("2019") as "2019", sum("2020") as "2020"
#       FROM pfacs
#       GROUP BY Facility_hydroid')

### POWER BAR GRAPH FIGURE 26 ############################################################

#transform wide to long table
power <- cat_table[1:4,-9]
colnames(power) <- c('Source', 'Power', year.range, 'Average')

power <- gather(power,Year, MGD, paste(syear):paste(eyear), factor_key = TRUE)

mean_mgd <- power[1:4,1:3]
colnames(mean_mgd) <- c('Source', 'Power', 'MGD')
mean_mgd_gw <- sqldf('SELECT * FROM mean_mgd WHERE Source LIKE "Groundwater"')
mean_mgd_sw <- sqldf('SELECT * FROM mean_mgd WHERE Source LIKE "Surface Water"')

#plot bar graph
ggplot(data=power, aes(x=Year, y=MGD, fill = Source)) +
  geom_col(position=position_dodge(), colour = "gray") + 
  #geom_hline(data = mean_mgd,aes(yintercept = MGD), size = .7, colour = "black",linetype = "dashed") +
  geom_segment(data = mean_mgd, x = .5, xend = 5.5, aes(y = MGD, yend = MGD, colour = Source), size = .8, linetype = "dashed") +
  labs( y="Million Gallons per Day", fill = "Source Type") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "light gray", size=.3),
        legend.position="bottom", 
        legend.box = "horizontal",
        axis.title.x=element_text(size=15),  # X axis title
        axis.title.y=element_text(size=15),
        axis.text.x = element_text(size=15, vjust = 1),
        axis.text.y = element_text(size=12),
        plot.margin = unit(c(0,2,1,1), "lines"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12)) + # This widens the right margin
  coord_cartesian(xlim = c(1,5.61), clip = "off") +
  geom_text(aes(label=MGD),
            position = position_stack(vjust = .5), 
            vjust = -.2)+
  scale_fill_brewer(palette = "Dark2", direction = -1) +
  scale_colour_brewer(palette = "Dark2", direction = -1, name = "5 Year Avg. (MGD)") +
  geom_text(data = mean_mgd, aes( y = MGD, label = paste0(MGD, " \n MGD")), x = 5.8) +
  facet_grid(Source~Power, scales = "free_y")



# #plot bar graph
# ggplot(data=power, aes(x=Year, y=MGD, fill = Source)) +
#   geom_col(position=position_dodge(), colour = "gray") + 
#   geom_hline(data = mean_mgd,aes(yintercept = MGD), size = .7, colour = "black",linetype = "dashed") +
#   #geom_hline(yintercept = power$Average, size = .4, colour = "black",linetype = "dashed") +
#   labs( y="Million Gallons per Day", fill = "Source Type") +
#   theme(panel.background = element_rect(fill = "white"),
#         panel.grid.major.y = element_line(colour = "light gray", 
#                                           size=.3),
#         legend.position="bottom", 
#         legend.box = "horizontal",
#         axis.title.x=element_text(size=14),  # X axis title
#         axis.title.y=element_text(size=14),
#         axis.text.x = element_text(size=11, vjust = 1)) +
#   scale_fill_brewer(palette = "Dark2", direction = -1) +
#   geom_text(aes(label=MGD),
#             position = position_stack(vjust = .5), 
#             vjust = -.2)+
#   facet_grid(Source~Power, scales = "free_y") 

filename <- paste("Power",paste(syear,"-",eyear, sep = ""),"Bar_Graph.pdf", sep="_")
ggsave(file=filename, path = paste("U:/OWS/Report Development/Annual Water Resources Report/October",eyear+1,"Report/Overleaf/",sep = " "), width=12, height=6)

################### TOP USERS BY USE TYPE  ############################

#Table: Highest Reported  Withdrawals in eyear (MGD)
#make Category values capital
multi_yr_data$Use_Type <- str_to_title(multi_yr_data$Use_Type)
multi_yr_data$Facility <- str_to_title(multi_yr_data$Facility)
#transform from long to wide table
data_all <- pivot_wider(data = multi_yr_data, id_cols = c(HydroID, Source_Type, MP_Name, Facility_hydroid, Facility,Use_Type, fips), names_from = Year, values_from = mgy)

data_all <- sqldf('SELECT a.*, b.name AS Locality
                  FROM data_all a
                  LEFT OUTER JOIN fips b
                  ON a.fips = b.code')

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
data_all_fac <- sqldf(paste('SELECT Facility_HydroID, Facility, Source_Type, Use_Type, Locality, round((sum(',paste('"',eyear,'"', sep = ''),')/365),1) AS mgd, round((sum(multi_yr_avg)/365),1) as multi_yr_avg, sum(GW_type) AS GW_type, sum(SW_type) AS SW_type
                      FROM data_all
                      GROUP BY Facility_HydroID',sep = ''))

top5 <- sqldf(paste('SELECT Facility_HydroID, Facility, 
                        Locality, 
                        CASE 
                        WHEN GW_Type > 0 AND SW_Type IS NULL
                        THEN "GW"
                        WHEN SW_Type > 0 AND GW_Type IS NULL
                        THEN "SW"
                        WHEN GW_Type > 0 AND SW_Type > 0
                        THEN "SW/GW"
                        END AS Type,
                        "" AS "Major Source",
                        multi_yr_avg,
                        mgd,
                        Use_Type AS Category
                FROM data_all_fac
                WHERE Use_Type LIKE "%power%"
                ORDER BY mgd DESC
                LIMIT 5',sep = ''))

#KABLE
top5_latex <- kable(top5[2:7],'latex', booktabs = T, align = c('l','l','c','l','c','c') ,
                    caption = paste("Highest Reported Power Generation Withdrawals in",eyear,"(MGD)",sep=" "),
                    label = paste("Highest Reported Power Generation Withdrawals in",eyear,"(MGD)",sep=" "),
                    col.names = c(
                      'Facility',
                      'Locality',
                      'Type',
                      'Major Source',
                      paste((eyear-syear)+1,"Year Avg."),
                      paste(eyear, 'Withdrawal', sep = ' '))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  column_spec(1, width = "12em")

#CUSTOM LATEX CHANGES
#insert hold position header
top5_tex <- gsub(pattern = "{table}[t]", 
                 repl    = "{table}[ht!]", 
                 x       = top5_latex, fixed = T )
top5_tex

top5_tex %>%
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\Power_top5_",eyear,".tex",sep = ''))
