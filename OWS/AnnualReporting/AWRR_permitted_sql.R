#library("readxl")
library('dplyr')
library('httr')
library('sqldf')
library('stringr')
library("tidyr") # GM add library needed for pivot_wider
library("kableExtra") # GM add library needed for latex

y <- 2020
eyear <- y
print(y)
startdate <- paste(y, "-01-01",sep='')
enddate <- paste(y, "-12-31", sep='')

localpath <- tempdir()
filename <- paste("data.all_",y,".csv",sep="")
destfile <- paste(localpath,filename,sep="\\")  
download.file(paste("https://deq1.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), destfile = destfile, method = "libcurl")  
data.all <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")

data1 <- data.all #GM edit data -> data1 throughout so we done use a reserved word

#remove duplicates (keeps one row)
data1 <- distinct(data1, MP_hydroid, Year, .keep_all = TRUE)
#exclude dalecarlia
data1 <- data1[-which(data1$Facility=='DALECARLIA WTP'),]

if (length(which(data1$Use.Type=='facility')) > 0) {
  data1 <- data1[-which(data1$Use.Type=='facility'),]
}
#rename columns
# colnames(data1) <- c('HydroID', 'Hydrocode', 'Source_Type',
#                     'MP_Name', 'Facility', 'Use_Type', 'Year',
#                     'mgy', 'mgd', 'lat', 'lon', 'locality')

#GM edit to add 'Planner' title to OWS planner column, and use names() instead of colnames()
names(data1) <- c('HydroID',
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
                    'locality',
                    'Planner')

data1$mgd <- data1$mgy/365
sum(data1$mgy)
#make use type values lowercase
data1$Use_Type <- str_to_lower(data1$Use_Type)
#change 'Well' and 'Surface Water Intake' values in source_type column to match report headers
#GM if we can do this without levels, that would be better for future
levels(data1$Source_Type) <- c(levels(data1$Source_Type), "Groundwater", "Surface Water")
data1$Source_Type[data1$Source_Type == 'Well'] <- 'Groundwater'
data1$Source_Type[data1$Source_Type == 'Surface Water Intake'] <- 'Surface Water'


data1$Use_Type[data1$Use_Type == 'industrial'] <- 'manufacturing'

##################################################################
data1 <- sqldf(paste0('SELECT *
                FROM data1
                WHERE Year = ',eyear,''))
#GM data1 not multi_yr_data


#PULL IN PERMITTED MPs (NO VWUDS) 
download.file(paste("https://deq1.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200",sep=""), destfile = destfile, method = "libcurl")  
datap <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")

sqldf("select count(*) from datap")

# #remove duplicates (keeps one row for each year)
datap <- sqldf('SELECT "MP_hydroid","Hydrocode","Source.Type","MP.Name","Facility_hydroid","Facility","Use.Type","Month","Year",max("Water.Use.MGY") AS "Water.Use.MGM","Latitude","Longitude","Locality","FIPS.Code" 
               FROM datap
               GROUP BY "MP_hydroid","Hydrocode","Source.Type","MP.Name","Facility_hydroid","Facility","Use.Type","Month","Year","Latitude","Longitude","Locality","FIPS.Code"
                ORDER BY "Water.Use.MGY" DESC ')

qpi = "SELECT a.*, CASE WHEN b.MP_hydroid is not NULL THEN 1 ELSE 0 END as has_permit
  from data1 as a left outer join datap as b
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

data_pi$Use_Type <- str_to_title(data_pi$Use_Type)


########## STATIC DATA #######################################################################################

#save the multi_yr_data to use for data reference - we can refer to that csv when asked questions about the data
#write.csv(data_pi, paste("U:\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_permitted_",eyear,".csv",sep = ""), row.names = F)
#GM uncomment when run 2021 ^

data_pi <- read.csv(file = paste("U:\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_permitted_",eyear,".csv",sep = ""))
data_pi$Use_Type <- recode(data_pi$Use_Type, "Municipal" = "Public Water Supply")
## DEPRECATED - FORMAT Table 2: 20XX Permitted and Unpermitted (Excluded) Withdrawals (MGD) ################################
# permit_srctype <- sqldf(
#   "select Source_type, has_permit, 
#   ROUND(sum(mgd),2) AS mgd, count(*) 
#   from data_pi 
#   group by Source_type, has_permit"
# )
# 
# table2_bysrc <- sqldf('SELECT Source_Type AS "Source Type", CASE 
#                     WHEN has_permit = 1 
#                     THEN "Permitted"
#                     WHEN has_permit = 0 
#                     THEN "Unpermitted"
#                     END AS "Withdrawal Type",
#                     mgd AS "Withdrawal Amount"
#                     FROM permit_srctype
#                     ORDER BY Source_Type, has_permit desc
#                     ')
# gw_perm_pct <- round((table2_bysrc[1,3] / sqldf('SELECT sum("Withdrawal Amount")
#                                FROM table2_bysrc
#                                GROUP BY "Source Type"')[1,]) * 100,2)
# gw_unperm_pct <- round((table2_bysrc[2,3] / sqldf('SELECT sum("Withdrawal Amount")
#                                FROM table2_bysrc
#                                GROUP BY "Source Type"')[1,]) * 100,2)
# sw_perm_pct <- round((table2_bysrc[3,3] / sqldf('SELECT sum("Withdrawal Amount")
#                                FROM table2_bysrc
#                                GROUP BY "Source Type"')[2,]) * 100,2)
# sw_unperm_pct <- round((table2_bysrc[4,3] / sqldf('SELECT sum("Withdrawal Amount")
#                                FROM table2_bysrc
#                                GROUP BY "Source Type"')[2,]) * 100,2)
# 
# table2_total <- sqldf('SELECT "Total" AS "Source Type", CASE 
#                     WHEN has_permit = 1 
#                     THEN "Permitted"
#                     WHEN has_permit = 0 
#                     THEN "Unpermitted"
#                     END AS "Withdrawal Type",
#                     sum(mgd) AS "Withdrawal Amount"
#                       FROM permit_srctype
#                       GROUP BY has_permit
#                       ORDER BY Source_Type, has_permit desc')
# tot_perm_pct <- round((table2_total[1,3] / sqldf('SELECT sum("Withdrawal Amount")
#                                FROM table2_total')) * 100,2)
# tot_unperm_pct <-  round((table2_total[2,3] / sqldf('SELECT sum("Withdrawal Amount")
#                                FROM table2_total')) * 100,2)
# 
# pct <- rbind(gw_perm_pct,gw_unperm_pct,sw_perm_pct,sw_unperm_pct,tot_perm_pct,tot_unperm_pct)
# table2 <- cbind(rbind(table2_bysrc,table2_total),pct)
# 
# #remove clutter 
# rm(gw_perm_pct,gw_unperm_pct,sw_perm_pct,sw_unperm_pct,tot_perm_pct,tot_unperm_pct,table2_total,table2_bysrc,pct)
# 
# #KABLE
# table2_latex <- kable(table2[2:4],'latex', booktabs = T, align =  c('l','c','c'),
#                       caption = paste(eyear, "Permitted and Unpermitted (Excluded) Withdrawals (MGD)",sep=" "),
#                       label = paste(eyear, "Permitted and Unpermitted (Excluded) Withdrawals (MGD)",sep=" "),
#                       col.names = c( 'Withdrawal Type',
#                                      paste(eyear,"Withdrawal Amount",sep = ' '),
#                                      '% of Total')) %>%
#   kable_styling(latex_options = c("striped"), full_width = F,position = "center", font_size = 12) %>%
#   pack_rows("Groundwater", 1, 2, hline_before = T, hline_after = F) %>%
#   pack_rows("Surface Water", 3, 4, hline_before = T, hline_after = F) %>%
#   pack_rows("Total (GW + SW)", 5, 6, hline_before = T, hline_after = F)
# 
# #CUSTOM LATEX CHANGES
# #insert hold position header
# table2_tex <- gsub(pattern = "{table}[t]", 
#                    repl    = "{table}[ht!]", 
#                    x       = table2_latex, fixed = T )
# table2_tex %>%
#   cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\summary_table2_",eyear,".tex",sep = ''))

## DEPRECATED - FORMAT Table 3: 20XX Permitted and Unpermitted (Excluded) By Use Type Withdrawals (MGD) ###############
permit_src_use <- sqldf(
  "select Source_type, Use_Type, has_permit, ROUND(sum(mgd),2) AS mgd, count(*) 
  from data_pi 
  group by Source_type, Use_Type, has_permit"
)

table3_gw <- sqldf('SELECT Source_Type, Use_Type, CASE 
                    WHEN has_permit = 1 
                    THEN "Permitted"
                    WHEN has_permit = 0 
                    THEN "Unpermitted"
                    END AS "Withdrawal Type",
                    mgd AS "Withdrawal Amount",
                    round((mgd / 
                    (SELECT sum(a.mgd)
                    FROM permit_src_use a
                    WHERE a.Source_Type = "Groundwater")) * 100,2)
                    AS "pct_of_total"
                   FROM permit_src_use
                   WHERE Source_Type = "Groundwater"')

table3_gw_tot <- sqldf('SELECT "Total Groundwater" AS Source_Type, 
              "Total Groundwater" AS Use_Type,
              "" AS "Withdrawal Type",
              sum("Withdrawal Amount") AS "Withdrawal Amount",
              round(sum(pct_of_total),1) AS pct_of_total
      FROM table3_gw')

table3_sw <- sqldf('SELECT Source_Type, Use_Type, CASE 
                    WHEN has_permit = 1 
                    THEN "Permitted"
                    WHEN has_permit = 0 
                    THEN "Unpermitted"
                    END AS "Withdrawal Type",
                    mgd AS "Withdrawal Amount",
                    round((mgd / 
                    (SELECT sum(a.mgd)
                    FROM permit_src_use a
                    WHERE a.Source_Type = "Surface Water")) * 100,2)
                    AS "pct_of_total"
                   FROM permit_src_use
                   WHERE Source_Type = "Surface Water"')

table3_sw_tot <- sqldf('SELECT "Total Surface Water" AS Source_Type, 
              "Total Surface Water" AS Use_Type,
              "" AS "Withdrawal Type",
              sum("Withdrawal Amount") AS "Withdrawal Amount",
              round(sum(pct_of_total),1) AS pct_of_total
      FROM table3_sw')

table3 <- rbind(table3_gw,table3_gw_tot,table3_sw,table3_sw_tot)

#remove clutter
rm(table3_gw,table3_gw_tot,table3_sw,table3_sw_tot)

# #KABLE
# table3_latex <- kable(table3[2:5],'latex', booktabs = T, align =  c('l','l','c','c'),
#                       caption = paste(eyear, "Permitted and Unpermitted (Excluded) By Use Type Withdrawals (MGD)",sep=" "),
#                       label = paste(eyear, "Permitted and Unpermitted (Excluded) By Use Type Withdrawals (MGD)",sep=" "),
#                       col.names = c( 'Use Type',
#                                      'Withdrawal Type',
#                                      paste(eyear,"Withdrawal Amount",sep = ' '),
#                                      '% of Total')) %>%
#   kable_styling( full_width = F,position = "center", font_size = 10) %>%
#   pack_rows("Groundwater", 1, 13, hline_before = T, hline_after = F) %>%
#   pack_rows("Surface Water", 14, 26, hline_before = T, hline_after = F)  %>%
#   #row_spec(13,bold = T, background = "gray!6") %>%
#   #row_spec(25,bold = T, background = "gray!6") %>%
#   collapse_rows(columns = 1, valign = "top",latex_hline = 'none')
# 
# #CUSTOM LATEX CHANGES
# #insert hold position header
# table3_tex <- gsub(pattern = "{table}[t]", 
#                    repl    = "{table}[ht!]", 
#                    x       = table3_latex, fixed = T )
# 
# #remove extra characters inserted by collapse_rows because of repeating lines
# table3_tex <- gsub(pattern = "[t]{-2}{*}", 
#                    repl    = "", 
#                    x       = table3_tex, fixed= T)
# 
# #custom striping
# table3_tex <- gsub(pattern = " & Unpermitted ", 
#                    repl    = "\\rowcolor{gray!20}   & Unpermitted ", 
#                    x       = table3_tex, fixed= T)
# 
# table3_tex %>%
#   cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\Overleaf\\summary_table3_",eyear,".tex",sep = ''))


### TABLE 3 - NEW FORMATTING ################################
table3_wide <- pivot_wider(data = table3, id_cols = c("Source_Type", "Use_Type"), names_from = "Withdrawal Type", names_sep = "_", values_from = c("Withdrawal Amount", "pct_of_total"))

table3_gw_tot <- sqldf('SELECT "Total  Groundwater" AS Use_Type,
              round(sum("Withdrawal Amount_Unpermitted"),2) AS "Withdrawal Amount_Unpermitted",
              round(sum("Withdrawal Amount_Permitted"),2) AS "Withdrawal Amount_Permitted",
              round(sum("pct_of_total_Unpermitted"),2) AS "pct_of_total_Unpermitted",
              round(sum("pct_of_total_Permitted"),2) AS "pct_of_total_Permitted"
      FROM table3_wide
      WHERE Source_Type = "Groundwater"')

table3_sw_tot <- sqldf('SELECT  "Total  Surface Water" AS Use_Type,
              round(sum("Withdrawal Amount_Unpermitted"),2) AS "Withdrawal Amount_Unpermitted",
              round(sum("Withdrawal Amount_Permitted"),2) AS "Withdrawal Amount_Permitted",
              round(sum("pct_of_total_Unpermitted"),2) AS "pct_of_total_Unpermitted",
              round(sum("pct_of_total_Permitted"),2) AS "pct_of_total_Permitted"
      FROM table3_wide
      WHERE Source_Type = "Surface Water"')

table3_wide <- sqldf('SELECT "Use_Type", "Withdrawal Amount_Unpermitted", "Withdrawal Amount_Permitted", "pct_of_total_Unpermitted", "pct_of_total_Permitted" 
                     FROM table3_wide')
table3_wide <- rbind(table3_wide[1:6,], table3_gw_tot, table3_wide[8:13,], table3_sw_tot)

#KABLE
table3w_latex <- kable(table3_wide,'latex', booktabs = T, align =  c('l','l','l','l','l'),
                       caption = paste(eyear, "Permitted and Unpermitted (Excluded) By Use Type Withdrawals (MGD)",sep=" "),
                       label = paste(eyear, "Permitted and Unpermitted (Excluded) By Use Type Withdrawals (MGD)",sep=" "),
                       col.names = c( 'Use Type',
                                      'Unpermitted',
                                      'Permitted',
                                      'Unpermitted',
                                      'Permitted')) %>%
  kable_styling( full_width = F,position = "center", font_size = 10) %>%
  pack_rows("Groundwater", 1, 7, hline_before = T, hline_after = F) %>%
  pack_rows("Surface Water", 8, 14, hline_before = T, hline_after = F)  %>%
  add_header_above(c(" ", "2020 Withdrawal Amount" = 2, '% of Total' = 2)) %>%
  row_spec(7,bold = T) %>%
  row_spec(14,bold = T) %>%
  collapse_rows(columns = 1, valign = "top",latex_hline = 'none')


#CUSTOM LATEX CHANGES
#insert hold position header
table3w_tex <- gsub(pattern = "{table}[t]", 
                    repl    = "{table}[ht!]", 
                    x       = table3w_latex, fixed = T )

#remove extra characters inserted by collapse_rows because of repeating lines
table3w_tex <- gsub(pattern = "[t]{-2}{*}", 
                    repl    = "", 
                    x       = table3w_tex, fixed= T)

#custom striping
use_stripe <- c("Public Water Supply", "Manufacturing", "Commercial")
for (i in 1:length(use_stripe)) {
  table3w_tex <- gsub(pattern = paste0("\\hspace{1em}",use_stripe[i]),
                      repl    = paste0("\\rowcolor{gray!20}   \\hspace{1em}",use_stripe[i]),
                      x       = table3w_tex, fixed= T)
  
}
table3w_tex
