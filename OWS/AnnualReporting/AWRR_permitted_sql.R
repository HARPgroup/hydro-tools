# This script generates the Permitted & Unpermitted Withdrawal Table in the Annual Report
# It is an update to avoid counting unpermitted surface water intakes from facilities with GWPs as permitted SW withdrawals 
library('dplyr')
library('httr')
library('sqldf')
library('stringr')
library("tidyr")
library("kableExtra")
library(odbc) ## To connect to CEDs
## Pulls in onedrive_location for onedrive
basepath <- '/var/www/R'
source(paste0(basepath,'/config.local.private'))

#Set current year
eyear <- 2023
eyearX <- paste0("X",eyear)
Xyears <- paste0('X',(eyear+1) - 5:1)

#load foundation data
#permit only data from https://deq1.bse.vt.edu/d.dh/ows-permit-list

##Connect to Prod
conn_DBI <- dbConnect(odbc::odbc(),
                      .connection_string = "driver={SQL Server};
                      server={DEQ-SQLODS-PROD,50000};
                      database={ODS};
                      trusted_connection=yes")

getquery <- function(tableView, schema = "water") {
## Getting Permit info
## Table is flawed and the columns need to be ordered

    #Get column information on the fields in the table
    col_info <- odbc::odbcConnectionColumns(conn_DBI, tableView,schema_name=schema)
    
    #Look at those large column sizes associated with some of the fields. Those are
    #the problem columns. You can pull all the other columns with no problem. Those
    #are the ones causing the issue.
    #View(col_info)
    
    #Lets get all thos long columns 
    long_cols <- col_info$name[col_info$column_size >= 9000]
    
    #Now lets get all of the rest of the data. These are the non-problem columns:
    all_other_cols <- col_info$name[col_info$column_size < 9000]
    
    #List the columns in order for the query in form SQL expects
    colOrderForQuery <- c(all_other_cols,long_cols)
    colOrderForQuery <- paste0(colOrderForQuery,collapse='","')
    
    #Develop a query
    sql <- paste0('SELECT "',
                  colOrderForQuery,
                  '" FROM ',
                  paste0(schema,".",tableView))
    
    result <- dbGetQuery(conn_DBI,sql)
    return(result)
}

## These two tables have the weird column issue and need the workaround
gwp <- getquery("GWP_Permits_Vw")
##Format the date column
gwp$GPP_DATE_START <- as.Date(gwp$GPP_DATE_START)

vwp_cond <- getquery("VWP_Withdrawal_Condition_Vw")

sql_string <- 'SELECT * FROM water.[VWP Permits]'
vwp <-  dbGetQuery(conn_DBI,sql_string)

vwp$`Most Recent Date Issuance` <- as.Date(vwp$`Most Recent Date Issuance`)

sql_string <- 'SELECT * FROM water.Measuring_Point_Vw'
mp <-  dbGetQuery(conn_DBI,sql_string)

# facs <- dbGetQuery(conn_DBI, "SELECT * FROM ceds.CEDS_Core_Facilities_Geospatial_Data_View")

vwpdf <- sqldf('
SELECT 
c.Permit_Id, v."Permit Number" AS Permit_Number, v."CEDS Facility Id" AS Fac_Id,
v."Activity Type" AS Use_Type,c.Annual_Withdrawal_Limit AS Annual_Limit, c.Monthly_Withdrawal_Limit AS Monthly_Limit,
"Surface Water" AS Permit_Type, v."Most Recent Date Issuance" AS Start_date

FROM vwp v
LEFT JOIN vwp_cond c
  ON c.Permit_Id = v."Permit Id"

WHERE v.Classification = "Active"
  AND v."Activity Type" = "Water Withdrawal"

GROUP BY v."CEDS Facility Id" -- Remove facilities with multiple permits. Only need permited or not field
')

## Due to multiple versions of the permits and no classification field (for some reason),
#### you need to take the highest ID version of the permit, as that is the most recent one created
#### Migration was ordered by adminid (asc), so based on assumption msot recent permit = active = highest id
gwpdf <- sqldf('
-- Get the highest ID for each CEDS permit number (sorts through versions)
WITH mostrecent AS (
  SELECT GPP_PERMIT_NUMBER, MAX(GPP_DATE_END) AS END_DATE
  FROM gwp
  GROUP BY GPP_PERMIT_NUMBER
)

SELECT g.GPP_ID AS Permit_Id, g.GPP_VAHYDRO_PERMIT_NUMBER AS Permit_Number, mp.Facility_ID AS Fac_Id,
g.GPPU_DESCRIPTION AS Use_Type, g.GPP_OP_COND_YEARLY_LIMIT AS Annual_Limit, g.GPP_OP_COND_MONTHLY_LIMIT AS Monthly_Limit,
"Groundwater" AS Permit_Type, GPP_DATE_START AS Start_date

FROM gwp g
INNER JOIN mostrecent mr
  ON g.GPP_PERMIT_NUMBER = mr.GPP_PERMIT_NUMBER
  AND g.GPP_DATE_END = mr.END_DATE
INNER JOIN mp
  ON mp.Permit_ID = g.GPP_ID
  
--WHERE Classification = "Active"  -- This would be ideal but this field does not exist
  
GROUP BY mp.Facility_ID -- Remove facilities with multiple permits. Only need permited or not field
')

ceds_permits <- rbind(vwpdf,gwpdf)

ceds_permits$Start_date <- as.Date(ceds_permits$Start_date)

## CORRECTIONS : DUE TO FLAWS IN CEDS PERMITTING DATASET #####
## REMOVE THIS SECTION IN 2025 WHEN THE DATASET IS CORRECTED

## Adding in permit 16-0946 (not in ceds)
fluv <- data.frame(Permit_Id = NA, Permit_Number = '16-0946', Fac_Id = 200000069020,
                   Use_Type = 'public water supply',Annual_Limit = NA,Monthly_Limit = NA,Permit_Type = 'Surface Water',
                   Start_date = as.Date('2018-09-01'))

ceds_permits <- rbind(ceds_permits,fluv)

## Removing History GWP permits since there is no classification field
## 7 of these are drawn from vw2
RemovePermits <- c("GW0060001","AGTRO19","GW0009000","EV0041600","GW0032900","GW0032200","GW00126EU")

ceds_permits <- ceds_permits[!(ceds_permits$Permit_Number %in% RemovePermits),]

## END CORRECTIONS #######



## Writing the data 
write.csv(ceds_permits, paste0(onedrive_location,"/OWS/foundation_datasets/awrr/",eyear+1,"/Ceds_permits.csv"), row.names = F)



## Joining permits to mp_all ###########################
ows_permit_list <- read.csv(file = paste0(onedrive_location,"\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\ows_permit_list.csv")) 

#mp_all_mgy generated from AWRR_data.R is all MPs without power, without Dalecarlia, source type and use type names mostly already corrected
mp_all_mgy <- read.csv(file = paste0(onedrive_location,"\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_all_mgy_",eyear-4,"-",eyear,".csv"))

## Joins ceds permit data to foundation. Groups by facility and source type. One faiclity can have 2 rows
ceds_join <- sqldf(paste0('
SELECT a.Facility_hydroid, a.fac_CEDSid, a.Facility, a.Source_Type,a.Use_type,a.Locality,
c.Permit_Number, SUM(a.X',eyear,') AS MGY, SUM(a.X',eyear,') / 365 AS MGD,
CASE
  WHEN c.Permit_Number IS NULL THEN "Non-permitted"
  WHEN c.Permit_Number IS NOT NULL THEN "Permitted"
END AS Permit_Status

FROM mp_all_mgy a
LEFT JOIN ceds_permits c
  ON c.Fac_Id = a.fac_CEDSid
  AND c.Permit_Type = a.Source_Type

WHERE a.X2023 > 0
  AND a.Use_Type NOT LIKE "%power"

GROUP BY a.Facility_hydroid,  a.fac_CEDSid, Source_Type -- Groups by both facility IDs, unnecessary after 2023
'))

## QC Code #######
## Commented out, but can uncomment when running for the first time in a year
# ## Used to QC CEDS permit list
# awrr_join <- sqldf('
# SELECT a.Facility_hydroid, a.fac_CEDSid, a.Source_Type,a.Use_type,a.Locality,
# v."Permit.ID" AS Permit_Number, SUM(a.X2023) AS X2023
# 
# FROM ows_permit_list v
# INNER JOIN mp_all_mgy a
#   ON a.Facility_hydroid = v."VA.Hydro.Facility.ID"
#   AND (   (a.Source_Type = "Groundwater"   AND v."Permit.Program" LIKE "%GWP%")
#        OR (a.Source_Type = "Surface Water" AND v."Permit.Program" LIKE "%VWP%")  )
# 
# WHERE a.X2023 > 0
#   AND a.Use_Type NOT LIKE "%power"
# 
# GROUP BY a.Facility_hydroid, a.Source_Type
# ')

# Determining if any permits are missing from CEDS or permits that werent in VA Hyro
### Not inherently bad, have to check each one
# ex  <- (unique(ceds_join$Permit_Number[!(ceds_join$Permit_Number %in% awrr_join$Permit_Number)]))
# ex2 <- (unique(awrr_join$Permit_Number[!(awrr_join$Permit_Number %in% ceds_join$Permit_Number)]))
# 
# facex <- unique(awrr_join$Facility_hydroid[!(awrr_join$Facility_hydroid %in% ceds_join$Facility_hydroid)])
# 
# vw <- ows_permit_list[ows_permit_list$VA.Hydro.Facility.ID %in% facex,]
# View(vw)
# 
# facex2 <- unique(ceds_join$Facility_hydroid[!(ceds_join$Facility_hydroid %in% awrr_join$Facility_hydroid)])
# 
# vw2 <- ceds_join[ceds_join$Facility_hydroid %in% facex2,]
# View(vw2)


## Permits for facility hydroid c(450455,72173) not in CEDS as of 05-30-2024

## Building the Table #####

### Permitted sums ####
## The apply function takes the sum of mgy for each use type
## Permit_numbr is NA for facility/source type with no active permit

####Groundwater  ####
gw_uses <- data.frame(Use_Type = unique(ceds_join$Use_Type))

## Only takes the groundwater facilities
gw_join <- ceds_join[ceds_join$Source_Type == 'Groundwater',]

## Sums the use for GW by use type IF there is no permit number
gw_uses$Unpermitted_total <- 
  sapply(gw_uses$Use_Type, 
    function(x) {
      sum(gw_join$MGD[is.na(gw_join$Permit_Number) 
                      & gw_join$Use_Type == x])
    }) 

## Opposite, sums use by type when there is a permit number
gw_uses$Permitted_total <- 
  sapply(gw_uses$Use_Type, 
         function(x) {
           sum(gw_join$MGD[!is.na(gw_join$Permit_Number) 
                           & gw_join$Use_Type == x])
         })

## Getting the totla GW uses (make sure this matches Table 1)
gwTotal <- sum(gw_uses$Unpermitted_total+gw_uses$Permitted_total)

## Ordering the dataframe alphabetically 
gw_uses <- gw_uses[order(gw_uses$Use_Type),]

## Getting the percent of the total
gw_uses$Unpermitted_Pct <- gw_uses$Unpermitted_total/gwTotal * 100
gw_uses$Permitted_Pct <- gw_uses$Permitted_total/gwTotal * 100

## Creating a row of totals
gw_total_row <- data.frame(Use_Type = 'Total Groundwater',
                           Unpermitted_total = sum(gw_uses$Unpermitted_total),
                           Permitted_total = sum(gw_uses$Permitted_total),
                           Unpermitted_Pct = sum(gw_uses$Unpermitted_Pct),
                           Permitted_Pct   = sum(gw_uses$Permitted_Pct)  )
####Surface Water  ####
## This is the same section as above, but for surface water
sw_uses <- data.frame(Use_Type = unique(ceds_join$Use_Type))

sw_join <- ceds_join[ceds_join$Source_Type == 'Surface Water',]

sw_uses$Unpermitted_total <- 
  sapply(sw_uses$Use_Type, 
         function(x) {
           sum(sw_join$MGD[is.na(sw_join$Permit_Number) 
                           & sw_join$Use_Type == x])
         }) 

sw_uses$Permitted_total <- 
  sapply(sw_uses$Use_Type, 
         function(x) {
           sum(sw_join$MGD[!is.na(sw_join$Permit_Number) 
                           & sw_join$Use_Type == x])
         })

swTotal <- sum(sw_uses$Unpermitted_total+sw_uses$Permitted_total)

## Getting the percent of the total
sw_uses$Unpermitted_Pct <- sw_uses$Unpermitted_total/swTotal * 100
sw_uses$Permitted_Pct <- sw_uses$Permitted_total/swTotal * 100

sw_uses <- sw_uses[order(sw_uses$Use_Type),]

sw_total_row <- data.frame(Use_Type = 'Total Surface Water',
                           Unpermitted_total = sum(sw_uses$Unpermitted_total),
                           Permitted_total = sum(sw_uses$Permitted_total),
                           Unpermitted_Pct = sum(sw_uses$Unpermitted_Pct),
                           Permitted_Pct   = sum(sw_uses$Permitted_Pct)  )

## Tieing it all together into a single table

table3 <- rbind(gw_uses,gw_total_row,sw_uses,sw_total_row)

## Formatting the table for the kable call

table3$Use_Type <- str_to_title(table3$Use_Type)

## Resets row numbers
rownames(table3) <- NULL

## Rounds all values to 2 decimals (roudns all columns except Use_type)
table3[,2:ncol(table3)] <- sapply(table3[,2:ncol(table3)],function(x) {round(x,2)})

## Building .tex file ###########
table3w_latex <- kable(table3,'latex', booktabs = T, align =  c('l','l','l','l','l'),
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
  add_header_above(c(" ", "Annual Withdrawal Amount" = 2, '% of Total' = 2), bold=TRUE) %>%
  row_spec(0, bold = T) %>%
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
#table3w_tex
table3w_tex %>%
  cat(., file = paste(onedrive_location,"\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\overleaf\\summary_table3.tex",sep = ''))
