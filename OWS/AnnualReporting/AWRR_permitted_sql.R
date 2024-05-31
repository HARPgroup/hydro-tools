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
    colOrderForQuery <- paste0(colOrderForQuery,collapse=",")
    
    #Develop a query
    sql <- paste0("SELECT ",
                  colOrderForQuery,
                  " FROM ",
                  paste0(schema,".",tableView))
    
    result <- dbGetQuery(conn_DBI,sql)
    return(result)
}

## These two tables have the weird column issue and need the workaround
gwp <- getquery("GWP_Permits_Vw")

vwp_cond <- getquery("VWP_Withdrawal_Condition_Vw")

sql_string <- 'SELECT * FROM water.[VWP Permits]'
vwp <-  dbGetQuery(conn_DBI,sql_string)

sql_string <- 'SELECT * FROM water.Measuring_Point_Vw'
mp <-  dbGetQuery(conn_DBI,sql_string)


vwpdf <- sqldf('
SELECT 
c.Permit_Id, v."Permit Number" AS Permit_Number, v."CEDS Facility Id" AS Fac_Id,
v."Activity Type" AS Use_Type,c.Annual_Withdrawal_Limit AS Annual_Limit, c.Monthly_Withdrawal_Limit AS Monthly_Limit,
"Surface Water" AS Permit_Type

FROM vwp v
INNER JOIN vwp_cond c
  ON c.Permit_Id = v."Permit Id"

WHERE v.Classification = "Active"

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
"Groundwater" AS Permit_Type

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

## CORRECTIONS : DUE TO FLAWS IN CEDS PERMITTING DATASET #####
## REMOVE THIS SECTION IN 2025 WHEN THE DATASET IS CORRECTED

## Adding in permit 16-0946 (not in ceds)
fluv <- data.frame(Permit_Id = NA, Permit_Number = '16-0946', Fac_Id = 200000069020,
                   Use_Type = 'public water supply',Annual_Limit = NA,Monthly_Limit = NA,Permit_Type = 'Surface Water')

ceds_permits <- rbind(ceds_permits,fluv)

## Removing History GWP permits since there is no classification field
## 7 of these are drawn from vw2
RemovePermits <- c("GW0060001","AGTRO19","GW0009000","EV0041600","GW0032900","GW0032200","GW00126EU")

ceds_permits <- ceds_permits[!(ceds_permits$Permit_Number %in% RemovePermits),]

## END CORRECTIONS #######

## Joining permits to mp_all ######
ows_permit_list <- read.csv(file = paste0(onedrive_location,"\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\ows_permit_list.csv")) 

#mp_all_mgy generated from AWRR_data.R is all MPs without power, without Dalecarlia, source type and use type names mostly already corrected
mp_all_mgy <- read.csv(file = paste0(onedrive_location,"\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_all_mgy_",eyear-4,"-",eyear,".csv"))

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

GROUP BY a.Facility_hydroid,  a.fac_CEDSid, Source_Type
'))

## Used to QC CEDS permit list
awrr_join <- sqldf('
SELECT a.Facility_hydroid, a.fac_CEDSid, a.Source_Type,a.Use_type,a.Locality,
v."Permit.ID" AS Permit_Number, SUM(a.X2023) AS X2023

FROM ows_permit_list v
INNER JOIN mp_all_mgy a
  ON a.Facility_hydroid = v."VA.Hydro.Facility.ID"
  AND (   (a.Source_Type = "Groundwater"   AND v."Permit.Program" LIKE "%GWP%")
       OR (a.Source_Type = "Surface Water" AND v."Permit.Program" LIKE "%VWP%")  )

WHERE a.X2023 > 0
  AND a.Use_Type NOT LIKE "%power"

GROUP BY a.Facility_hydroid, a.Source_Type
')

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
gw_uses <- data.frame(Use_type = unique(ceds_join$Use_Type))

gw_join <- ceds_join[ceds_join$Source_Type == 'Groundwater',]

gw_uses$Unpermitted_total <- 
  sapply(gw_uses$Use_type, 
    function(x) {
      sum(gw_join$MGD[is.na(gw_join$Permit_Number) 
                      & gw_join$Use_Type == x])
    }) 

gw_uses$Permitted_total <- 
  sapply(gw_uses$Use_type, 
         function(x) {
           sum(gw_join$MGD[!is.na(gw_join$Permit_Number) 
                           & gw_join$Use_Type == x])
         })

## Getting the totla GW uses (make sure this matches Table 1)
gwTotal <- sum(gw_uses$Unpermitted_total+gw_uses$Permitted_total)

## Ordering the dataframe alphabetically 
gw_uses <- gw_uses[order(gw_uses$Use_type),]

## Getting the percent of the total
gw_uses$Unpermitted_Pct <- gw_uses$Unpermitted_total/gwTotal * 100
gw_uses$Permitted_Pct <- gw_uses$Permitted_total/gwTotal * 100

####Surface Water  ####
sw_uses <- data.frame(Use_type = unique(ceds_join$Use_Type))

sw_join <- ceds_join[ceds_join$Source_Type == 'Surface Water',]

sw_uses$Unpermitted_total <- 
  sapply(sw_uses$Use_type, 
         function(x) {
           sum(sw_join$MGD[is.na(sw_join$Permit_Number) 
                           & sw_join$Use_Type == x])
         }) 

sw_uses$Permitted_total <- 
  sapply(sw_uses$Use_type, 
         function(x) {
           sum(sw_join$MGD[!is.na(sw_join$Permit_Number) 
                           & sw_join$Use_Type == x])
         })

swTotal <- sum(sw_uses$Unpermitted_total+sw_uses$Permitted_total)

sw_uses <- sw_uses[order(sw_uses$Use_type),]

## Getting the percent of the total
sw_uses$Unpermitted_Pct <- sw_uses$Unpermitted_total/swTotal * 100
sw_uses$Permitted_Pct <- sw_uses$Permitted_total/swTotal * 100

#Process mp_all ##################################################

#remove duplicates (keeps one row)
data_all <- sqldf(paste('SELECT * FROM mp_all_mgy
               GROUP BY "MP_hydroid", "Source_Type", "MP_Name", "Facility_hydroid",fac_CEDSid, "Facility", "Use_Type","FIPS_Code", "Locality", "OWS_Planner"'))


#rename columns and remove prior year withdrawal values
mp_all <- sqldf(paste('
    SELECT MP_CEDSid,MP_hydroid,
      "Source_Type",
      "MP_Name" ,
      Facility_hydroid,
      fac_CEDSid,
      Facility ,
      "Use_Type",
      Latitude AS "lat",
      Longitude AS "lon",
      "FIPS_Code" AS "FIPS",
      Locality AS "locality",
      ',eyearX,' AS mgy,
      (',eyearX,')/365 AS mgd
    FROM data_all',sep=''))

#make use type values lowercase
mp_all$Use_Type <- str_to_lower(mp_all$Use_Type)

#Process ows_permit_list ##################################################

permit_all <- ows_permit_list

# remove duplicates (keeps one row)
permit_all <- sqldf('SELECT * FROM permit_all
               GROUP BY "Owner", "Permit", "Permit.ID","VA.Hydro.Facility.ID", "Facility","Status","Use.Type", "Locality", "Facility.Latitude","Facility.Longitude" ,"Permit.Start","Permit.Expiration","Permit.Program","Permit.Staff","GWP.Annual.Limit","GWP.Monthly.Limit","VWP.Annual.Limit","VWP.Monthly.Limit","VWP.Daily.Limit"')

# cleanup names
permit_all$Use.Type[permit_all$Use.Type == 'industrial'] <- 'manufacturing'
permit_all$Use.Type[permit_all$Use.Type == 'municipal'] <- 'public water supply'

colnames(permit_all)[colnames(permit_all)=="Permit.Program"] <- "Permit_Program"
colnames(permit_all)[colnames(permit_all)=="VA.Hydro.Facility.ID"] <- "Facility_HydroID"


## Mark permitted facilities with unpermitted and permitted status #####################################

#mark GWP and VWPs in the permitted data
permit_has <- sqldf('SELECT *, 
                CASE WHEN Permit_Program = "Virginia Groundwater Permit Program (GWPermit)"
                 OR Permit_Program = "Virginia Groundwater Permit Program (GWPermit), Virginia Department of Environmental Quality Water Use Database (VWUDS)"
                 THEN 1 ELSE 0
                 END AS has_GWP,
                CASE WHEN Permit_Program = "Virginia Water Protection Permit Program (VWP)"
                 OR Permit_Program = "Virginia Water Protection Permit Program (VWP), Virginia Department of Environmental Quality Water Use Database (VWUDS)"
                 THEN 1 ELSE 0
                 END AS has_VWP
               FROM permit_all')
#visual check
sqldf('select has_GWP, has_VWP, count(*) from permit_has group by has_GWP, has_VWP')

#filter for facilities that have a GW or SW permit
permit_gw <- sqldf('SELECT * FROM permit_has WHERE has_GWP = 1')
permit_sw <- sqldf('SELECT * FROM permit_has WHERE has_VWP = 1')

#Just count permits that are active, expired (which are current but past permit term), and under application
per_gw_act <- sqldf('SELECT * FROM permit_gw
                      WHERE Status = "active"
                      OR Status = "expired"
                      OR Status = "application"')
per_sw_act <- sqldf('SELECT * FROM permit_sw
                      WHERE Status = "active"
                      OR Status = "expired"
                      OR Status = "application"')
# NOTE Any filtering of permit row values must happen before this Group By otherwise values may not be selected when rows are collapsed
per_gw_fac <- sqldf('SELECT * FROM per_gw_act
                      GROUP BY Facility_HydroID')
per_sw_fac <- sqldf('SELECT * FROM per_sw_act
                      GROUP BY Facility_HydroID')

### application status ########################################
#Check this the first time, then skip if just rerunning a table
#This section checks that we are not duplicating facility hydroids when including the 'application' permit status 
temp_gw_act <- sqldf('SELECT * FROM permit_gw
                      WHERE Status = "active"
                      OR Status = "expired"')
temp_sw_act <- sqldf('SELECT * FROM permit_sw
                      WHERE Status = "active"
                      OR Status = "expired"')
temp_gw_fac <- sqldf('SELECT * FROM temp_gw_act
                      GROUP BY Facility_HydroID')
temp_sw_fac <- sqldf('SELECT * FROM temp_sw_act
                      GROUP BY Facility_HydroID')
apps_sw <- sqldf('SELECT a.*, b.Facility_HydroID as compare
                  FROM per_sw_fac AS a
                  LEFT OUTER JOIN temp_sw_fac AS b
                   ON a.Facility_hydroid = b.Facility_HydroID')
apps_gw <- sqldf('SELECT a.*, b.Facility_HydroID as compare
                  FROM per_gw_fac AS a
                  LEFT OUTER JOIN temp_gw_fac AS b
                   ON a.Facility_hydroid = b.Facility_HydroID')
#contains an 'application' permit status
#apps_sw_status <- sqldf('select * from apps_sw where status = "application" ') #this can still have duplicate hydroids so it's less helpful than the step below
#these are the facilities added by including the 'application' permit status
apps_sw_added <- sqldf('select * from apps_sw where compare is null')
apps_gw_added <- sqldf('select * from apps_gw where compare is null')
#If 0 obs, we are not duplicating faciilty hydroids
apps_sw_QA <- sqldf('select * from apps_sw group by Facility_hydroid HAVING count(*) >1') 
apps_gw_QA <- sqldf('select * from apps_gw group by Facility_hydroid HAVING count(*) >1')
#print to do manual check of application facility names that have different facility names in VAHydro
write.csv(apps_sw_added, "C:\\Users\\rnv55934\\Documents\\Docs\\AnnualReport\\2022\\apps_sw_added.csv")
write.csv(apps_gw_added, "C:\\Users\\rnv55934\\Documents\\Docs\\AnnualReport\\2022\\apps_gw_added.csv")
#remove clutter
rm(temp_gw_act,temp_sw_act,temp_gw_fac,temp_sw_fac,apps_sw,apps_gw,apps_sw_added,apps_gw_added, apps_sw_QA, apps_gw_QA)
##################################

##########################################################################
#Join all facilities with marked permitted facilities #####################

#join main data with marked permitted data using facility hydroid, keep gw and sw separate for separate sums
join_all <- sqldf('SELECT a.*, b.has_GWP, c.has_VWP
                  FROM mp_all AS a
                  LEFT OUTER JOIN per_gw_fac AS b
                   ON a.Facility_hydroid = b.Facility_HydroID
                  LEFT OUTER JOIN per_sw_fac AS c
                   ON a.Facility_hydroid = c.Facility_HydroID')
#check there is no duplication of MPs by the join, should result in 0 obs.
QAjoin <- sqldf('select *,count(*) from join_all group by MP_hydroid HAVING count(*) >1')

join_all$Use_Type <- str_to_title(join_all$Use_Type)

#------------------ testing when to group by facility
#now sum by gwp and vwp individually. Should work because count the GWP from pemrit_gw and the groundwater without gwp from main data as unpermitted

#sum MPs withdrawals from mp_all to create facility level table, because permit list only has facility hydroids to join on
# fac_all <- sqldf('SELECT Source_Type, Facility_hydroid, Facility, Use_Type, FIPS, sum(mgy) AS mgy, sum(mgd) as mgd
#                    FROM mp_all
#                    GROUP BY Facility_hydroid') # DONT DO THIS, LOSE USE TYPE

# # this shows why we need to group by facility on the permit list as well
# testp <- permit_all
# testp_active <- sqldf('SELECT * FROM testp
#                       WHERE Status = "active"
#                       OR Status = "expired"')
# testp_active <- sqldf('SELECT * FROM testp_active
#                       GROUP BY Facility_HydroID
#                       HAVING count(*) > 1')
# rm(testp,testp_active)
# #   just active results in 1 extra row, probably because there are two duplicate hydoids. One is lake kilby accounting for that extra row, and the other is a fossilpower that wont join bc mp_all doesnt have power
# #   just expired results in 0 extra rows 
# #   active or expired results in 12 extra rows, that's likely because the same hydroid can have both statuses 

#-----------

########## STATIC DATA #######################################################################################

#save the multi_yr_data to use for data reference - we can refer to that csv when asked questions about the data
# NOTE that csv output will show has_GWP for all MPs in a facility that has a single permitted GW MP, and has_VWP for all MPs in a facility that has a permitted SW MP, because we only had permit status by facility level. Make export name clear
colnames(join_all)[colnames(join_all)=="has_GWP"] <- "facility_contains_a_GWP"
colnames(join_all)[colnames(join_all)=="has_VWP"] <- "facility_contains_a_VWP"

write.csv(join_all, paste(onedrive_location,"\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_permitted_",eyear,".csv",sep = ""), row.names = F)

#read static data in
join_all <- read.csv(file = paste(onedrive_location,"\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_permitted_",eyear,".csv",sep = ""))

#and undo name change
colnames(join_all)[colnames(join_all)=="facility_contains_a_GWP"] <- "has_GWP"
colnames(join_all)[colnames(join_all)=="facility_contains_a_VWP"] <- "has_VWP"

## FORMAT Table 3: 20XX Permitted and Unpermitted (Excluded) By Use Type Withdrawals (MGD) ###############
#Removing Use Type agricultural

join_all <- sqldf('SELECT * FROM join_all WHERE Use_Type NOT IN ("Agricultural")')

#Sum groundwater

gw_src_use <- sqldf(
  'SELECT Source_type, Use_Type, has_GWP, ROUND(sum(mgd),2) AS mgd, count(*) 
  FROM join_all
  WHERE Source_type = "Groundwater"
  GROUP BY Source_type, Use_Type, has_GWP')

table3_gw <- sqldf('SELECT Source_Type, Use_Type, 
                    CASE 
                      WHEN has_GWP = 1 THEN "Permitted"
                      WHEN has_GWP IS NULL THEN "Unpermitted"
                    END AS "Withdrawal Type",
                    mgd AS "Withdrawal Amount",
                    round((mgd /(SELECT sum(a.mgd)
                                 FROM gw_src_use a
                                 WHERE a.Source_Type = "Groundwater")
                            ) * 100,2)
                    AS "pct_of_total"
                   FROM gw_src_use')

table3_gw_tot <- sqldf('SELECT "Total Groundwater" AS Source_Type, 
              "Total Groundwater" AS Use_Type,
              "" AS "Withdrawal Type",
              sum("Withdrawal Amount") AS "Withdrawal Amount",
              round(sum(pct_of_total),1) AS pct_of_total
      FROM table3_gw')

#Sum surface water

# #check that MP 60555 and 67045 are showing as has_VWP = NA even though has_GWP = 1, confirmed!
# QAsw <- sqldf( 'SELECT MP_hydroid, MP_Name, FAcility_hydroid, Facility, Source_type, Use_Type, mgy, has_VWP 
#   FROM join_all
#   WHERE Source_type = "Surface Water"')

sw_src_use <- sqldf(
  'SELECT Source_type, Use_Type, has_VWP, ROUND(sum(mgd),2) AS mgd, count(*) 
  FROM join_all
  WHERE Source_type = "Surface Water"
  GROUP BY Source_type, Use_Type, has_VWP')

table3_sw <- sqldf('SELECT Source_Type, Use_Type, CASE 
                    WHEN has_VWP = 1 
                    THEN "Permitted"
                    WHEN has_VWP IS NULL 
                    THEN "Unpermitted"
                    END AS "Withdrawal Type",
                    mgd AS "Withdrawal Amount",
                    round((mgd / 
                    (SELECT sum(a.mgd)
                    FROM sw_src_use a
                    WHERE a.Source_Type = "Surface Water")) * 100,2)
                    AS "pct_of_total"
                   FROM sw_src_use')

table3_sw_tot <- sqldf('SELECT "Total Surface Water" AS Source_Type, 
              "Total Surface Water" AS Use_Type,
              "" AS "Withdrawal Type",
              sum("Withdrawal Amount") AS "Withdrawal Amount",
              round(sum(pct_of_total),1) AS pct_of_total
      FROM table3_sw')

table3 <- rbind(table3_gw,table3_gw_tot,table3_sw,table3_sw_tot)

#remove clutter
rm(table3_gw,table3_gw_tot,table3_sw,table3_sw_tot)


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

table3_wide[is.na(table3_wide)] = 0 #GM add for mining and manufacturing
write.csv(table3_wide, paste("C:\\Users\\rnv55934\\Documents\\Docs\\AnnualReport\\2022\\table3b.csv"), row.names = F)

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
table3w_tex
table3w_tex %>%
  cat(., file = paste(onedrive_location,"\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\overleaf\\summary_table3.tex",sep = ''))
