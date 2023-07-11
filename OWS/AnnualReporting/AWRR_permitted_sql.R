# This script generates the Permitted & Unpermitted Withdrawal Table in the Annual Report
# It is an update to avoid counting unpermitted surface water intakes from facilities with GWPs as permitted SW withdrawals 
library('dplyr')
library('httr')
library('sqldf')
library('stringr')
library("tidyr")
library("kableExtra")

#Set current year
eyear <- 2022
eyearX <- paste0("X",eyear)
Xyears <- array()
five <- 5:1
for (y in five) { Xyears[y] = paste0("X",(eyear+1)-five[y]) }

#load foundation data
#permit only data from https://deq1.bse.vt.edu/d.dh/ows-permit-list

ows_permit_list <- read.csv(file = paste0("U:\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\ows_permit_list.csv")) 
#mp_all_mgy generated from AWRR_data.R is all MPs without power, without Dalecarlia, source type and use type names mostly already corrected
mp_all_mgy <- read.csv(file = paste0("U:\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_all_mgy_",eyear-4,"-",eyear,".csv"))


#Process mp_all ##################################################

#remove duplicates (keeps one row)
data_all <- sqldf(paste('SELECT * FROM mp_all_mgy
               GROUP BY "MP_hydroid", "Hydrocode", "Source.Type", "MP.Name", "Facility_hydroid", "Facility", "Use.Type","FIPS.Code", "Locality", "OWS.Planner"'))

# retaining this line in case it's still needed
if (length(which(data_all$Use.Type=='facility')) > 0) {
  data_all <- data_all[-which(data_all$Use.Type=='facility'),]
} 

#rename columns and remove prior year withdrawal values
mp_all <- sqldf(paste('SELECT MP_hydroid AS "MP_hydroid",
    Hydrocode AS "Hydrocode",
    "Source.Type" AS "Source_Type",
    "MP.Name" AS "MP_Name",
    Facility_hydroid AS "Facility_hydroid",
    Facility AS "Facility",
    "Use.Type" AS "Use_Type",
    Latitude AS "lat",
    Longitude AS "lon",
    "FIPS.Code" AS "FIPS",
    Locality AS "locality",
    ',eyearX,' AS mgy,
    (',eyearX,')/365 AS mgd
  FROM data_all',sep=''))

#make use type values lowercase
mp_all$Use_Type <- str_to_lower(mp_all$Use_Type)

mp_all$Use_Type[mp_all$Use_Type == 'municipal'] <- 'public water supply'



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

write.csv(join_all, paste("U:\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_permitted_",eyear,".csv",sep = ""), row.names = F)

#read static data in
join_all <- read.csv(file = paste("U:\\OWS\\foundation_datasets\\awrr\\",eyear+1,"\\mp_permitted_",eyear,".csv",sep = ""))

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

table3_gw <- sqldf('SELECT Source_Type, Use_Type, CASE 
                    WHEN has_GWP = 1 
                    THEN "Permitted"
                    WHEN has_GWP IS NULL 
                    THEN "Unpermitted"
                    END AS "Withdrawal Type",
                    mgd AS "Withdrawal Amount",
                    round((mgd / 
                    (SELECT sum(a.mgd)
                    FROM gw_src_use a
                    WHERE a.Source_Type = "Groundwater")) * 100,2)
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
  cat(., file = paste("U:\\OWS\\Report Development\\Annual Water Resources Report\\October ",eyear+1," Report\\overleaf\\summary_table3.tex",sep = ''))
