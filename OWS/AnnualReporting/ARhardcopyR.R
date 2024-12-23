##########################################################################################################################################
### This RMD generates hardcopy annual water withdrawal reporting forms ###
### You need to change  the eyear to the current reporting year for this to work ###
### You need to set up the reporting folder in sharepoint to receive these if not already set up ### 
### Also, the ARhardcopy RMD file will be run in batch from this script to generate the reporting forms themselves,
### because it relies on loading the base datasets from CEDS through this script first to save batch running time ###
### Decisions that can be revisited, and improvements to this script for 2025 reporting, are noted at the bottom in a comments section, 
### including that there are still hardcoded 2024 year references in the RMD we didn't have time to genericize  ###
##########################################################################################################################################

### TOP OF SCRIPT
options(scipen=999)
library(sqldf)
library(flextable) #To make reporting table
library(odbc) #To connect to CEDS ODBC
basepath='/var/www/R'
source('/var/www/R/config.R')

#define your reporting range each year
eyear <- 2024
myear <- eyear-1
syear <- eyear-2

#define where you want the hardcopy reports to be stored
export_path <- paste0(onedrive_location,"\\OWS\\Annual Water Withdrawal Reporting\\Reporting Documents by Year\\",eyear,"\\Hardcopy\\Batch\\")

## INITILIZE ##################################################################################

## Connect to CEDS Production 
conn_DBI <- dbConnect(odbc::odbc(),
                      .connection_string = "driver={SQL Server};
                      server={DEQ-SQLODS-PROD,50000};
                      database={ODS};
                      trusted_connection=yes")

#load CEDS query function
getquery <- function(tableView, schema = "water") {
  col_info <- odbc::odbcConnectionColumns(conn_DBI, tableView,schema_name=schema)
  long_cols <- col_info$name[col_info$column_size >= 9000]
  all_other_cols <- col_info$name[col_info$column_size < 9000]
  colOrderForQuery <- c(all_other_cols,long_cols)
  colOrderForQuery <- paste0(colOrderForQuery,collapse='","')
  sql <- paste0('SELECT "',
                colOrderForQuery,
                '" FROM ',
                paste0(schema,'."',tableView,'"'))
  result <- dbGetQuery(conn_DBI,sql)
  return(result)
}

#define table generation function
fn_tablegen_AR <- function(tablecols){ 
  #set table defaults of theme and font and make sure theres no blanks from NAs, can be edited if desired
  set_flextable_defaults(
    font.size = 8, background.color = "white", 
    na_str = "NA", nan_str = "NA", #padding = 1, 
    table.layout = 'fixed', tabcolsep = 1.5,
    fonts_ignore = T, big.mark = "", decimal.mark = ".", digits = 4 ) #fonts_ignore applies when knitting as pdf 

  ft <- flextable(tablecols)
  
  ft <- theme_vanilla(ft)
  ft <- autofit(ft)
  ft <- width(ft, j=1, width = 1.5)
  ft <- width(ft, width = 1)
  ft <- align(ft, align = 'center', part = "all") #alignment: text alignment in flextable, either 'left', 'center', 'right', or 'justify'
  ft <- add_header_lines(ft, values= 'Water Withdrawal Amount or Bulk Transfers in Million Gallons (MG)') #add title 
  ft <- fontsize(ft, i=1, size=14, part = "header") #inc size of title
  return(ft)
}

##########################################################################################################
#This is the hardcopy part

#2024 filebackup in case CEDS connection is down
#write.csv(MPdf, paste0(onedrive_location,"\\OWS\\Annual Water Withdrawal Reporting\\Reporting Documents by Year\\2024\\Hardcopy\\TempBackup\\MPdf.csv"), row.names=FALSE   )

#Pull Registration, Organization, and MP Location and Withdrawal data from CEDS 
REGdf <- getquery('Water_Withdrawal_Reg_Vw') # hardcopy vs electronic reporters, with fac id, and use type
ORGdf <- getquery('CEDS Facility Owners', schema ="CEDS") # owner name and address, by fac
LATdf <- getquery('Measuring_Point_GIS_Vw') # lat/lon by MP
WDdf <- getquery('Water_Use_By_Month_Vw') # withdrawal data by month in gallons, use withdrawal date
MPIDdf <- getquery('Measuring_Point_Vw') # remaining measuring point details, including PWSID, but this has duplicates due to inactive/history permits

#Also load the Planner Areas by County
PlannerAreas <- read.csv(paste0(onedrive_location,"\\OWS\\GIS\\WSP\\PlannerCoverageTable_12102024.csv")) 

#isolate the Hardcopy reporters, keeping "Out of Service/Temporarily Inactive", "Active", "Proposed" facilities that may have reporting
HRep <- sqldf('select * from REGdf 
                     WHERE Reporting_Method == "Hardcopy"
                     AND Facility_Status != "Abandoned" 
                     AND Facility_Status != "Out of Service/Temporarily Inactive"
                     GROUP BY CEDS_Facility_ID') 

#Join Hardcopy reporting facilities to organization contact information,
ORGFACdf <- sqldf('select * from HRep as a
              LEFT OUTER JOIN (select * from ORGdf GROUP BY "CEDS Facility ID") as b
              ON a.CEDS_Facility_ID == b."CEDS Facility ID"
              WHERE "Owner ID" IS NOT NULL
              ORDER BY "Owner NAme"')

#Isolate the portion of the organization and facility information that will be used for mailing letters, in a format that Word mail merge can read, (note, Owner ID is the vahydro adminid equivalent)
MAILMERGEdf <- sqldf('select "Owner Id" as Owner_ID, "Owner Name" as Name, "Owner Address 1" as "Address 1","Owner Address 2" as "Address 2","Owner City" as City, "Owner State" as State, "Owner Zip Code" as "Zip Code"
                  FROM ORGFACdf
                  GROUP BY "Owner Id"') 
write.csv(MAILMERGEdf, paste0(onedrive_location,"\\OWS\\Annual Water Withdrawal Reporting\\Reporting Documents by Year\\2024\\Hardcopy\\OrgContacts_forMailMerge.csv"), row.names=FALSE   )

#Select Hardcopy reporters to measuring point and facility details, that will be used to fill in the form
MPdf <- sqldf('select * from MPIDdf where CEDS_Facility_ID in (select CEDS_Facility_ID from HRep) GROUP BY Measuring_Point_Id')
MPdf <- sqldf('select * from MPdf as a
              LEFT OUTER JOIN (select Measuring_Point_Id as Measuring_Point_Id2, Latitude, Longitude from LATdf) as b
              ON a.Measuring_Point_Id == b.Measuring_Point_Id2')
MPdf<-sqldf('select * from MPdf WHERE Permit_Number LIKE "%WWR%"') #remove groundwater GWI##### permitted facilities, keep only annual reporting WWR#### facilities


#Now generate a reporting form for each organization
OrgBatch <-  sqldf('select Owner_Id FROM MAILMERGEdf')
OrgBatch <- as.array(OrgBatch[,1])

for (x in 1:nrow(OrgBatch)){
#for (x in 24:nrow(OrgBatch)){
  #orgid <- OrgBatch[x,1]
  orgid <- OrgBatch[x]
  OrgName <- as.character(sqldf(paste0('SELECT "Owner Name" FROM MAILMERGEdf WHERE Owner_Id ==',orgid)))
  print(paste0("Rendering ",orgid," ",OrgName))
      rmarkdown::render(paste0(hydro_tools,"\\OWS\\AnnualReporting\\ARhardcopyRMD.Rmd"),
                        output_file =  paste0(export_path,"Org_", orgid,"_",OrgName),
                        params = list(
                          orgid = orgid
                        ))
}

##### QA - Compare VAHydro List to Ceds Method  #############################################
OrgHydro <- read.csv(paste0(onedrive_location,"\\OWS\\Annual Water Withdrawal Reporting\\Reporting Documents by Year\\",eyear,"\\Hardcopy\\vahydro_annual_reporting_envelope_labels_2024-12-15.csv")) #this is the hardcopy eblast vahydro export
OrgHydro <- sqldf('select * from OrgHydro group by Organization_adminid')
OrgCeds <- sqldf('select "OWner ID", Owner_Name from ORGFACdf group by "Owner ID"')
temp <- sqldf('select * from OrgHydro 
              WHERE Organization NOT IN (select Owner_Name from OrgCeds)') #The 6 Orgs we are missing with this method or the transition
write.csv(temp, paste0(export_path,"Missing_from_Ceds.csv"), row.names=FALSE   )
temp2 <- sqldf('select * from OrgCeds
              WHERE Owner_Name NOT IN (select Organization from OrgHydro)') #The 27 Orgs we have gained with this method or the transition
write.csv(temp2, paste0(export_path,"Extra_in_Ceds.csv"), row.names=FALSE   )


#### Notes ##################################################################

### handy IDs for testing ####
#sourceid <- "871000003712"
#orgid <- "100000055391" #pass in orgid, first trial of last 978
#orgid<-"100000057628" #ArlenBeery, has data
#orgid<-"100000055391" #Appalachian Power Company, multiple MPs but blank
#orgid<-"100000020498" #Thomas M. Schaubach, two MPs with some data

###### QA Questions Remaining ###### 


##### Later improvements ###### 

#remove CASE WHEN 2022, and use syear. Also there's years in the first and second paragraphs text itself, and in the RMD header.
#Include inactive facilities within the organizations with other active facilities


##### Decisions ######

#Decided to remove groundwater permitted MPs, not whole Orgs with permits, because an Org may have permitted and unpermitted facilities, this way we only include their reporting facilities, but it is theoretically possible we get a printout of an organization with no MP pages if one of its facilities was incorrectly coded to be hardcopy in CEDS for some reason when it is actually a permitted facility only.
#Decided to remove the NA organizations, of which there was only one active facility that we didn't have in VAHydro's list to report anyways (200000900760 BENNS CHURCH SERVICE AREA)
#Decided to remove the 900 inactive facilities
#Decided not to pass in the needed dataframes (ORGFACdf, MPdf WDdf, PlannerAreas), chose this becase loading CEDS tables for every loop will make the batch take too long, and because it does render after clearing environment as long as you run from this R script
#Using group by, Remove duplicate facilities in the ORGdf, which is the CEDS Facility Owners table, because fac can have multiple owners or outdated ones from old programs
#Organizations with facilities in multiple counties will have one selected at random for the planner address
#Use Officedown instead of officer