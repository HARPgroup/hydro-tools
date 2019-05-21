library(httr) #required for REST
library(lubridate) #required for leap.year
library("readxl") #required for read_excel()
#----------------------------------------------
site <- "http://deq2.bse.vt.edu/d.alpha"
#----------------------------------------------

#Set Repo Location
basepath="C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\hydro-tools"

#Retrieve REST token
source(paste(basepath,"VAHydro-2.0/rest_functions.R", sep = "/")) #Load REST functions
source(paste(basepath,"auth.private", sep = "/"))#load rest username and password, contained in auth.private file
token <- rest_token(site, token, rest_uname, rest_pw)


#####################
#Load import file
#data <-read.csv("U:/OWS/VWWR_VWUDS/Annual Reporting/2018/AquaVA2018_mgm_VWUDS.csv",sep=",",header=TRUE) #AQUAVA 2018
#data <-read.csv("U:/OWS/VWWR_VWUDS/Annual Reporting/2017/AquaVA2017_mgm_VWUDS.csv",sep=",",header=TRUE) #AQUAVA 2017

#data <-read.csv("U:/OWS/VWWR_VWUDS/Annual Reporting/2018/AquaVA2018.csv",sep=",",header=TRUE) #SYDNOR
data <- read_excel("U:/OWS/VWWR_VWUDS/Annual Reporting/2018/Sydnor/Sydnor_VAHydro.xlsx", sheet = "import-ready")

names(data) <- as.character(data[1,])
data <- data[-1,]


reporting_year <- 2018

#---Summary variables
run_started <- Sys.time()
print (paste("Run Started: ",run_started, sep=""))
num_recs <- length(data$deq.mp.hydroid)

#i <- 1
#i <- 304

#---Begin MP Feature Loop
for (i in 1:num_recs){
#for (i in 304:num_recs){

  featureid <- paste(data[i,]$deq.mp.hydroid)
  #IF STATEMENT TO ONLY IMPORT DATA FOR EXISTING VAHYDRO WELLS
  if (featureid == "NA") {
    print (paste("MP Does Not Exist In VAHydro OR Reports Quarterly - Skipping Well ",i," of ",num_recs,sep=""))
    next 
  }
  
  print (paste("Processing MP Feature ",i," of ",num_recs,sep=""))
  
  #needed to convert gal/mo to mgm for Sydnor data
  JAN_mgm <- as.numeric(data[i,]$jan_gal)/1000000
  FEB_mgm <- as.numeric(data[i,]$feb_gal)/1000000
  MAR_mgm <- as.numeric(data[i,]$mar_gal)/1000000
  APR_mgm <- as.numeric(data[i,]$apr_gal)/1000000
  MAY_mgm <- as.numeric(data[i,]$may_gal)/1000000
  JUN_mgm <- as.numeric(data[i,]$jun_gal)/1000000
  JUL_mgm <- as.numeric(data[i,]$jul_gal)/1000000
  AUG_mgm <- as.numeric(data[i,]$aug_gal)/1000000
  SEP_mgm <- as.numeric(data[i,]$sep_gal)/1000000
  OCT_mgm <- as.numeric(data[i,]$oct_gal)/1000000
  NOV_mgm <- as.numeric(data[i,]$nov_gal)/1000000
  DEC_mgm <- as.numeric(data[i,]$dec_gal)/1000000

  
  # #Monthly data MGM
  # JAN_mgm <- data[i,]$jan_mgm
  # FEB_mgm <- data[i,]$feb_mgm
  # MAR_mgm <- data[i,]$mar_mgm
  # APR_mgm <- data[i,]$apr_mgm
  # MAY_mgm <- data[i,]$may_mgm
  # JUN_mgm <- data[i,]$jun_mgm
  # JUL_mgm <- data[i,]$jul_mgm
  # AUG_mgm <- data[i,]$aug_mgm
  # SEP_mgm <- data[i,]$sep_mgm
  # OCT_mgm <- data[i,]$oct_mgm
  # NOV_mgm <- data[i,]$nov_mgm
  # DEC_mgm <- data[i,]$dec_mgm

  
  timeseries_values <- c(JAN_mgm,
                         FEB_mgm,
                         MAR_mgm,
                         APR_mgm,
                         MAY_mgm,
                         JUN_mgm,
                         JUL_mgm,
                         AUG_mgm,
                         SEP_mgm,
                         OCT_mgm,
                         NOV_mgm,
                         DEC_mgm)
  dates <- c(paste(reporting_year,"-01-01",sep=""),
             paste(reporting_year,"-02-01",sep=""),
             paste(reporting_year,"-03-01",sep=""),
             paste(reporting_year,"-04-01",sep=""),
             paste(reporting_year,"-05-01",sep=""),
             paste(reporting_year,"-06-01",sep=""),
             paste(reporting_year,"-07-01",sep=""),
             paste(reporting_year,"-08-01",sep=""),
             paste(reporting_year,"-09-01",sep=""),
             paste(reporting_year,"-10-01",sep=""),
             paste(reporting_year,"-11-01",sep=""),
             paste(reporting_year,"-12-01",sep=""))

  timeseries_values <- data.frame(timeseries_values,dates)

 #j <- 1
  #---Begin MP Timeseries Loop
  num_timeseries_values <- length(timeseries_values$timeseries_values)
  for (j in 1:num_timeseries_values){  

    #variable id for wd_mgm
    varid <- 1021

    #tsvalue <-  as.numeric(as.character(timeseries_values$timeseries_values[j]))
    tsvalue <-  as.character(timeseries_values$timeseries_values[j])

    #Print messages
    print (paste("-- Formatting Timeseries ",j," of ",num_timeseries_values,sep=""))
    print (paste("----- ",dates[j]," -> ",as.numeric(as.character(timeseries_values$timeseries_values[j])),sep=""))

    #Convert date to UNIX timestamp
    month <- as.character(timeseries_values$dates[j])
    #tstime <- as.numeric(as.POSIXct(month,origin = "1970-01-01", tz = "GMT"))
    tstime <- as.numeric(as.POSIXct(month,origin = "1970-01-01"))

    #Perform REST timeseries import
    ################################################
    inputs = list(
      featureid = featureid,
      varid = varid,
      entity_type = 'dh_feature',
      tsvalue = tsvalue,
      tstime = tstime
    )

    #ts <- postTimeseries(inputs, site)
    ts <- postTimeseriesIFempty(inputs, site)
    #get_ts <- getTimeseries(inputs, site)
    ################################################
  } #---END MP Timeseries Loop
  
} #---END MP Feature For Loop

run_ended <- Sys.time()
print (paste("Run Ended: ",run_ended, sep=""))
print (run_ended-run_started)

