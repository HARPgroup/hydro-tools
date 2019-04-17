library(httr) #required for REST
library(lubridate) #required for leap.year

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
#data <-read.csv("C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/hydro-tools/OWS/AnnualReporting/AquaVA2018.csv",sep=",",header=TRUE)
data <-read.csv("U:/OWS/VWWR_VWUDS/Annual Reporting/2018/AquaVA2018.csv",sep=",",header=TRUE)

reporting_year <- 2018

#---Summary variables
run_started <- Sys.time()
print (paste("Run Started: ",run_started, sep=""))
num_recs <- length(data[,1])

#i <- 1
#i <- 241

#---Begin MP Feature Loop
for (i in 1:num_recs){

  featureid <- paste(data[i,]$deq.mp.hydroid)
  #IF STATEMENT TO ONLY IMPORT DATA FOR EXISTING VAHYDRO WELLS
  if (featureid == "NA") {
    print (paste("MP Does Not Exist In VAHydro OR Reports Quarterly - Skipping Well ",i," of ",num_recs,sep=""))
    next 
  }
  
  print (paste("Processing MP Feature ",i," of ",num_recs,sep=""))
  
  # JAN_mgd <- paste(data[i,]$jan_mgd)
  # FEB_mgd <- paste(data[i,]$feb_mgd)
  # MAR_mgd <- paste(data[i,]$mar_mgd)
  # APR_mgd <- paste(data[i,]$apr_mgd)
  # MAY_mgd <- paste(data[i,]$may_mgd)
  # JUN_mgd <- paste(data[i,]$jun_mgd)
  # JUL_mgd <- paste(data[i,]$jul_mgd)
  # AUG_mgd <- paste(data[i,]$aug_mgd)
  # SEP_mgd <- paste(data[i,]$sep_mgd)
  # OCT_mgd <- paste(data[i,]$oct_mgd)
  # NOV_mgd <- paste(data[i,]$nov_mgd)
  # DEC_mgd <- paste(data[i,]$dec_mgd)
  
  #Monthly data MGD
  JAN_mgd <- data[i,]$jan_mgd
  FEB_mgd <- data[i,]$feb_mgd
  MAR_mgd <- data[i,]$mar_mgd
  APR_mgd <- data[i,]$apr_mgd
  MAY_mgd <- data[i,]$may_mgd
  JUN_mgd <- data[i,]$jun_mgd
  JUL_mgd <- data[i,]$jul_mgd
  AUG_mgd <- data[i,]$aug_mgd
  SEP_mgd <- data[i,]$sep_mgd
  OCT_mgd <- data[i,]$oct_mgd
  NOV_mgd <- data[i,]$nov_mgd
  DEC_mgd <- data[i,]$dec_mgd
  
  #Monthly data MGM
  JAN_mgm <- JAN_mgd * 31
    if (leap_year(reporting_year) == TRUE) {
      FEB_mgm <- FEB_mgd * 29
    } else {
      FEB_mgm <- FEB_mgd * 28
    }
  MAR_mgm <- MAR_mgd * 31
  APR_mgm <- APR_mgd * 30
  MAY_mgm <- MAY_mgd * 31
  JUN_mgm <- JUN_mgd * 30
  JUL_mgm <- JUL_mgd * 31
  AUG_mgm <- AUG_mgd * 31
  SEP_mgm <- SEP_mgd * 30
  OCT_mgm <- OCT_mgd * 31
  NOV_mgm <- NOV_mgd * 30
  DEC_mgm <- DEC_mgd * 31
  
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
  dates <- c(paste(2018,"-01-01",sep=""),
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

    ts <- postTimeseries(inputs, site)
    get_ts <- getTimeseries(inputs, site)
    ################################################
  } #---END MP Timeseries Loop
  
} #---END MP Feature For Loop

run_ended <- Sys.time()
print (paste("Run Ended: ",run_ended, sep=""))
print (run_ended-run_started)

