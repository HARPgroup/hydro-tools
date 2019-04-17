library(httr)

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
data <-read.csv("C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/hydro-tools/OWS/AnnualReporting/AquaVA2018.csv",sep=",",header=TRUE)
reporting_year <- "2018"

#---Summary variables
run_started <- Sys.time()
num_recs <- length(data[,1])

#i <- 1

#---Begin MP Feature Loop
for (i in 1:num_recs){

  featureid <- paste(data[i,]$deq.mp.hydroid)
  #IF STATEMENT TO ONLY IMPORT DATA FOR EXISTING VAHYDRO WELLS
  if (featureid == "NA") {
    print (paste("MP Does Not Exist In VAHydro OR Reports Quarterly - Skipping Well ",i," of ",num_recs,sep=""))
    next 
  }
  
  print (paste("Processing MP Feature ",i," of ",num_recs,sep=""))
  
  JAN <- paste(data[i,]$jan_mgd)
  FEB <- paste(data[i,]$feb_mgd)
  MAR <- paste(data[i,]$mar_mgd)
  APR <- paste(data[i,]$apr_mgd)
  MAY <- paste(data[i,]$may_mgd)
  JUN <- paste(data[i,]$jun_mgd)
  JUL <- paste(data[i,]$jul_mgd)
  AUG <- paste(data[i,]$aug_mgd)
  SEP <- paste(data[i,]$sep_mgd)
  OCT <- paste(data[i,]$oct_mgd)
  NOV <- paste(data[i,]$nov_mgd)
  DEC <- paste(data[i,]$dec_mgd)
  
  timeseries_values <- c(JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC)
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

    varid <- 1021 #variable id for wd_mgm

    #tsvalue <-  as.numeric(as.character(timeseries_values$timeseries_values[j]))
    tsvalue <-  as.character(timeseries_values$timeseries_values[j])

  
#-------------------------------------
#-------------------------------------
#FORMAT TIMESERIES DATA
print (paste("---Formatting Timeseries ",j," of ",num_timeseries_values,sep=""))
print (paste("----- ",dates[j]," -> ",as.numeric(as.character(timeseries_values$timeseries_values[j])),sep=""))


#convert date to UNIX timestamp
month <- as.character(timeseries_values$dates[j])
#tstime <- as.numeric(as.POSIXct(month,origin = "1970-01-01", tz = "GMT"))
tstime <- as.numeric(as.POSIXct(month,origin = "1970-01-01"))


################################################

inputs = list(
  featureid = featureid,
  varid = varid,
  entity_type = 'dh_feature',
  tsvalue = tsvalue,
  tstime = tstime
)

ts <- postTimeseries(inputs, site)
################################################




inputs <- list (
  featureid = featureid
)

Feature <- getFeature(inputs, token, site);






pbody = list(
  featureid = featureid,
  varid = varid,
  entity_type = 'dh_feature',
  tsvalue = tsvalue,
  tstime = tstime
)
#-------------------------------------

print (paste("---Retrieving Timeseries ",month," from VAHydro",sep=""))
get_ts <- GET(paste(site,"/dh_timeseries.json",sep=""), 
            add_headers(HTTP_X_CSRF_TOKEN = token),
            query = list(
              featureid = featureid,
              varid = varid,
              entity_type = 'dh_feature',
              tstime = tstime),
            encode = "json"
);
get_ts <- content(get_ts)


if (length(get_ts$list)) {
  tid <- get_ts$list[[1]]$tid
  print ("-----Timeseries exists - PUT");
  print (paste("-----TID = ",tid,sep=""));
  put_ts <- PUT(paste(site,"/dh_timeseries/",tid,sep=""), 
           add_headers(HTTP_X_CSRF_TOKEN = token),
           content_type("numeric"),
           body = list(
             featureid = featureid,
             varid = varid,
             entity_type = 'dh_feature',
             tsvalue = tsvalue
           ), 
           encode = "json");
  put_ts <- content(put_ts)

} else {
  print ("-----Timeseries does not exist - POST");
  post_ts <- POST(paste(site,"/dh_timeseries/",sep=""), 
          add_headers(HTTP_X_CSRF_TOKEN = token),
          body = pbody,
          encode = "json")

  post_ts <- content(post_ts)

}

} #---END MP Timeseries Loop
} #---END MP Feature For Loop