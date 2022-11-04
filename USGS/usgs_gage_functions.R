#Load required libraries 
library(dataRetrieval) #https://cran.r-project.org/web/packages/dataRetrieval/dataRetrieval.pdf
require(data.table)
require(zoo)
library(stringr)


streamgage_historic <- function(gageid){
  print(paste("Retrieving Historic Data for: ",gageid, sep='')); 
  
  #Retrieve Extra Gage Attribute Info - Such as gage station name
  ##gage_info <- readNWISsite(gageid)
  ##staname <- gage_info$station_nm
  ##print(paste("Station Name: ",staname, sep='')); 
  
  #Retrieve all historic gage streamflow data in cfs
  gage_data <- readNWISdv(gageid,'00060')
  gage_data <- renameNWISColumns(gage_data)
  
  # Skip gages that don't have a column for flow data from USGS
  if (ncol(gage_data) < 5) {
    print(paste("NWIS Responded NO DATA for ", gageid, sep=''))
    next
  } else {
    gage_data <- gage_data 
  }
  
} #Close Function

clean_historic <- function(historic){
  # ******************************************************************************************
  # Remove any rows with "Ice", "P Ice" or "P Eqp" values for Flow_cd
  # ******************************************************************************************
  if (length(which(historic[,5]== "Ice")) != 0 ){
    print("Removing Ice Flows")
    historic <- historic[-which(historic[,5]== "Ice"),]
  }
  
  if (length(which(historic[,5]== "P Ice")) != 0 ){
    print("Removing Ice Flows")
    historic <- historic[-which(historic[,5]== "P Ice"),]
  }
  
  if (length(which(historic[,5]== "P Eqp")) != 0 ){
    print("Removing P Eqp Flows")
    historic <- historic[-which(historic[,5]== "P Eqp"),]
  }
  
  if (length(which(historic[,4] < 0.0)) != 0 ){
    print("Removing Negative Flow Values")
    historic <- historic[-which(historic[,4] < 0.0),]
  }
  
  historic <- historic
} 

gage_pct_mo <- function(gageid,percentiles.df) {
  #gageid <- "01634000"
  
  print(paste("gageid: ", gageid, sep = ''))
  
  gage_info <- readNWISsite(gageid)
  staid <- gage_info$site_no
  staname <- gage_info$station_nm
  print(paste("Processing ", staname, sep = ""))
  
  
  #############################################################################
  # Retrieve Historic Gage Data From NWIS
  ############################################################################
  gage <- streamgage_historic(gageid)
  
  startdate <- gage[1, ]$Date
  enddate <- gage[length(gage$Date),]$Date
  
  print(paste("Gage Start Date: ", startdate, sep = ''))
  print(paste("Gage End Date: ", enddate, sep = ''))
  ############################################################################
  ############################################################################
  
  #Calculate 7-Day Average Streamflow for every day of the historic record
  rollmean_7day <- rollmeanr(gage$Flow, 7, fill = NA)
  gage <- cbind(gage, rollmean_7day)
  #tail(gage)
  
  months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  months_num <- c('-01-','-02-','-03-','-04-','-05-','-06-','-07-','-08-','-09-','-10-','-11-','-12-')
  months_all <- data.frame(months, months_num)
  
  percentiles.df <- data.frame(
    Month = character(),
    Percentile_0 = character(),
    Percentile_5 = character(),
    Percentile_10 = character(),
    Percentile_25 = character(),
    Percentile_50 = character(),
    Percentile_75 = character(),
    Percentile_90 = character(),
    Percentile_95 = character(),
    Percentile_100 = character(),
    stringsAsFactors = FALSE
  )
  
  #x <- 1
  for (x in 1:length(months)) {
    daily_month <- months[x]
    print(paste("Processing ", daily_month, sep = ""))
    
    #Determine Current month name and numeric value
    month_row <- which(months_all$months == daily_month)
    month_num <- months_all[month_row, ]
    month <- month_num$months
    month <- toString(month)
    month_num <- month_num$months_num
    
    #Determine The percentiles for this month based on historic 7-Day Average Streamflows
    gage [grep(month_num , gage$Date, perl = TRUE), "month"] <- month
    month_rows <- which(gage$month == month)
    month_data <- gage[month_rows, ]
    month_data <- na.omit(month_data) #in case the first month of historic record is same as current month
    
    month_flows_7day <- month_data$rollmean_7day
    quant_num <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    quant <- c(0, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 1)
    month_quant <- quantile(month_flows_7day, probs =  quant)
    
    percentiles <- round(month_quant, digits = 2)
    month_percentiles.row <- cbind(Month = month, t(percentiles))
    
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "0%"] <- "Percentile_0"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "5%"] <- "Percentile_5"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "10%"] <- "Percentile_10"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "25%"] <- "Percentile_25"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "50%"] <- "Percentile_50"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "75%"] <- "Percentile_75"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "90%"] <- "Percentile_90"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "95%"] <- "Percentile_95"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "100%"] <- "Percentile_100"
    
    percentiles.df <- rbind(percentiles.df, month_percentiles.row)
  }
  
  #Output CSV
  #startdate <- gage[1, ]$Date
  #enddate <- gage[length(gage$Date),]$Date
  #write.csv(percentiles.df,file = paste(gageid,'_monthly_percentiles_',startdate,'__',enddate,'.csv',sep = ""))
  
  percentiles.df <- percentiles.df
  
} #CLOSE FUNCTION

well_pct_mo <- function(siteNumber, percentiles.df) {
  print(paste("siteNumber ", siteNumber, sep = ''))
  
  
  welldata <- whatNWISdata(siteNumber = siteNumber)
  
  #Parameter code '72019' = Depth to water level, feet below land surface (ft) https://help.waterdata.usgs.gov/code/parameter_cd_nm_query?parm_nm_cd=%25level%25&fmt=html
  gwl_row <- which(welldata$parm_cd == 72019)
  gwl_rows <- welldata[gwl_row, ]
  gwl_rows <- gwl_rows[order(gwl_rows$begin_date , decreasing = TRUE), ]
  begin_date_row <- gwl_rows[length(gwl_rows$begin_date), ]
  begin_date <- begin_date_row$begin_date
  print(paste("Historic Record begining ", begin_date, sep = ""))
  
  url <-
    paste(
      "https://waterdata.usgs.gov/nwis/dv?cb_72019=on&format=rdb_meas&site_no=",
      siteNumber,
      "&referred_module=sw&period=&begin_date=",
      begin_date,
      "&end_date=",
      sep = ""
    )
  print(paste("Retrieving Data from NWIS using:", url))
  data <- read.table(url, header = TRUE, sep = "\t")
  
  data$max <- as.numeric(as.character(data[, 5])) #COPY MAX (Depth to water level) COLUMN AND FORCE 'NA's WHERE THERE IS MISSING DATA
  data$min <- as.numeric(as.character(data[, 7])) #COPY MIN (Depth to water level) COLUMN AND FORCE 'NA's WHERE THERE IS MISSING DATA
  data$mean <- rowMeans(subset(data, select = c(max, min)), na.rm = TRUE) #CREATE COLUMN OF MEAN OF MAX AND MIN COLUMNS
  
  ############################################################################
  ############################################################################
  rollmean_7day <- rollapply(data$mean, 7, mean, na.rm = TRUE)  #BUILD VECTOR OF 7-DAY ROLLING MEAN OF DAILY MEAN GWL
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)   #ROLLING MEAN FUNCTION EXCLUDED FIRST 6 DATA VALUES
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)     #MUST MANUALLY SET 'NA's FOR THE FIRST 6 VALUES
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)
  data$rollmean_7day <- rollmean_7day #CREATE COLUMN OF 7-DAY ROLLING MEANS ON DATAFRAME
  
  months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  months_num <- c('-01-','-02-','-03-','-04-','-05-','-06-','-07-','-08-','-09-','-10-','-11-','-12-')
  
  months_all <- data.frame(months, months_num)
  
  percentiles.df <- data.frame(
    Month = character(),
    Percentile_0 = character(),
    Percentile_5 = character(),
    Percentile_10 = character(),
    Percentile_25 = character(),
    Percentile_50 = character(),
    Percentile_75 = character(),
    Percentile_90 = character(),
    Percentile_95 = character(),
    Percentile_100 = character(),
    stringsAsFactors = FALSE
  )
  
  #x <- 1
  for (x in 1:length(months)) {
    daily_month <- months[x]
    
    print(paste("Processing ", daily_month, sep = ""))
    
    #Determine Current month name and numeric value
    month_row <- which(months_all$months == daily_month)
    month_num <- months_all[month_row, ]
    month <- month_num$months
    month <- toString(month)
    month_num <- month_num$months_num
    
    #Determine The percentiles for this month based on historic 7-Day Average Streamflows
    data [grep(month_num , data$datetime, perl = TRUE), "month"] <- month
    month_rows <- which(data$month == month)
    month_data <- data[month_rows, ]
    month_data <- na.omit(month_data) #in case the first month of historic record is same as current month
    month_flows_7day <- month_data$rollmean_7day
    quant_num <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
    quant <- c(0, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 1)
    month_quant <- quantile(month_flows_7day, probs =  quant)
    
    percentiles <- round(month_quant, digits = 2)
    percentiles <- rev(percentiles)
    month_percentiles.row <- cbind(Month = month, t(percentiles))
    
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "0%"] <- "Percentile_100"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "5%"] <- "Percentile_95"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "10%"] <- "Percentile_90"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "25%"] <- "Percentile_75"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "50%"] <- "Percentile_50"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "75%"] <- "Percentile_25"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "90%"] <- "Percentile_10"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "95%"] <- "Percentile_5"
    colnames(month_percentiles.row)[colnames(month_percentiles.row) == "100%"] <- "Percentile_0"
    
    percentiles.df <- rbind(percentiles.df, month_percentiles.row)
    
  }
  
  #startdate <- data[2, ]$datetime
  #enddate <- data[length(data$datetime), ]$datetime
  #write.csv(percentiles.df,file = paste(siteNumber,'_monthly_percentiles_',startdate,'__',enddate,'.csv',sep = ""))
  
  percentiles.df <- percentiles.df
  
} #CLOSE FUNCTION

  
  