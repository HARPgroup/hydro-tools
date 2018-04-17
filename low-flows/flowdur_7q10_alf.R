## Load necessary libraries
library('zoo')
library('IHA')
library("stringr")

### CREATE IMAGE FILES of AUGUST LOW FLOW - FLOW DURATION PLOT for ALL USGS GAGES

rm(list = ls()) # clear variables

#save_directory <- "/var/www/html/images/dh/dev"
save_directory <- "C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\plots" #For running locally
usgs_gage_functions <- "C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\hydro-tools\\usgs-gage\\" 

#Load Gage Functions               
source(paste(usgs_gage_functions,"usgs_gage_functions.R", sep = ""));

## Initialize ALF
alf <- c()
NP.alf <- c()


## Get active streamflow gages in VA
# URL to list of all active streamgages in VA
url <- "http://waterservices.usgs.gov/nwis/dv/?format=rdb&stateCd=va&siteStatus=active&variable=00060"

# determining how many active gages there are to specify when to stop reading data
all_info <- scan(url, what="character", skip=10, nlines=1)
num_sites <- as.numeric(all_info[6])
gage_info <- scan(url, what="character", skip=11, sep="\t", nlines=num_sites)

# setting up a pattern to pull only the gage numbers from the string
pattern <- "([[:digit:]]{8})"
x <- str_extract_all(gage_info, pattern)

# set the gage numbers as a vector
gage <- as.character(x)

#i <- 10
## Calculations & Plot for Each Gage 
for (i in 1:length(gage)) {   

  if (nchar(gage[i]) == 8) {
  data <- streamgage_historic(gage[i])
  } else {
    next
  }

  # Continuing even with error with file
  if (class(data)=="try-error") { 
    print(paste("Error: empty file ", url))
    alf[i] <- "FILE ERROR"
    
    NP <- seq(0, 100, 1) #make vector of all possible non-exceed probs
    sortflow <- rep(NA, times=length(NP)) #create vector of 0s for all NP values

    plot(NP, sortflow, pch=20, log='y', main=gage[i], ylim=c(1,100),  
         xlab="Percent Non-Exceedence (%)", ylab="Flow (cfs)")
    text((par()$usr[2]/2),10,  "No flow values available.") #Add error message in center of empty plot
    
    dev.off()  #close plot
    next
  }

  date <- as.Date(data$Date) #store vector of all dates 
  startdate <- min(date)
  enddate <- max(date)
  discharge <- as.numeric(as.vector(data$Flow)) #store vector of all discharge vals

  f3 <- zoo(as.numeric(as.vector(data$Flow)), order.by = as.Date(data$Date))
  
  # Omit any missing values from the data set and continuing on
  missing <- f3[!complete.cases(f3),]                     
  not_missing <- na.omit(f3) 
  
  # Check if all flow values are missing
  if (length(not_missing)==0) {
    NP <- seq(0, 100, 1) #make vector of all possible non-exceed probs
    sortflow <- rep(NA, times=length(NP)) #create vector of 0s for all NP values
    
    # Create plot file - showing ERROR
    file.name <- paste("usgs_", gage[i], "_alf_line_flowdur.png", sep="") #create name for file
    file.location <- paste("/var/www/html/images/dh/", file.name, sep="") #attach file name to proper directory
    write(c(), file = file.location)  #create file where image will be saved
    png(file.location)  #start writing to that file	
    
    plot(NP, sortflow, pch=20, log='y', main=gage[i], ylim=c(1,100),  
         xlab="Percent Non-Exceedence (%)", ylab="Flow (cfs)")
    text((par()$usr[2]/2),10,  "No flow values available.") #Add error message in center of empty plot
    
    dev.off()  #close plot
    next 
  }
  
  # Specify water year, and monthly minimum as the metric in "group1"
  modat <- group1(not_missing,"water","min")
  # Find the quantiles for August - the 50th percentile is the median of minimums or monthly low flow
  august <- as.numeric(modat[,"August"])
  qaug <- quantile(na.omit(august))
  alf[i] <- as.numeric(qaug["50%"])
  
  ## Create Flow Duration Curve
  #Sort/rank average daily discharges from largest to smallest value.
  sortflow <- sort(discharge, decreasing=TRUE)
  n <- length(sortflow)
  
  #Assign each discharge value a rank (M), starting with 1 for largest value.
  m <- 1:n
  
  #Calculate exceedence probability (P) as: P = 100 * [ M / (n + 1) ]
  P <- 100 * (m/(n+1))
  #Calculate non-exceedence probability (NP) as: NP = 100 - P
  NP <- 100 - P
  
  ## Find Location of Aug Low Flow on Flow Duration Curve
  location.alf <- which.min(abs(sortflow-as.numeric(alf[i])))
  curve.alf <- sortflow[location.alf]
  NP.alf[i] <- NP[location.alf]
  
  
  #-----------------------------------------------------------------------------------------
  #Calculate 7q10
  library('zoo')
  library('IHA')
  library(PearsonDS)
  zoots <- f3 
  g2 <- group2(zoots) 
  #print("Group 2, 7-day low flow results ")
  #print(g2["7 Day Min"])
  x <- as.vector(as.matrix(g2["7 Day Min"]))
  # fudge 0 values
  # correct for zeroes?? If so, use this loop:
  # This is not an "approved" method - we need to see how the GS/other authorities handles this
  for (k in 1:length(x)) {
    if (x[k] <= 0) {
      x[k] <- 0.00000001
      print (paste("Found 0.0 average in year", g2["year"], sep = " "))
    }
  }
  x <- log(x)
  pars <- PearsonDS:::pearsonIIIfitML(x)
  x7q10 <- exp(qpearsonIII(0.1, params = pars$par))
  
  ## Find Location of 7Q10 on Flow Duration Curve
  location.7q10 <- which.min(abs(sortflow-as.numeric(x7q10)))
  curve.7q10 <- sortflow[location.7q10]
  NP.7q10 <- NP[location.7q10]
  
  #-----------------------------------------------------------------------------------------
  
  ## Create Plot File
  file.name <- paste("/usgs_", gage[i], "_alf_line_flowdur.png", sep="") #create name for file
  file.location <- paste(save_directory, file.name, sep="") #attach file name to proper directory
  write(c(), file = file.location)  #create file where image will be saved
  png(file.location)  #start writing to that file	
  
  plot(NP, sortflow, pch=20, log='y', main=paste("Gage ",gage[i]," Flow Duration Curve","\n",startdate," to ",enddate,sep=""), 
  
       xlab="Percent Non-Exceedence (%)", ylab="Flow (cfs)")
  #Add August Low Flow point and lines
  points(NP.alf[i], curve.alf, pch=3, col="red")
  abline(h=curve.alf, lty=2, col="red")
  abline(v=NP.alf[i], lty=2, col="red")
  
  abline(h=x7q10, lty=3, col="blue")
  abline(v=NP.7q10, lty=3, col="blue")
  
  legend("topleft", c("Flow Duration Curve", 
                      paste("ALF = ",curve.alf," cfs",sep=""), 
                      paste("7Q10 = ",round(x7q10,2)," cfs",sep="")), 
         pch=c(20, NA, NA), lty=c(NA, 2,3), col=c("black", "red", "blue"), bg="white")
  
  dev.off()  #close plot
}
