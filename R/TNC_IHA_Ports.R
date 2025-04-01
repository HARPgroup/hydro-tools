#'Extract the month of the water year from a date or time object.
#'
#'Extract the month of the water year from a date or time object.  Can be returned
#'as a number 1:12, for October through September respectively, or an ordered factor. 
#'A user may alternatively enter a calendar year month as a numeric to return
#'the equivalent water year month
#'@param x a date or time object or a numeric representing a calendar year month
#'@param label logical TRUE will return an ordered factor for month with the
#'  month name as labels, FALSE will return a numeric
#'@param abbr logical. FALSE will abbreviate the name of the month in the ordered factor.
#'@return If x is numeric, then returns a numeric. Otherwise, if label is TRUE,
#'  returns an ordered factor indicating the equivalent month of the water year.
#'  Otherwise, returns only the numeric that represents the month.
#'@author jason.e.law@@gmail.com (imported to Hydrotools by Connor Brogan,connor.brogan@deq.virginia.gov)
#'@S3method water.month default
#'@S3method water.month numeric
#'@importFrom lubridate month
#'@export water.month
water.month <- function(x, label = FALSE, abbr = TRUE){
  UseMethod('water.month')
}


#A simple function that in a number and returns the month in the water year that
#would be that number away from the beginning of the water year (October 1st).
#e.g. water.month(4, TRUE) returns January 
#Returns x is label is not TRUE, otherwise returns the abbr or full name of
#month
water.month.numeric <- function (
    #Month number of interest  
  x, 
  #Should result be labeled as a month (TRUE) or a number (FALSE, returns x)?
  label = FALSE, 
  #Should months be abbreviated?
  abbr = TRUE) {
  #If label is FALSE, return x
  if (!label) {
    return(x)
  }
  #Return the abbreviated or full month name
  if (abbr) {
    labels <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", 
                "Apr", "May", "Jun", "Jul", "Aug", "Sep")
  }
  else {
    labels <- c("October", "November", "December", "January", 
                "February", "March", "April", "May", "June", "July", 
                "August", "September")
  }
  #Return the xth level of an ordered factor labeled by labels
  ordered(x, levels = 1:12, labels = labels)
}

#A function that uses water.month.numeric() to find the water month for an input
#Date x. This function takes similar arguments to water.month.numeric()
water.month.default <- function (    
    #Date of interest  
  x, 
  #Should result be labeled as a month (TRUE) or a number (FALSE, returns x)?
  label = FALSE, 
  #Should months be abbreviated?
  abbr = TRUE) {
  #Get the month associated with x using lubridate::month, then return the
  #appropriate month of the water year
  water.month.numeric(c(4:12, 1:3)[lubridate::month(x, label = FALSE)],
                      label, abbr)
}


#'Return a numeric giving the water year for a date
#'
#'Returns a number specifying the water year (\code{2010}) for a date. Water year is hardcoded here as Oct 1 - Sept
#'30
#'@param x a date-time object which can be handled by lubridate
#'@return Returns the equivalent water year of the date x input by the uee
#'@author jason.e.law@@gmail.com (imported to Hydrotools by Connor Brogan,connor.brogan@deq.virginia.gov)
#'@importFrom lubridate month year
#'@export
#
water.year <- function (x) {
  #Get the year of x using lubridate
  yr <- lubridate::year(x)
  #If the month of x is greater than September, increment the year by one to
  #reflect the water year (e.g. water year 2024 ENDs on 09/31/2025)
  ifelse(lubridate::month(x, label = FALSE) > 9, yr + 1, yr)
}



#'Magnitude of monthly water conditions
#'
#'Calculates the IHA parameter group 1: Magnitude of montly water conditions
#'group1() applies an input function FUN to a 1-D zoo x, grouping by water
#'month+water year or calendar month+calendar year depending on user input
#'
#'See IHA documentation:
#'\url{http://www.nature.org/initiatives/freshwater/conservationtools/art17004.html}
#'
#'@param x A zoo timeseries object containing the flow series
#'@param year The type of year factor to be used when determining statistcs,
#'  \code{yr = 'water'} or \code{yr ='calendar'} for water years and calculated years respectively
#'@param FUN the function to be applied to the monthly values.  Median is the
#'  default here. This can be a character string of a function name but should
#'  ideally be a closure
#'@return A matrix with the monthly results of FUN over each year
#'@author jason.e.law@@gmail.com (imported to Hydrotools by Connor Brogan,connor.brogan@deq.virginia.gov)
#'@references
#'\url{http://www.nature.org/initiatives/freshwater/conservationtools/art17004.html}
#'@importFrom zoo index coredata is.zoo
#'@importFrom lubridate year month
#'@export
#'@examples
#'library(dataRetrieval)
#'gageData <- dataRetrieval::readNWISdv("01634500","00060")
#'gageFlow <- zoo(test[,4],order.by = test$Date)
#'group1(gageFlow,'water',mean)
group1 <- function (
    #A zoo timeseries  
  x, 
  #Whether statistics should be applied on a water or calendar year basis
  yearType = c("water", "calendar"), 
  #The name of a function (character) OR a function (closure)
  FUN = median) {
  #x must be a zoo:
  stopifnot(is.zoo(x))
  #Get the yearType argument of the function input (either water or calendar)
  yearType <- match.arg(yearType)
  if(!(yearType %in% c('water','calendar'))){
    stop(paste0("'yearType' argument must be either 'water' or 'calendar'.
    ",yearType," has not yet been implemented"))
  }
  #Get the dates associated with the zoo timeseries (stored in the index)
  idx <- index(x)
  #Set the month and year to be either water year or calendar year based using
  #switch and the yearType input by user
  yr <- switch(yearType, water = water.year(idx), calendar = year(idx))
  mo <- switch(yearType, water = water.month(idx, label = TRUE, 
                                             abbr = FALSE),
               calendar = month(idx, label = TRUE, abbr = FALSE))
  #Apply the input function FUN to the unique combination of mo and yr. Since
  #zoos are inherently ordered, no other ordering is needed to group by month
  #and year
  res <- tapply(zoo::coredata(x), list(mo, yr), FUN)
  #Store the function as an attribute for reference by user before returning
  #data back to the user
  attr(res, "FUN") <- deparse(substitute(FUN))
  return(t(res))
}

#'Calculates the group2 IHA statistics
#'
#'The group 2 statistics measure the magnitude of monthly water condition and
#'include 12 parameters. This will calculate several metrics for each year of
#'the 1-D zoo input x.
#'
#'This function divides the zoo by water or calendar year
#'and finds the 1, 3, 7, 30, and 90 day rolling averages of the zoo. Then, it 
#'finds the range, base index, and days of zero flow of each year, with base 
#'index defined as the minimum 7-day flow divided by the average flow
#'
#'@param x A zoo timeseries object containing the flow series
#'@param yearType The type of year factor to be used when determining statistcs,
#'  \code{yr = 'water'} or \code{yr ='calendar'} for water years and calculated years respectively
#'@param mimic.tnc should the function perform the calculation like the TNC IHA
#'software? If mimic.tnc is TRUE, then running means will be calculated for each
#'year individually and will not use data from the previous or next year, thereby
#treating each year independently of the others. group2 statistics will then be
#calcualted from these running means
#'@param ... additional arguments passed to ddply. Likely not used without
#'  modificiation to this function
#'@return a data frame with the group 2 statistics for each year
#'@author jason.e.law@@gmail.com (imported to Hydrotools by Connor Brogan,connor.brogan@deq.virginia.gov)
#'@references \url{http://www.conservationgateway.org/Files/Pages/indicators-hydrologic-altaspx47.aspx}
#'@importFrom plyr ddply '.'
#'@importFrom zoo coredata index
#'@importFrom lubridate year
#'@export
#'@examples
#'library(dataRetrieval)
#'gageData <- dataRetrieval::readNWISdv("01634500","00060")
#'gageFlow <- zoo(test[,4],order.by = test$Date)
#'group2(gageFlow,'water',TRUE)
group2 <- function ( 
    #A zoo timeseries  
  x, 
  #Whether statistics should be applied on a water or calendar year basis
  yearType = c("water", "calendar"), 
  mimic.tnc = T, ...) {
  
  #x must be a zoo:
  stopifnot(is.zoo(x))
  #Get the yearType argument of the function input (either water or calendar)
  yearType <- match.arg(yearType)
  if(!(yearType %in% c('water','calendar'))){
    stop(paste0("'yearType' argument must be either 'water' or 'calendar'.
    ",yearType," has not yet been implemented"))
  }
  #Get the year as either water year or calendar year based using switch and the
  #yearType input by user and getting the timeseries from zoo using index
  yr <- switch(yearType, water = water.year(index(x)), calendar = year(index(x)))
  #Calculate the rolling average of all data, treating years independently (not
  #allowing running averages to use data outside of the current year)
  #(controlled by mimic.tnc)
  rollx <- runmean.iha(x, year = yr, mimic.tnc = mimic.tnc)
  #Add the years as the first column to rollx
  xd <- cbind(year = yr, as.data.frame(rollx))
  #Use plyr::ddply to apply group2Funs() to each subset of xd based on the
  #grouping variable year e.g. apply group2Funs() to each year of data in xd and
  #combine results into one data.frame
  res <- plyr::ddply(xd, .(year), function(x) group2Funs(x[, -1]), 
                     ...)
  return(res)
}


#'Calculate rolling means for group2 statistics
#'
#'Calculate centered rolling means four group2 statsistics.  Uses runmean from caTools
#'to quickly get rolling averages for a time series. 
#'
#'If mimic.tnc is TRUE, it will calculate running means for each year
#'independtly of the others (thereby not using data from the next or previous
#'year), If mimic.tnc is TRUE, yearVector must be provided as a vector of the
#'year for each entry in x. Rolling means are calculated as centered means,
#'using data that occurs before and after a given day to compute the rolling
#'mean
#'@param x a numeric vector
#'@param yearVector a vector of years (calendar or water year identifiers; necessary 
#'for mimic.tnc = TRUE)
#'@param mimic.tnc logical should the years be split before the running mean is 
#'calculated e.g. should running means for each year be calculated
#'independently? If mimic.tnc is TRUE, then yearVector must be provided and
#'running means will be calculated for each year of data without using data from
#'the following or previous year
#'@return a matrix with the rolling means of all the data combined into columns.
#'  Each column represents a different rolling average and is either the 1-day,
#'  3-day, 7-day, 30-day, or 90-day average. NAs will be present at the start and
#'  end of each year if \code{mimic.tnc = TRUE}
#'@author jason.e.law@@gmail.com (imported to Hydrotools by Connor Brogan,connor.brogan@deq.virginia.gov)
#'@importFrom caTools runmean
#'@export
runmean.iha <- function (x, yearVector = NULL, mimic.tnc = F) {
  #Typical hydrology low flow periods
  window <- c(1, 3, 7, 30, 90)
  #Create a function wrapper for runmean that vectorizes the argument k for
  #runmean. This argument specifies the window over which to calculate the
  #running average
  vrunmean <- Vectorize(caTools::runmean, vectorize.args = "k")
  
  #If mimic.tnc is TRUE, then running means will be calculated for each year
  #individually and will not use data from the previous or next year, thereby
  #treating each year independently of the others
  if (mimic.tnc) {
    #Divide the coredata by the year, creating a list where each entry
    #represents a separate year
    sx <- split(coredata(x), yearVector)
    #Use the vectorized running mean function and the 'fast' c algorithim to
    #calculate the rolling average of each entry in sx (each year) leaving NAs
    #at the start and end of each year since the caTools::runMean defaults to
    #center running means
    rollx <- lapply(sx, vrunmean, k = window, alg = "fast", 
                    endrule = "NA")
    #Combine the list entries into a single data frame
    rollx <- do.call("rbind", rollx)
  } else {
    #Calculate the running mean of all data concurrently, assuming years are not
    #independent of one anothe
    rollx <- vrunmean(coredata(x), k = window, alg = "fast", 
                      endrule = "NA")
  }
  #Add w to the start of each "window" and use these as the column names for
  #rollx
  colnames(rollx) <- sprintf("w%s", window)
  return(rollx)
}

#'Calculates group2 statistics from a matrix of rolling means
#'
#'Calculates group2 statistics from a matrix of rolling means. This include
#'range, base index (minimum 7-day flow divided by mean daily flow), and the
#'number of days with zero flow.
#'#'@return a named vector containing the group2 statistics, including the range
#'of the 1-, 3-, 7-, 30-, and 90-day flows and the base index and days of zero
#'flow
#'@author jason.e.law@@gmail.com (imported to Hydrotools by Connor Brogan,connor.brogan@deq.virginia.gov)
#'@param x a matrix of rolling means
#'@export
group2Funs <- function (x) {
  #Apply range over the second matrix margin (columns), ignoring NAs. This
  #returns a vector of length 2*ncol(x)
  rng <- as.numeric(apply(x, 2, range, na.rm = T))
  #Calculate the minimum 7-day average flow divided by the mean flow. Note that
  #the columns may change and thus this may not be accurate if the third column
  #is not 7-day flow and first column is not 1-day flow, but windows are
  #hard-coded in group2()
  baseindex <- min(x[, 3], na.rm = T)/mean(x[, 1], na.rm = T)
  #Find the number of days of zero flow (daily flow ONLY)
  zeros <- length(which(x[, 1] == 0))
  #Combine metrics
  ans <- c(rng, zeros, baseindex)
  #Label the ranges appropriately
  nms <- sprintf(c("%1$s Day Min", "%1$s Day Max"), 
                 rep(c(1, 3, 7, 30, 90), each = 2))
  #Set the names for the metrics, include the zero flow days and the base index
  names(ans) <- c(nms, "Zero flow days", "Base index")
  return(ans)
}
