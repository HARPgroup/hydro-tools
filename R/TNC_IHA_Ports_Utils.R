#'Calculates group2 statistics from a matrix of rolling means
#'
#'Calculates group2 statistics from a matrix of rolling means. This include
#'range, base index (minimum 7-day flow divided by mean daily flow), and the
#'number of days with zero flow.
#'
#'@return a named vector containing the group2 statistics, including the range
#'of the 1-, 3-, 7-, 30-, and 90-day flows and the base index and days of zero
#'flow
#'@author jason.e.law@@gmail.com (imported to Hydrotools by Connor Brogan,connor.brogan@@deq.virginia.gov)
#'@param x a matrix of rolling means
#'@export
group2Funs <- function (x) {
  #Apply range over the second matrix margin (columns), ignoring NAs. This
  #returns a vector of length 2*ncol(x)
  rng <- as.numeric(apply(x, 2, base::range, na.rm = T))
  #Calculate the minimum 7-day average flow divided by the mean flow. Note that
  #the columns may change and thus this may not be accurate if the third column
  #is not 7-day flow and first column is not 1-day flow, but windows are
  #hard-coded in group2()
  baseindex <- base::min(x[, 3], na.rm = T) / base::mean(x[, 1], na.rm = T)
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


#'Calculate rolling means for group2 statistics
#'
#'@description
#'Calculate centered rolling means four group2 statsistics.  Uses runmean from caTools
#'to quickly get rolling averages for a time series.
#'
#'@details
#'If mimic.tnc is TRUE, it will calculate the running mean for each year
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
#'@author jason.e.law@@gmail.com (imported to Hydrotools by Connor Brogan,connor.brogan@@deq.virginia.gov)
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