#' The base class for executable equation based meta-model components.
#'
#' @param elid integer OM model element id
#' @param runid integer run id 
#' @param site URL of om server
#' @param cached boolean - if TRUE will use recently stored local copy
#' @param hydrowarmup boolean - if TRUE will trim beginning of model time frame
#' @return reference class of type openmi.om.equation
#' @seealso NA
#' @export om_get_rundata
#' @examples NA
om_get_rundata <- function(elid, runid, site='http://deq2.bse.vt.edu', cached=FALSE, hydrowarmup=TRUE) {
  
  # replace this with a single function that grabs
  # a hydro model for summarization and slims it down
  dat <- fn_get_runfile(elid, runid, 37, site,  cached = FALSE)
  syear = as.integer(min(dat$year))
  eyear = as.integer(max(dat$year))
  if ((hydrowarmup == TRUE) & (syear < (eyear - 2))) {
    sdate <- paste0(syear,"-10-01")
    edate <- paste0(eyear,"-09-30")
  } else {
    # take a couple months in the begining to make some modest warmup.
    if (nrow(dat) > 90) {
      sdate <- paste0(syear,"-02-01")
      edate <- paste0(eyear,"-12-31")
    } else {
      sdate <- paste0(syear,"-01-01")
      edate <- paste0(eyear,"-12-31")
    }
  }
  #Due to change in stats::window in R 4.3.3, convert dates to posixCT to
  #ensure there are associated timezones
  sdate <- as.POSIXct(sdate,tz = "EST")
  edate <- as.POSIXct(edate,tz = "EST")
  #Get the window of interest from the timeseries
  dat <- window(dat, start = sdate, end = edate);
  #Change mode of zoo to numeric e.g. Convert all fields to numeric
  mode(dat) <- 'numeric'
  return(dat)
}