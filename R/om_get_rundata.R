#' Get OM Model Data
#' @description Retrieve model results (run data) from the VA Hydro server for a
#'   particular model and run ID (scenario ID).
#' @details A wrapper of \code{hydrotools::fn_get_runfile()}. This function
#'   returns the results in a zoo (see zoo::zoo()) that has the timestamp as the
#'   index. See \code{hydrotools::fn_get_runfile()} for more details. If
#'   outaszoo is TRUE, the output will be a zoo TS with numeric mode. Please note
#'   that zoo's are matrices and cannot hold more than one data type such that
#'   character and date fields will be set to NA.
#' @param elid integer OM element connection ID e.g. the original OM model ID
#' @param runid integer run id representing the scenario. Ask the modeling team
#'   for scenario IDs if you are unsure otherwise see the WSPA Shiny Dashboard
#'   for more information
#' @param site URL of OM server, typically established in WSPA config files
#' @param cached boolean - if TRUE will use recently stored local copy
#' @param hydrowarmup boolean - if TRUE will trim beginning of model time frame
#'   to account for potential model warm up as water routes downstream from the
#'   headwaters and operational rules engage
#' @param cleanup Logical. Should the function delete the log file create for
#'   the cached argument? If this is TRUE, the .log files will be deleted after
#'   download from OM server
#' @param outaszoo boolean return as a zoo timeseries with numeric mode if TRUE,
#'   or as data frame if FALSE. Default is TRUE.
#' @return data.frame of model results
#' @export om_get_rundata
#' @examples #om_get_rundata(72446, 600, site=omsite)
om_get_rundata <- function(elid, runid, site='http://deq2.bse.vt.edu',
                           cached=FALSE, hydrowarmup=TRUE,
                           cleanup = FALSE,  outaszoo = TRUE) {
  # replace this with a single function that grabs
  # a hydro model for summarization and slims it down
  dat <- fn_get_runfile(elid, runid, 37, site,  cached = FALSE, cleanup = cleanup,
                        outaszoo = outaszoo)
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
  if(outaszoo){
    dat <- stats::window(dat, start = sdate, end = edate)
    #Change mode of zoo to numeric e.g. Convert all fields to numeric
    mode(dat) <- 'numeric'
  }else{
    dat <- dat[dat$timestamp >= sdate & dat$timestamp <= edate,]
  }

  return(dat)
}
