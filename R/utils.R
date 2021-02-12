# Automating August Low Flows

library('zoo')
#library('IHA')
options(timeout=480); # set timeout to twice default level to avoid abort due to high traffic
#' Retrieve Run summary data from Old OM model
#'
#' @param elementid integer OM model element id
#' @param runid integer run id 
#' @param varname character - if set will isolate a single column of data
#' @param scenid integer - model domain ID
#' @param site URL of om server
#' @return reference class of type openmi.om.equation
#' @seealso NA
#' @export fn_get_rundata
#' @examples NA
fn_get_rundata <- function(
  elementid = -1, runid = -1, 
  varname = 'Qout', scenid = 37,
  site = "http://deq2.bse.vt.edu"
  ) {
  if (elementid == -1 ) {
    return(FALSE);
  }
  if (runid == -1 ) {
    return(FALSE);
  }
  # may be obsolete
  #setInternet2(TRUE)

  # Set up query for batch of model objects
  # Internal variable to construct the query
  urlbase<- paste(site, "om/remote/get_modelData.php?elementid=", sep='/');
  print(paste("Getting data for run ", runid, " for element ", elementid))      # creates the whole url by pasting the element and run ids into it
  filename<-paste(urlbase, elementid, "&variables=", varname, "&runid=", runid, "&startdate=1984-10-01&enddate=2005-09-30", sep = "")
  print(paste("From ", filename));
  
  dat = try(read.table(filename, header = TRUE, sep = ",")) 
  if (class(dat)=='try-error') { 
    # what to do if file empty 
    print(paste("Error: empty file ", filename))
    return (FALSE);
  } else { 
    #dat<-read.table(filename, header = TRUE, sep = ",")   #  reads the csv-formatted data from the url	
    print(paste("Data obtained, found ", length(dat[,1]), " lines - formatting for IHA analysis"))
    datv<-as.vector(dat)  # stores the data as a vector     
    datv$thisdate <- as.POSIXct(datv$thisdate)
    f3 <- zoo(datv[,paste(varname, runid, sep="_")], order.by = datv$thisdate)
  }
  return(f3);
  
}

#' Retrieve Info About Run File from Old OM model
#'
#' @param elementid integer OM model element id
#' @param runid integer run id 
#' @param scenid integer - model domain ID
#' @param site URL of om server
#' @return reference class of type openmi.om.equation
#' @seealso NA
#' @export fn_get_runfile_info
#' @examples NA
fn_get_runfile_info <- function(
  elementid = -1, runid = -1, scenid = 37,
  site = "http://deq2.bse.vt.edu"
) {
  if (elementid == -1 ) {
    return(FALSE);
  }
  if (runid == -1 ) {
    return(FALSE);
  }
  # may be obsolete
  #setInternet2(TRUE)
  
  # just get the run file
  urlbase<- paste(site, "om/remote/get_modelData.php?operation=11&delimiter=tab&elementid=", sep='/');
  print(paste("Getting Info for run ", runid, " for element ", elementid))      # creates the whole url by pasting the element and run ids into it
  filename<-paste(urlbase, elementid, "&runid=", runid, "&startdate=1984-10-01&enddate=2005-09-30", sep = "")
  print(paste("From ", filename))
  finfo = try(read.csv(filename, header = TRUE, sep = "\t")) ;
  if (class(finfo)=='try-error') { 
    # what to do if file empty 
    print(paste("Error: retrieving ", filename))
    return (FALSE);
  }
  print("Returning file Info")
  return(finfo);
  
}

#' Retrieve Entire Run Log File from Old OM model
#'
#' @param elementid integer OM model element id
#' @param runid integer run id 
#' @param scenid integer - model domain ID
#' @param site URL of om server
#' @param cached boolean - use local copy or force refresh
#' @param outaszoo boolean return as a zoo timeseries if TRUE, or as data frame
#' @param use_tz character pass in a custom timezone for zoo
#' @return reference class of type openmi.om.equation
#' @seealso NA
#' @export fn_get_runfile_info
#' @examples NA
fn_get_runfile <- function(
  elementid = -1, runid = -1, scenid = 37,
  site = "http://deq2.bse.vt.edu", cached = TRUE, 
  outaszoo=TRUE, use_tz=FALSE
  ) {
  if (elementid == -1 ) {
    return(FALSE);
  }
  if (runid == -1 ) {
    return(FALSE);
  }
  # may be obsolete
  #setInternet2(TRUE)

  # just get the run file
  finfo = fn_get_runfile_info(elementid, runid, scenid, site)
  if (!is.list(finfo)) {
    return(FALSE);
  }
  if (finfo$compressed == 1) {
    # If the host is not the same as site, and finfo$compressed == 1, then we need to 
    # Repeat this request on the other host
    host_site <- paste0('http://',finfo$host)
    if (host_site != site) {
      print("Compressed file requested, repeating req1uest on model run host site")
      finfo <- fn_get_runfile_info(elementid, runid, scenid, host_site)
    }
  }
  filename = as.character(finfo$remote_url);
  localname = basename(as.character(finfo$output_file));
  if (cached & file.exists(localname)) {
    linfo = file.info(localname)
    if (as.Date(finfo$run_date) > as.Date(linfo$mtime)) {
      # re-download if the remote is newer than the local
      if (finfo$compressed == 1) {
        print(paste("Downloading Compressed Run File ", filename));
        download.file(filename,'tempfile',mode="wb", method = "libcurl");
        filename <-  unzip ('tempfile');
      } else {
        print(paste("Downloading Un-compressed Run File ", filename));
      }
    } else {
      # not new, so just use the local copy
      print(paste("Remote file date ", as.Date(finfo$run_date), " <= run date ", as.Date(linfo$mtime), "Using cached copy "));
      filename = localname
    }
  } else {
    # does not exist locally
    print(paste("Downloading Run File ", filename));
    download.file(filename,'tempfile',mode="wb", method = "libcurl");
    if (finfo$compressed == 1) {
      print(paste("Unpacking Compressed Run File ", filename));
      filename <-  unzip ('tempfile');
    }
  }
  dat = try(read.table( filename, header = TRUE, sep = ",")) ;
  if (class(dat)=='try-error') { 
    # what to do if file empty 
    print(paste("Error: empty file ", filename))
    return (FALSE);
  } else { 
    #dat<-read.table(filename, header = TRUE, sep = ",")   #  reads the csv-formatted data from the url	
    print(paste("Data obtained, found ", length(dat[,1]), " lines - formatting for IHA analysis"))
    datv<-as.vector(dat)  # stores the data as a vector     
    if (is.logical(use_tz)) {
      datv$timestamp <- as.POSIXct(datv$timestamp,origin="1970-01-01")
    } else {
      datv$timestamp <- as.POSIXct(datv$timestamp,origin="1970-01-01", tz = use_tz)
    }
    f3 <- zoo(datv, order.by = datv$timestamp)
  }
  unlink('tempfile')
  if(outaszoo){
    return(f3)  
  }else{
    return(datv)  
  }
}

fn_storeprop_vahydro1 = function(site = "http://deq2.bse.vt.edu"){
  # NOT FINISHED - JUST PASTED CODE
  url <- paste(site,"om/remote/setModelData.php?hash=", sep='/');
  print (paste("Setting 7Q10 for element ", id, " run id ", rid, " to ", x7q10 , sep = "") )
  # building the correct url
  ins_url <- paste(url, hash, "&username=", username, "&elementid=", id, "&runid=", rid, "&dataname=7q10&reporting_frequency=single&dataval=", x7q10, "&starttime=1984-10-01&endtime=2005-09-30&temporal_res=water_year", sep = "")  
  #shell.exec(alf_url)  # opening the webpage
  print(ins_url);
  readLines(ins_url)
}
