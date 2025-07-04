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
  message(paste("Getting data for run ", runid, " for element ", elementid))      # creates the whole url by pasting the element and run ids into it
  filename<-paste(urlbase, elementid, "&variables=", varname, "&runid=", runid, "&startdate=1984-10-01&enddate=2005-09-30", sep = "")
  message(paste("From ", filename));
  
  dat = try(read.table(filename, header = TRUE, sep = ",")) 
  if (class(dat)=='try-error') { 
    # what to do if file empty 
    message(paste("Error: problem reading file ", filename))
    return (FALSE);
  } else { 
    #dat<-read.table(filename, header = TRUE, sep = ",")   #  reads the csv-formatted data from the url	
    message(paste("Data obtained, found ", length(dat[,1]), " lines - formatting for IHA analysis"))
    datv<-as.vector(dat)  # stores the data as a vector     
    datv$thisdate <- as.POSIXct(datv$thisdate)
    f3 <- zoo::zoo(datv[,paste(varname, runid, sep="_")], order.by = datv$thisdate)
  }
  return(f3);
  
}


#' Truncate run data to best guess for warmup and water year periods
#'
#' @param dat the data timeseries to handle
#' @return a shortened time series
#' @seealso NA
#' @export fn_remove_model_warmup
#' @examples NA
fn_remove_model_warmup <- function(dat) {
  syear = as.integer(min(dat$year))
  eyear = as.integer(max(dat$year))
  if (syear < (eyear - 2)) {
    sdate <- as.POSIXct(paste0(syear,"-10-01"), tz = "UTC")
    edate <- as.POSIXct(paste0(eyear,"-09-30"), tz = "UTC")
    flow_year_type <- 'water'
  } else {
    sdate <- as.POSIXct(paste0(syear,"-02-01"), tz = "UTC")
    edate <- as.POSIXct(paste0(eyear,"-12-31"), tz = "UTC")
    flow_year_type <- 'calendar'
  }
  #Due to change in stats::window in R 4.3.3, convert dates to posixCT to
  #ensure there are associated timezones
  sdate <- as.POSIXct(sdate,tz = "EST")
  edate <- as.POSIXct(edate,tz = "EST")
  dat <- window(dat, start = sdate, end = edate)
  return(dat)
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
  message(paste("Getting Info for run ", runid, " for element ", elementid))      # creates the whole url by pasting the element and run ids into it
  filename<-paste(urlbase, elementid, "&runid=", runid, "&startdate=1984-10-01&enddate=2005-09-30", sep = "")
  message(paste("From ", filename))
  finfo = try(read.csv(filename, header = TRUE, sep = "\t")) ;
  if (class(finfo)=='try-error') { 
    # what to do if file empty 
    message(paste("Error: retrieving ", filename))
    return (FALSE);
  }
  message("Returning file Info")
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
#' @param cleanup Logical - Should the log file be deleted (TRUE) or written to
#'   working directory (FALSE)? Note that the cached argument requires these log
#'   files so setting cleanup to FALSE will prevent caching regardless of cached
#'   argument
#' @return reference class of type openmi.om.equation
#' @seealso NA
#' @export fn_get_runfile
#' @examples NA
fn_get_runfile <- function(
  elementid = -1, runid = -1, scenid = 37,
  site = "http://deq2.bse.vt.edu", cached = TRUE, 
  outaszoo=TRUE, use_tz=FALSE,
  cleanup = FALSE
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
  # set this for comparisons
  host_site <- paste0('http://',finfo$host)
  if (finfo$compressed == 1) {
    # If the host is not the same as site, and finfo$compressed == 1, then we need to 
    # Repeat this request on the other host
    if (host_site != site) {
      finfo_save <- finfo
      message("Compressed file requested, repeating request on model run host site")
      finfo <- try(fn_get_runfile_info(elementid, runid, scenid, host_site))
      if (is.logical(finfo) | class(finfo)=='try-error') { 
        message("host site retrieval failed, trying original site.")
        finfo <- finfo_save
      }
    } else {
      # allow access to local file which will be much faster
      cached = TRUE
    }
  }
  if (!is.list(finfo)) {
    return(FALSE);
  }
  filename = as.character(finfo$remote_url);
  message(paste("Comparing host_site and site", host_site, "site"))
  if (host_site == site) {
    localname = finfo$output_file
    cached = TRUE
    message(paste("Using Local File Storage", localname))
  } else {
    localname = basename(as.character(finfo$output_file));
  }
  if (cached & file.exists(localname)) {
    linfo = file.info(localname)
    if (as.Date(finfo$run_date) > as.Date(linfo$mtime)) {
      # re-download if the remote is newer than the local
      if (finfo$compressed == 1) {
        message(paste("Downloading Compressed Run File ", filename));
        drez <- try(download.file(filename,'tempfile',mode="wb", method = "libcurl"))
        if ((drez == FALSE) | class(drez)=='try-error') { 
          message(paste("Download for", filename, "failed. "))
          return(FALSE)
        }
        filename <-  utils::unzip ('tempfile');
      } else {
        message(paste("Downloading Un-compressed Run File ", filename));
      }
    } else {
      # not new, so just use the local copy
      message(paste("Remote file date ", as.Date(finfo$run_date), " <= run date ", as.Date(linfo$mtime), "Using cached copy "));
      filename = localname
    }
  } else {
    # does not exist locally
    message(paste("Downloading Run File ", filename));
    drez <- try(download.file(filename,'tempfile',mode="wb", method = "libcurl"))
    if (is.logical(drez) | class(drez)=='try-error') { 
      message(paste("Download for", filename, "failed."))
      return(FALSE)
    }
    if (finfo$compressed == 1) {
      message(paste("Unpacking Compressed Run File ", filename)) 
      filename <-  utils::unzip ('tempfile');
    }
  }
  dat = try(read.table( filename, header = TRUE, sep = ",")) 
  if (is.logical(dat) | class(dat)=='try-error') { 
    # what to do if file empty 
    message(paste("Error: empty file ", filename))
    return (FALSE);
  } else { 
    #dat<-read.table(filename, header = TRUE, sep = ",")   #  reads the csv-formatted data from the url	
    message(paste("Data obtained, found ", length(dat[,1]), " lines - formatting for IHA analysis"))
    datv <- dat  # copy the data  
    if (is.logical(use_tz)) {
      datv$timestamp <- as.POSIXct(datv$timestamp,origin="1970-01-01")
    } else {
      datv$timestamp <- as.POSIXct(datv$timestamp,origin="1970-01-01", tz = use_tz)
    }
    f3 <- zoo::zoo(datv, order.by = datv$timestamp)
  }
  unlink('tempfile')
  #If the user has provided a logical value for cleanup, check to see if user
  #has requested the logfile be deleted. If cleanup is TRUE, delete the log file
  if(is.logical(cleanup) && cleanup){
    unlink(filename)
  }
  #Return either the data frame or the zoo
  if(outaszoo){
    return(f3)  
  }else{
    return(datv)  
  }
}


#' Retrieve Variable Def from View (not REST)
#'
#' @param varkey character variable key
#' @param site URL of om server
#' @param token for xhttp auth
#' @param debug show debugging info
#' @return variable definition as list
#' @seealso NA
#' @export fn_get_vardef_view
#' @examples NA
fn_get_vardef_view <- function(varkey, site, token, debug = FALSE) {
  tsdef_url<- paste(site,"?q=vardefs.tsv/", varkey,sep="")
  #tsdef_table <- read.table(tsdef_url,header = TRUE, sep = "\t")    
  #message(paste("Trying ", tsdef_url))
  tsdef_table <- om_auth_read(tsdef_url, token, "text/tab-separated-values", delim = "\t")
  vardef <- as.list(tsdef_table[1,])
  varid <- vardef$varid
  if (debug) {
    message(paste("varid: ",varid,sep=""))
  }
  if (is.na(varid)) {
    # we sent a bad variable id so we should return FALSE
    message(paste("Could not find variable key", varkey))
    return(FALSE)
  }
  return(vardef)
}

#' Retrieve time series value from RESTful web service
#'
#' @param inputs = list(entity_type, featureid, tid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
#' @param site URL of om server
#' @param token show debugging info
#' @return integer variable id
#' @seealso NA
#' @export fn_get_timeseries
#' @examples NA
fn_get_timeseries <- function(inputs, site, token){
  #Convert varkey to varid - needed for REST operations 
  varid <- NULL 
  if (!is.null(inputs$varkey)) {
    vardef <- fn_get_vardef_view(inputs$varkey, site, token)
    varid <- vardef$varid
    if (!varid) {
      # we got a bad variable id so we should return FALSE
      return(FALSE)
    }
  }
  
  pbody = list(
    featureid = inputs$featureid,
    entity_type = inputs$entity_type
  );
  if (!is.null(varid)) {
    pbody$varid = varid
  }
  if (!is.null(inputs$tscode)) {
    pbody$tscode = inputs$tscode
  }
  if (!is.null(inputs$tstime)) {
    pbody$tstime = inputs$tstime
  }
  if (!is.null(inputs$tid)) {
    if (!is.na(inputs$tid)) {
      # forget about other attributes, just use tid
      pbody = list(
        tid = inputs$tid
      )
    }
  }
  if (!is.null(inputs$page)) {
    pbody$page = inputs$page
    multipage = FALSE
  } else {
    page = 0
    pbody$page = 0
    multipage = TRUE ; # do we support multiple pages if records exceed limit?
  }
  if (!is.null(inputs$limit)) {
    pbody$limit = inputs$limit
  } else {
    pbody$limit = 0 # get all
  }
  ts <- data.frame(
    tid=integer(),
    tsvalue=character(),
    tscode=character(),
    tstime=character(),
    tsendtime=character(),
    featureid=character(),
    modified=character(),
    entity_type=character(),
    varid=character(),
    uid=character(),
    status=character(),
    stringsAsFactors=FALSE) 
  # set morepages to true to start, if multipage = FALSE, this gets reset immediately after 1st retrieval
  morepages = TRUE
  while (morepages == TRUE) {
    tsrest <- httr::GET(
      paste(site,"/dh_timeseries.json",sep=""), 
      httr::add_headers(HTTP_X_CSRF_TOKEN = token),
      query = pbody, 
      encode = "json"
    )
    #message(tsrest)
    ts_cont <- httr::content(tsrest);
    #message(paste(site,"/dh_timeseries.json",sep=""))
    
    if (length(ts_cont$list) != 0) {
      
      i <- 1
      numrecs = length(ts_cont$list)
      message(paste("----- Number of timeseries found: ",numrecs,sep=""))
      for (i in 1:numrecs) {
        
        ts_i <- as.data.frame( 
          list(
            tid = if (is.null(ts_cont$list[[i]]$tid)){NA} else {as.integer(as.character(ts_cont$list[[i]]$tid))},
            tsvalue = if (is.null(ts_cont$list[[i]]$tsvalue)){NA} else {as.numeric(as.character(ts_cont$list[[i]]$tsvalue))},
            tscode = if (is.null(ts_cont$list[[i]]$tscode)){NA} else {as.character(ts_cont$list[[i]]$tscode)},
            tstime = if (is.null(ts_cont$list[[i]]$tstime)){NA} else {as.integer(ts_cont$list[[i]]$tstime)},
            tsendtime = if (is.null(ts_cont$list[[i]]$tsendtime)){NA} else {as.integer(ts_cont$list[[i]]$tsendtime)},
            featureid = if (is.null(ts_cont$list[[i]]$featureid)){NA} else {as.integer(ts_cont$list[[i]]$featureid)},
            modified = if (is.null(ts_cont$list[[i]]$modified)){NA} else {as.integer(ts_cont$list[[i]]$modified)},
            entity_type = if (is.null(ts_cont$list[[i]]$entity_type)){NA} else {as.character(as.character(ts_cont$list[[i]]$entity_type))},
            varid = if (is.null(ts_cont$list[[i]]$varid)){NA} else {as.integer(ts_cont$list[[i]]$varid)},
            uid = if (is.null(ts_cont$list[[i]]$uid)){NA} else {as.integer(ts_cont$list[[i]]$uid)},
            status = if (is.null(ts_cont$list[[i]]$status)){NA} else {as.integer(ts_cont$list[[i]]$status)}
          ),
          stringsAsFactors=FALSE
        )
        ts  <- rbind(ts, ts_i)
      }
      
      
      trecs <- length(ts[,1])
      #message(trecs)
      # trecs = as.integer(count(ts))
      # pbody$limit <- 1
      # message(pbody$limit)
      
      if ( (pbody$limit > 0) & (trecs >= pbody$limit) ) {
        morepages = FALSE
      } else {
        morepages = TRUE
        pbody$page = pbody$page + 1
      }
    } else {
      morepages = FALSE
      #trecs = as.integer(count(ts))
      trecs <- length(ts[,1])
      if (trecs == 0) {
        message("----- This timeseries does not exist")
        ts = FALSE
      } else {
        message(paste("Total =", trecs))
      }
    }
  }
  return(ts)
}

#' Post any entity to a RESTful web service
#'
#' @param entity_type = dh_feature, dh_properties, ...
#' @param pk = primary key column name, e.g. hydroid, pid, ...
#' @param inputs  contents of record to post in list(pid, propname, propvalue, ...)
#' @param site URL of rest server
#' @param token for xhttp auth
#' @seealso NA
#' @export fn_post_rest
#' @examples NA
fn_post_rest <- function(entity_type, pk, inputs, site, token){
  #Search for existing ts matching supplied varkey, featureid, entity_type 
  #print(inputs)
  pkid <- as.integer(as.character(inputs[pk]))
  #message(paste("Called fn_post_rest(", entity_type, ",", pk, ", pkid=", pkid, ")"))
  
  for (j in 1:length(inputs)) {
    if (is.na(inputs[j])) {
      inputs[j] <- NULL
    }
  }
  #message(inputs)
  #message(paste("pk= ", pkid))
  this_result <- list(
    status = FALSE
  )
  if (!is.na(match(pk, names(inputs)) )) {
    #remove this for saving if it is not null
    iix <- which(names(inputs) == pk)
    inputs  <- inputs[-iix]
    # the reason we remove this is that the REST service bombs if we 
    # send it a pk, like tid or pid, and this is unnecessary because 
    # the pkid is part of the URL so no worries.
    #message(paste("removed ", pk, " from inputs"))
  }
  if ( is.na(pkid) | is.null(pkid) ) {
    message(paste0("----- Creating ", entity_type, "..."))
    this_result <- httr::POST(
      paste0(site, "/",entity_type, "/"), 
      httr::add_headers(HTTP_X_CSRF_TOKEN = token),
      body = inputs,
      encode = "json"
    )
    # need to harvest the id col since this is an insert
    rest_parts = httr::content(this_result)
    #print(paste("Parts:", rest_parts))
    pkid = as.integer(rest_parts$id)
  } else {
    message(paste0("----- Updating ", entity_type, "..."))
    #message(paste("PUT URL: ", paste0(site, "/",entity_type, "/", pkid)))
    this_result <- httr::PUT(
      paste0(site, "/",entity_type, "/", pkid), 
      httr::add_headers(HTTP_X_CSRF_TOKEN = token),
      body = inputs,
      encode = "json"
    );
    #print(this_result)
  }
  #rest_parts = strsplit(this_result$url, '/', fixed = TRUE)
  #print(paste("Rest Parts:", rest_parts))
  #pkid = as.integer(rest_parts[[1]][length(rest_parts[[1]])])
  
  if (!is.logical(this_result$status )) {
    return_id <- switch(
      this_result$status,
      "200" = pkid,
      "201" = pkid,
      "400" = FALSE,
      "500" = FALSE
    )
  } else {
    pkid = FALSE
  }
  message(paste("REST returned", pkid))
  return(pkid)
}

#' Get any entity from a RESTful web service
#'
#' @param entity_type = dh_feature, dh_properties, ...
#' @param pk = primary key column name, e.g. hydroid, pid, ...
#' @param inputs  contents of record to get in list(pid, propname, propvalue, ...)
#' @param site URL of rest server
#' @param token for xhttp auth
#' @export fn_get_rest
#' @examples NA
fn_get_rest <- function(entity_type, pk, inputs, site, token){
  #Search for existing ts matching supplied varkey, featureid, entity_type 
  
  pkid <- as.integer(as.character(inputs[pk]))
  
  for (j in 1:length(inputs)) {
    if (is.na(inputs[j])) {
      inputs[j] <- NULL
    }
  }
  if (is.null(inputs$limit)) {
    inputs$limit = 0
  }
  if (!is.null(inputs$page)) {
    multipage = FALSE
  } else {
    page = 0
    inputs$page = 0
    multipage = TRUE ; # do we support multiple pages if records exceed limit?
  }
  #message(inputs)
  #message(paste("pk= ", pkid))
  morepages = TRUE
  entities = FALSE
  ecols = FALSE
  while (morepages == TRUE) {
    entity_rest <- httr::GET(
      paste0(site, "/",entity_type, ".json"), 
      httr::add_headers(HTTP_X_CSRF_TOKEN = token),
      query = inputs, 
      encode = "json"
    );
    #message(entity_rest)
    entity_cont <- httr::content(entity_rest);
    #message(paste0(site, "/",entity_type, ".json"))
    
    if (length(entity_cont$list) != 0) {
      numrecs <- length(entity_cont$list)
      for (i in 1:numrecs) {
        entity <- FALSE
        # set up column names to fetch based on first returned row.
        # this will limit overly broad queries
        if (is.logical(entities)) {
          ecols <- unlist(names(entity_cont$list[[i]]))
        }
        # now capture these entities
        for (j in ecols) {
          if (is.logical(entity)) {
            entity <- rbind(unlist(entity_cont$list[[i]][j]))[1,]
          } else {
            if (is.null(unlist(entity_cont$list[[i]][j]))) {
              entity[j] <- ''
            } else {
              entity[j] <- unlist(entity_cont$list[[i]][j])
            }
          }
        }
        if (is.logical(entities)) {
          entities <- as.data.frame(t(entity))
        } else {
          entities <- rbind(entities, as.data.frame(t(entity)))
        }
      }
      
      trecs <- nrow(entities)
      
      if ( entity_cont$self == entity_cont$last ) {
        morepages = FALSE
      } else {
        morepages = TRUE
        inputs$page = inputs$page + 1
      }
    } else {
      morepages = FALSE
    }
  }
  if (is.logical(entities)) {
    message("----- This entity does not exist")
    entities = FALSE
  } else {
    message(paste("Total =", trecs))
  }
  return(entities)
}

fn_storeprop_vahydro1 = function(site = "http://deq2.bse.vt.edu"){
  # NOT FINISHED - JUST PASTED CODE
  url <- paste(site,"om/remote/setModelData.php?hash=", sep='/');
  message (paste("Setting 7Q10 for element ", id, " run id ", rid, " to ", x7q10 , sep = "") )
  # building the correct url
  ins_url <- paste(url, hash, "&username=", username, "&elementid=", id, "&runid=", rid, "&dataname=7q10&reporting_frequency=single&dataval=", x7q10, "&starttime=1984-10-01&endtime=2005-09-30&temporal_res=water_year", sep = "")  
  #shell.exec(alf_url)  # opening the webpage
  #message(ins_url);
  readLines(ins_url)
}

#' Retrieve TS data from tsvalues style data frame
#'
#' @param config = list(entity_type, featureid, tid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
#' @param tsvalues_tmp data frame to search
#' @param multiplicity uniqueness criteria. default = tstime_singular which is varid + tstime (all are varid singular)
#' @return data frame of tsvalue or FALSE
#' @seealso NA
#' @export fn_search_tsvalues
#' @examples NA
fn_search_tsvalues <- function(config, tsvalues_tmp, multiplicity = 'default') {
  tsvals = FALSE
  where_clause = ""
  number_cols = c("tid", "tsvalue", "featureid")
  tss <- "select * from tsvalues_tmp where "
  #print(paste("Searching for ", config))
  if (!is.null(config$tid)) {
    if (!is.na(config$tid)) {
      where_clause <- paste(
        where_clause,
        "tid = ", config$tid
      )
    }
  } else {
    wand = ""
    wcols = c('tstime', 'varid')
    if ( (multiplicity == 'default') | (is.null(multiplicity)) ) {
      wcols = c('tstime', 'varid')
    }
    # todo: handle other multiplicity modes
    for (i in wcols) {
      if (is.na(config[i])) {
        #message(paste("Skipping NULL", i))
        next
      }
      if (!(i %in% colnames(tsvalues_tmp))) {
        #message(paste0("Skipping ", i))
        next
      }
      if (nchar(where_clause) > 1) {
        wand = "AND"
      }
      if (!is.null(config[i])) {
        where_clause <- paste(
          where_clause, 
          wand, i, "="
        )
        if (is.element(i, number_cols)) {
          where_clause <- paste(
            where_clause, config[i]
          )
        } else {
          where_clause <- paste0(
            where_clause, " '", config[i], "'"
          )
        }
      } 
    }
  }
  if (nchar(where_clause) > 0 ) {
    tss <- paste(tss, where_clause)
    #message(tss)
    tsvals <- sqldf::sqldf(tss)
    if (!nrow(tsvals)) {
      tsvals = FALSE
    }
  }
  return(tsvals)
}


#' Retrieve Property data from propvalues style data frame
#'
#' @param config = list(entity_type, featureid, pid = NULL, varid = NULL, startdate = NULL, enddate = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
#' @param propvalues_tmp data frame to search
#' @param multiplicity uniqueness criteria. default = startdate_singular which is varid + startdate (all are varid singular)
#' @return data frame of propvalue or FALSE
#' @seealso NA
#' @export fn_search_properties
#' @examples NA
fn_search_properties <- function(config, propvalues_tmp, multiplicity = 'default') {
  propvals = FALSE
  where_clause = ""
  number_cols = c("pid", "propvalue", "featureid")
  propsql <- "select * from propvalues_tmp where "
  #print(paste("Searching for ", config))
  if (!is.null(config$pid)) {
    if (!is.na(config$pid)) {
      where_clause <- paste(
        where_clause,
        "pid = ", config$pid
      )
    }
  } else {
    wand = ""
    wcols = c('propname', 'varid', 'featureid', 'startdate', 'enddate', 'entity_type', 'propcode')
    # todo: handle other multiplicity modes
    for (i in wcols) {
      # check for null first since is.na will give an error if the column does not exist.
      if (is.null(config[[i]])) {
        #message(paste("Skipping NULL", i))
        next
      }
      if (is.na(config[[i]])) {
        #message(paste("Skipping NA", i))
        next
      }
      if (!(i %in% colnames(propvalues_tmp))) {
        #message(paste0("Skipping ", i))
        next
      }
      if (nchar(where_clause) > 1) {
        wand = "AND"
      }
      if (!is.null(config[i])) {
        where_clause <- paste(
          where_clause, 
          wand, i, "="
        )
        if (is.element(i, number_cols)) {
          where_clause <- paste(
            where_clause, config[i]
          )
        } else {
          where_clause <- paste0(
            where_clause, " '", config[i], "'"
          )
        }
      } 
    }
  }
  if (nchar(where_clause) > 0 ) {
    propsql <- paste(propsql, where_clause)
    #message(propsql)
    propvals <- sqldf::sqldf(propsql)
    if (!nrow(propvals)) {
      propvals = FALSE
    }
  }
  return(propvals)
}

#' @title fn_search_vardefs
#' @name fn_search_vardefs
#' @description Retrieve variable or property data from
#'   RomDataSource$propvalues() or RomProperty$propvalues() style data frame
#'   provided by user. Original intended use is to search local variable
#'   definitions for a provided hydroid or varkey, but may apply to any
#'   propvalues() style dataframe that has a hydroid primary key field
#' @param config A list that contains either a target dh_variabledefinition
#'   hydroid or varkey. The varkey is given preference. Hydroids from other
#'   tables (dh_properties) may be provided but user must provide appropriate
#'   table in var_defs_tmp
#' Definitions for these items are defined in the hydrotools readme.
#' @param var_defs_tmp data frame to search, often those of variable definitions
#'   with hydroid and varkeys at least, but may also be a similar table from
#'   dh_properties or other table maybe generated through
#'   RomProperty$propvalues()
#' @return data frame subset from var_defs_tmp based on the hydroid or varkey
#'   provided by user. Returns FALSE if nothing found
#' @export fn_search_vardefs
#' @examples NA
#'#ds <- RomDataSource$new()
#'#fn_search_vardefs(config = list(hydroid = 1456),)
fn_search_vardefs <- function(config, var_defs_tmp) {
  #Function return false if no variable found, meaning nothing found in
  #var_defs_tmp on a RomDataSource instance or similar object. Usually indicates
  #the variable must be retrieved from remote databse (dbase2/3). If any variable
  #information for this ID is in var_defs_tmp, return it. Else return FALSE.
  #Note that varkey is given priority if somehow both varid and hydroid exist
  vardef = FALSE
  #If a variable hydroid has been provided, search for definitions
  if (!is.null(config$hydroid)) {
    vardef = var_defs_tmp[var_defs_tmp$hydroid == config$hydroid,]
    if (nrow(vardef) == 0) { vardef = FALSE }
  }
  #If a variable varkey has been provided, search for definitions
  if (!is.null(config$varkey)) {
    vardef = var_defs_tmp[var_defs_tmp$varkey == config$varkey,]
    if (nrow(vardef) == 0) { vardef = FALSE }
  }
  return(vardef)
}


#' Delete any entity from a RESTful web service
#'
#' @param entity_type = dh_feature, dh_properties, ...
#' @param pk = primary key column name, e.g. hydroid, pid, ...
#' @param inputs  contents of record to get in list(pid, propname, propvalue, ...)
#' @param site URL of rest server
#' @param token for xhttp auth
fn_delete_rest <- function(entity_type, pk, inputs, site, token){
  # not yet 
  result = FALSE
  return(result)
}


#' Retrieve features data from dh_feature style data frame
#'
#' @param config = list(entity_type, featureid, pid = NULL, varid = NULL, startdate = NULL, enddate = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
#' @param features_tmp data frame to search
#' @param multiplicity uniqueness criteria. default = startdate_singular which is varid + startdate (all are varid singular)
#' @return data frame of propvalue or FALSE
#' @seealso NA
#' @export fn_search_features
#' @examples NA
fn_search_features <- function(config, features_tmp, multiplicity = 'default') {
  features = FALSE
  where_clause = ""
  number_cols = c("hydroid")
  tmp_sql <- "select * from features_tmp where "
  #print(paste("Searching for ", config))
  if (!is.null(config$hydroid)) {
    if (!is.na(config$hydroid)) {
      where_clause <- paste(
        where_clause,
        "hydroid = ", config$hydroid
      )
    }
  } else {
    wand = ""
    wcols = c('name', 'hydrocode', 'bundle', 'ftype')
    # todo: handle other multiplicity modes
    for (i in wcols) {
      # check for null first since is.na will give an error if the column does not exist.
      if (is.null(config[[i]])) {
        #message(paste("Skipping NULL", i))
        next
      }
      if (is.na(config[[i]])) {
        #message(paste("Skipping NA", i))
        next
      }
      if (!(i %in% colnames(features_tmp))) {
        #message(paste0("Skipping ", i))
        next
      }
      if (nchar(where_clause) > 1) {
        wand = "AND"
      }
      if (!is.null(config[i])) {
        where_clause <- paste(
          where_clause, 
          wand, i, "="
        )
        if (is.element(i, number_cols)) {
          where_clause <- paste(
            where_clause, config[i]
          )
        } else {
          where_clause <- paste0(
            where_clause, " '", config[i], "'"
          )
        }
      } 
    }
  }
  if (nchar(where_clause) > 0 ) {
    tmp_sql <- paste(tmp_sql, where_clause)
    #message(tmp_sql)
    features <- sqldf::sqldf(tmp_sql)
    if (!nrow(features)) {
      features = FALSE
    }
  }
  return(features)
}



#' Send a property to VAHydro with a simple one line command
#'
#' @param pid The unique property ID
#' @param varkey The variable key for the property
#' @param propcode The propcode, null OK
#' @param propname The property name - must be non-null
#' @param propvalue The numerical value
#' @param ds A valid Rom datasource
#' @return A RomProperty object
#' @seealso NA
#' @export vahydro_post_metric_to_scenprop
#' @examples NA
vahydro_post_metric_to_scenprop <- function(pid, varkey, propcode, propname, propvalue, ds = FALSE) {
  if (is.logical(ds)) {
    stop("Error: This function has been modified to require a ds (RomDataSource) argument.")
  }
  if (is.null(propvalue)) {
    propvalue <- 0
  }
  if (is.null(propcode)) {
    propcode <- ''
  }
  # first try to load a prop with just name, entity_type, featureid
  # in case there are varid mismatches
  metcheck <- ds$get_prop(
    list(propname=propname, featureid = as.integer(pid),
    entity_type = "dh_properties")
  )
  if (!is.logical(metcheck) && nrow(metcheck) > 0) {
    metprop <- RomProperty$new( ds, list(pid=metcheck[1,]$pid), TRUE)
  } else {
    metinfo <- list(
      varkey = varkey,
      propname = propname,
      featureid = as.integer(pid),
      entity_type = "dh_properties",
      bundle = "dh_properties"
    )
    metprop <- RomProperty$new( ds, metinfo, TRUE)
  }
  metprop$propcode <- propcode
  metprop$varid <- as.integer(ds$get_vardef(varkey)$hydroid)
  metprop$propvalue <- as.numeric(propvalue)
  metprop$save(TRUE)
}
