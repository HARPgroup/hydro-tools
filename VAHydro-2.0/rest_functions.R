library(httr);
library(stringr);
library(RCurl); #required for limiting connection timeout in vahydro_fe_data_icthy()

rest_token <- function(base_url, token, rest_uname = FALSE, rest_pw = FALSE) {
  #Cross-site Request Forgery Protection (Token required for POST and PUT operations)
  csrf_url <- paste(base_url,"restws/session/token/",sep="/");
  
  #IF THE OBJECTS 'rest_uname' or 'rest_pw' DONT EXIST, USER INPUT REQUIRED
  if (!is.character(rest_uname) | !(is.character(rest_pw))){
    
    rest_uname <- c() #initialize username object
    rest_pw <- c()    #initialize password object
    
    #currently set up to allow infinite login attempts, but this can easily be restricted to a set # of attempts
    token <- c("rest_uname","rest_pw") #used in while loop below, "length of 2"
    login_attempts <- 1
    if (!is.character(rest_uname)) {
      print(paste("REST AUTH INFO MUST BE SUPPLIED",sep=""))
      while(length(token) == 2  && login_attempts <= 5){
        print(paste("login attempt #",login_attempts,sep=""))
        
        rest_uname <- readline(prompt="Enter REST user name: ")
        rest_pw <- readline(prompt="Password: ")
        csrf <- GET(url=csrf_url,authenticate(rest_uname,rest_pw));
        token <- content(csrf);
        #print(token)
        
        if (length(token)==2){
          print("Sorry, unrecognized username or password")
        }
        login_attempts <- login_attempts + 1
      }
      if (login_attempts > 5){print(paste("ALLOWABLE NUMBER OF LOGIN ATTEMPTS EXCEEDED"))}
    }
    
  } else {
    print(paste("REST AUTH INFO HAS BEEN SUPPLIED",sep=""))
    print(paste("RETRIEVING REST TOKEN",sep=""))
    csrf <- GET(url=csrf_url,authenticate(rest_uname,rest_pw));
    token <- content(csrf);
  }
  
  if (length(token)==1){
    print("Login attempt successful")
    print(paste("token = ",token,sep=""))
  } else {
    print("Login attempt unsuccessful")
  }
  token <- token
} #close function


getTimeseries <- function(inputs, base_url, ts){

  #Convert varkey to varid - needed for REST operations 
  if (!is.null(inputs$varkey)) {
    # this would use REST 
    # getVarDef(list(varkey = inputs$varkey), token, base_url)
    # but it is broken for vardef for now metadatawrapper fatal error
    # EntityMetadataWrapperException: Invalid data value given. Be sure it matches the required data type and format. 
    # in EntityDrupalWrapper->set() 
    # (line 736 of /var/www/html/d.dh/modules/entity/includes/entity.wrapper.inc).
    
    tsdef_url<- paste(base_url,"/?q=vardefs.tsv/",inputs$varkey,sep="")
    tsdef_table <- read.table(tsdef_url,header = TRUE, sep = "\t")    
    varid <- tsdef_table[1][which(tsdef_table$varkey == inputs$varkey),]
    print(paste("varid: ",varid,sep=""))
    if (is.null(varid)) {
      # we sent a bad variable id so we should return FALSE
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
    if (inputs$tid > 0) {
      # forget about other attributes, just use tid
      pbody = list(
        tid = inputs$tid
      )
    }
  }
  
  ts <- GET(
    paste(base_url,"/dh_timeseries.json",sep=""), 
    add_headers(HTTP_X_CSRF_TOKEN = token),
    query = pbody, 
    encode = "json"
  );
  ts_cont <- content(ts);
  
  if (length(ts_cont$list) != 0) {
    print(paste("Number of timeseries found: ",length(ts_cont$list),sep=""))
    
    ts <- data.frame(
                       tid=character(),
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
    
    i <- 1
    for (i in 1:length(ts_cont$list)) {
      
      ts_i <- data.frame(  "tid" = if (is.null(ts_cont$list[[i]]$tid)){""} else {ts_cont$list[[i]]$tid},
                           "tsvalue" = if (is.null(ts_cont$list[[i]]$tsvalue)){""} else {ts_cont$list[[i]]$tsvalue},
                           "tscode" = if (is.null(ts_cont$list[[i]]$tscode)){""} else {ts_cont$list[[i]]$tscode},
                           "tstime" = if (is.null(ts_cont$list[[i]]$tstime)){""} else {ts_cont$list[[i]]$tstime},
                           "tsendtime" = if (is.null(ts_cont$list[[i]]$tsendtime)){""} else {ts_cont$list[[i]]$tsendtime},
                           "featureid" = if (is.null(ts_cont$list[[i]]$featureid)){""} else {ts_cont$list[[i]]$featureid},
                           "modified" = if (is.null(ts_cont$list[[i]]$modified)){""} else {ts_cont$list[[i]]$modified},
                           "entity_type" = if (is.null(ts_cont$list[[i]]$entity_type)){""} else {ts_cont$list[[i]]$entity_type},
                           "varid" = if (is.null(ts_cont$list[[i]]$varid)){""} else {ts_cont$list[[i]]$varid},
                           "uid" = if (is.null(ts_cont$list[[i]]$uid)){""} else {ts_cont$list[[i]]$uid},
                           "status" = if (is.null(ts_cont$list[[i]]$status)){""} else {ts_cont$list[[i]]$status}
      )
      ts  <- rbind(ts, ts_i)
    }
  } else {
    print("This timeseries does not exist")
    return(FALSE)
  }
  ts <- ts
}

postTimeseries <- function(inputs, base_url, ts){
  
  #Search for existing tserty matching supplied varkey, featureid, entity_type 
  dataframe <- getTimeseries(inputs, base_url, ts)
  if (is.data.frame(dataframe)) {
    tid <- as.character(dataframe$tid)
  } else {
    tid = NULL
  }
  if (!is.null(inputs$varkey)) {
    # this would use REST 
    # getVarDef(list(varkey = inputs$varkey), token, base_url)
    # but it is broken for vardef for now metadatawrapper fatal error
    # EntityMetadataWrapperException: Invalid data value given. Be sure it matches the required data type and format. 
    # in EntityDrupalWrapper->set() 
    # (line 736 of /var/www/html/d.dh/modules/entity/includes/entity.wrapper.inc).
    
    tsdef_url<- paste(base_url,"/?q=vardefs.tsv/",inputs$varkey,sep="")
    tsdef_table <- read.table(tsdef_url,header = TRUE, sep = "\t")    
    varid <- tsdef_table[1][which(tsdef_table$varkey == inputs$varkey),]
    print(paste("varid: ",varid,sep=""))
    if (is.null(varid)) {
      # we sent a bad variable id so we should return FALSE
      return(FALSE)
    }
  }
  if (!is.null(inputs$varid)) {
    varid = inputs$varid
  }
  
  if (is.null(varid)) {
    print("Variable IS is null - returning.")
    return(FALSE)
  }
  
  pbody = list(
    featureid = inputs$featureid,
    varid = varid,
    entity_type = inputs$entity_type,
    tsvalue = inputs$tsvalue,
    tscode = inputs$tscode,
    tstime = inputs$tstime,
    tsendtime = inputs$tsendtime
  );
  
  if (is.null(tid)){
    print("Creating timeseries...")
    ts <- POST(paste(base_url,"/dh_timeseries/",sep=""), 
                 add_headers(HTTP_X_CSRF_TOKEN = token),
                 body = pbody,
                 encode = "json"
    );
    if (ts$status == 201){ts <- paste("Status ",ts$status,", timeseries Created Successfully",sep="")
    } else {ts <- paste("Status ",ts$status,", Error: timeseries Not Created Successfully",sep="")}
    
  } else if (length(dataframe$tid) == 1){
    print("Single timeseries Exists, Updating...")
    ts <- PUT(paste(base_url,"/dh_timeseries/",tid,sep=""), 
                add_headers(HTTP_X_CSRF_TOKEN = token),
                body = pbody,
                encode = "json"
    );
    if (ts$status == 200){ts <- paste("Status ",ts$status,", timeseries Updated Successfully",sep="")
    } else {ts <- paste("Status ",ts$status,", Error: timeseries Not Updated Successfully",sep="")}
  } else {
    ts <- print("Multiple timeseries Exist, Execution Halted")
  }
  
}

getProperty <- function(inputs, base_url, prop){
  print(inputs)
  #Convert varkey to varid - needed for REST operations 
  if (!is.null(inputs$varkey)) {
    # this would use REST 
    # getVarDef(list(varkey = inputs$varkey), token, base_url)
    # but it is broken for vardef for now metadatawrapper fatal error
    # EntityMetadataWrapperException: Invalid data value given. Be sure it matches the required data type and format. 
    # in EntityDrupalWrapper->set() 
    # (line 736 of /var/www/html/d.dh/modules/entity/includes/entity.wrapper.inc).
    
    propdef_url<- paste(base_url,"/?q=vardefs.tsv/",inputs$varkey,sep="")
    print(paste("Trying", propdef_url))
    propdef_table <- read.table(propdef_url,header = TRUE, sep = "\t")    
    varid <- propdef_table[1][which(propdef_table$varkey == inputs$varkey),]
    print(paste("varid: ",varid,sep=""))
    if (is.null(varid)) {
      # we sent a bad variable id so we should return FALSE
      return(FALSE)
    }
    inputs$varid = varid
  }
  # now, verify that we have either a proper varid OR a propname
  if (is.null(inputs$varid) & is.null(inputs$propname)) {
    # we were sent a bad variable id so we should return FALSE
    return(FALSE)
  }
  
  pbody = list(
    bundle = 'dh_properties',
    featureid = inputs$featureid,
    entity_type = inputs$entity_type 
  );
  if (!is.null(inputs$varid)) {
    pbody$varid = inputs$varid
  }
  if (!is.null(inputs$propcode)) {
    pbody$propcode = inputs$propcode
  }
  if (!is.null(inputs$propname)) {
    pbody$propname = inputs$propname
  }
  if (!is.null(inputs$pid)) {
    if (inputs$pid > 0) {
      # forget about other attributes, just use pid
      pbody = list(
        pid = inputs$pid
      )
    }
  }
  
  prop <- GET(
    paste(base_url,"/dh_properties.json",sep=""), 
    add_headers(HTTP_X_CSRF_TOKEN = token),
    query = pbody, 
    encode = "json"
  );
  prop_cont <- content(prop);
  
  if (length(prop_cont$list) != 0) {
    print(paste("Number of properties found: ",length(prop_cont$list),sep=""))
    
    prop <- data.frame(proptext=character(),
                       pid=character(),
                       propname=character(), 
                       propvalue=character(),
                       propcode=character(),
                       startdate=character(),
                       enddate=character(),
                       featureid=character(),
                       modified=character(),
                       entity_type=character(),
                       bundle=character(),
                       varid=character(),
                       uid=character(),
                       vid=character(),
                       status=character(),
                       module=character(),
                       stringsAsFactors=FALSE) 
    
    i <- 1
    for (i in 1:length(prop_cont$list)) {
      
      prop_i <- data.frame(
        "proptext" = if (is.null(prop_cont$list[[i]]$proptext)){""} else {prop_cont$list[[i]]$proptext},
        "pid" = if (is.null(prop_cont$list[[i]]$pid)){""} else {as.integer(prop_cont$list[[i]]$pid)},
        "propname" = if (is.null(prop_cont$list[[i]]$propname)){""} else {prop_cont$list[[i]]$propname},
        "propvalue" = if (is.null(prop_cont$list[[i]]$propvalue)){""} else {as.numeric(prop_cont$list[[i]]$propvalue)},
        "propcode" = if (is.null(prop_cont$list[[i]]$propcode)){""} else {prop_cont$list[[i]]$propcode},
        "startdate" = if (is.null(prop_cont$list[[i]]$startdate)){""} else {prop_cont$list[[i]]$startdate},
        "enddate" = if (is.null(prop_cont$list[[i]]$enddate)){""} else {prop_cont$list[[i]]$enddate},
        "featureid" = if (is.null(prop_cont$list[[i]]$featureid)){""} else {prop_cont$list[[i]]$featureid},
        "modified" = if (is.null(prop_cont$list[[i]]$modified)){""} else {prop_cont$list[[i]]$modified},
        "entity_type" = if (is.null(prop_cont$list[[i]]$entity_type)){""} else {prop_cont$list[[i]]$entity_type},
        "bundle" = if (is.null(prop_cont$list[[i]]$bundle)){""} else {prop_cont$list[[i]]$bundle},
        "varid" = if (is.null(prop_cont$list[[i]]$varid)){""} else {prop_cont$list[[i]]$varid},
        "uid" = if (is.null(prop_cont$list[[i]]$uid)){""} else {prop_cont$list[[i]]$uid},
        "vid" = if (is.null(prop_cont$list[[i]]$vid)){""} else {prop_cont$list[[i]]$vid},
        "status" = if (is.null(prop_cont$list[[i]]$status)){""} else {prop_cont$list[[i]]$status},
        "module" = if (is.null(prop_cont$list[[i]]$module)){""} else {prop_cont$list[[i]]$module},
        stringsAsFactors=FALSE
      )
      prop  <- rbind(prop, prop_i)
    }
  } else {
    print("This property does not exist")
    return(FALSE)
  }
  prop <- prop
}


postProperty <- function(inputs,fxn_locations,base_url,prop){
  
  #Search for existing property matching supplied varkey, featureid, entity_type 
  dataframe <- getProperty(inputs, base_url, prop)
  if (is.data.frame(dataframe)) {
    pid <- as.character(dataframe$pid)
  } else {
    pid = NULL
  }
  if (!is.null(inputs$varkey)) {
    # this would use REST 
    # getVarDef(list(varkey = inputs$varkey), token, base_url)
    # but it is broken for vardef for now metadatawrapper fatal error
    # EntityMetadataWrapperException: Invalid data value given. Be sure it matches the required data type and format. 
    # in EntityDrupalWrapper->set() 
    # (line 736 of /var/www/html/d.dh/modules/entity/includes/entity.wrapper.inc).
    
    propdef_url<- paste(base_url,"/?q=vardefs.tsv/",inputs$varkey,sep="")
    propdef_table <- read.table(propdef_url,header = TRUE, sep = "\t")    
    varid <- propdef_table[1][which(propdef_table$varkey == inputs$varkey),]
    print(paste("varid: ",varid,sep=""))
    if (is.null(varid)) {
      # we sent a bad variable id so we should return FALSE
      return(FALSE)
    }
  }
  if (!is.null(inputs$varid)) {
    varid = inputs$varid
  }
  
  if (is.null(varid)) {
    print("Variable IS is null - returning.")
    return(FALSE)
  }
  
  pbody = list(
    bundle = 'dh_properties',
    featureid = inputs$featureid,
    varid = varid,
    entity_type = inputs$entity_type,
    proptext = inputs$proptext,
    propvalue = inputs$propvalue,
    propcode = inputs$propcode,
    startdate = inputs$startdate,
    propname = inputs$propname,
    enddate = inputs$enddate
  );
  
  if (is.null(pid)){
    print("Creating Property...")
    prop <- POST(paste(base_url,"/dh_properties/",sep=""), 
                 add_headers(HTTP_X_CSRF_TOKEN = token),
                 body = pbody,
                 encode = "json"
    );
    if (prop$status == 201){prop <- paste("Status ",prop$status,", Property Created Successfully",sep="")
    } else {prop <- paste("Status ",prop$status,", Error: Property Not Created Successfully",sep="")}
    
  } else if (length(dataframe$pid) == 1){
    print("Single Property Exists, Updating...")
    print(paste("Posting", pbody$varid )) 
    print(pbody) 
    #pbody$pid = pid
    prop <- PUT(paste(base_url,"/dh_properties/",pid,sep=""), 
                add_headers(HTTP_X_CSRF_TOKEN = token),
                body = pbody,
                encode = "json"
    );
    if (prop$status == 200){prop <- paste("Status ",prop$status,", Property Updated Successfully",sep="")
    } else {prop <- paste("Status ",prop$status,", Error: Property Not Updated Successfully",sep="")}
  } else {
    prop <- print("Multiple Properties Exist, Execution Halted")
  }
  
}

getVarDef <- function(inputs, token, base_url, vardef){
  
  pbody = list(
  );
  if (!is.null(inputs$hydroid)) {
    pbody$hydroid = inputs$hydroid;
  }
  if (!is.null(inputs$varname)) {
    pbody$varname = inputs$varname;
  }
  if (!is.null(inputs$varkey)) {
    pbody$varkey = inputs$varkey;
  }
  if (!is.null(inputs$varcode)) {
    pbody$varcode = inputs$varcode;
  }
  if (!is.null(inputs$varunits)) {
    pbody$varunits = inputs$varunits;
  }
  if (!is.null(inputs$vocabulary)) {
    pbody$vocabulary = inputs$vocabulary;
  }
  if (!is.null(inputs$vardesc)) {
    pbody$vardesc = inputs$vardesc;
  }
  
  vardef <- GET(
    paste(base_url,"/dh_variabledefinition.json",sep=""), 
    add_headers(HTTP_X_CSRF_TOKEN = token),
    query = pbody, 
    encode = "json"
  );
  vardef_cont <- content(vardef);
  print(vardef)
  
  if (length(vardef_cont$list) != 0) {
    print(paste("Number of variables found: ",length(vardef_cont$list),sep=""))
    
    vardef <- data.frame(
      hydroid=character(),
      varname=character(),
      varcode=character(),
      varkey=character(), 
      vardesc=character(),
      varunits=character(),
      vocabulary=character(),
      stringsAsFactors=FALSE
    ) 
    
    i <- 1
    for (i in 1:length(vardef_cont$list)) {
      
      vardef_i <- data.frame(
        hydroid = vardef_cont$list[[i]]$hydroid,
        varname = vardef_cont$list[[i]]$varname,
        varcode = vardef_cont$list[[i]]$varcode,
        varkey = vardef_cont$list[[i]]$varkey,
        vardesc = vardef_cont$list[[i]]$vardesc,
        varunits = vardef_cont$list[[i]]$varunits,
        vocabulary = vardef_cont$list[[i]]$vocabulary
      )
      vardef <- rbind(vardef, vardef_i)
    }
  } else {
    print("This variable does not exist")
    return(FALSE)
  }
  vardef <- vardef
  return(vardef)
}


getFeature <- function(inputs, token, base_url, feature){
  
  pbody = list(
  );
  if (!is.null(inputs$bundle)) {
    pbody$bundle = inputs$bundle;
  }
  if (!is.null(inputs$hydrocode)) {
    pbody$hydrocode = inputs$hydrocode;
  }
  if (!is.null(inputs$ftype)) {
    pbody$ftype = inputs$ftype;
  }
  if (!is.null(inputs$hydroid)) {
    pbody$hydroid = inputs$hydroid;
  }
  if (!is.null(inputs$name)) {
    pbody$name = inputs$name;
  }
  
  feature <- GET(
    paste(base_url,"/dh_feature.json",sep=""), 
    add_headers(HTTP_X_CSRF_TOKEN = token),
    query = pbody, 
    encode = "json"
  );
  feature_cont <- content(feature);
  #print(feature_cont)
  
  if (length(feature_cont$list) != 0) {
    print(paste("Number of features found: ",length(feature_cont$list),sep=""))
    
    feature <- data.frame(
      hydroid=character(),
      name=character(),
      bundle=character(),
      hydrocode=character(), 
      ftype=character(),
      geom=character(),
      stringsAsFactors=FALSE
    ) 
    
    #i <- 1
    for (i in 1:length(feature_cont$list)) {
      
      feature_i <- data.frame(
        hydroid = feature_cont$list[[i]]$hydroid,
        name = feature_cont$list[[i]]$name,
        bundle = feature_cont$list[[i]]$bundle,
        hydrocode = feature_cont$list[[i]]$hydrocode,
        ftype = feature_cont$list[[i]]$ftype,
        geom = feature_cont$list[[i]]$dh_geofield$geom
      )
      feature <- rbind(feature, feature_i)
    }
  } else {
    print("This Feature does not exist")
  }
  feature <- feature
}

vahydro_fe_multi_data <- function (
  bundle = 'watershed', 
  ftype = 'nhd_huc8',
  metric = 'aqbio_nt_total',
  selected = 'all',
  datasite = ''
) {
  if (datasite == '') {
    if (site == '' ) {
      datasite = 'http://deq1.bse.vt.edu/d.dh'
    } else {
      datasite <- site
    }
    
  }
  # load data for select ecoregion
  # subvfiew can be max or allmax - allmax does not filter
  if ( (selected == 'all') | (selected == '')) {
    subview = 'allmax'
  } else {
    subview = 'max'
  }
  uri <- paste(datasite, "multivariate", subview, metric, bundle, ftype, selected, sep='/') 
  print(paste('uri: ', uri, sep = '')) 
  data <- read.csv(uri, header = TRUE, sep = ",")
  #makes sure all metric values are numeric and not factorial (fixes error with ni, total)
  data$metric_value <- as.numeric(data$metric_value)
  return(data )
}

vahydro_fe_data <- function (
  Watershed_Hydrocode,x_metric_code,
  y_metric_code,bundle,ws_ftype_code,sampres, data, datasite = '') {
  if (datasite == '') {
    if (site == '' ) {
      datasite = 'http://deq1.bse.vt.edu/d.dh'
    } else {
      datasite <- site
    }
    
  }
  #note: add a 0 for the HUC6's or else the url doesn't work
  search_code <- Watershed_Hydrocode;
  if (ws_ftype_code == 'nhd_huc6') {
    search_code <- str_pad(Watershed_Hydrocode, 6, "left", pad = "0");
  }
  if (ws_ftype_code == 'nhd_huc10') {
    search_code <- str_pad(Watershed_Hydrocode, 10, "left", pad = "0");
  }
  
  uri <- paste(
    datasite,"elfgen_data_export",x_metric_code,y_metric_code,
    bundle,ws_ftype_code,sampres,search_code,sep="/"
  )
  print(paste("Using ", uri, sep=''));
  data <- read.csv(uri, header = TRUE, sep = ",")
}

vahydro_fe_data_icthy <- function (Watershed_Hydrocode,x_metric_code,y_metric_code,bundle,ws_ftype_code,sampres, data, datasite = '') {
  if (datasite == '') {
    if (site == '' ) {
      datasite = 'http://deq1.bse.vt.edu/d.dh'
    } else {
      datasite <- site
    }
    
  }
  #note: add a 0 for the HUC6's or else the url doesn't work
  search_code <- Watershed_Hydrocode;
  if (ws_ftype_code == 'nhd_huc6') {
    search_code <- str_pad(Watershed_Hydrocode, 6, "left", pad = "0");
  }
  if (ws_ftype_code == 'nhd_huc10') {
    search_code <- str_pad(Watershed_Hydrocode, 10, "left", pad = "0");
  }
  
  uri <- paste(
    datasite,"elfgen_data_export_sample_event",x_metric_code,"aqbio_nt_total",
    bundle,ws_ftype_code,"species",search_code,sep="/"
  )
  print(paste("Using ", uri, sep=''));
  myOpts <- curlOptions(connecttimeout = 200)
  data <- getURL(uri, .opts = myOpts)
  data <- read.csv(textConnection(data))
  
}

vahydro_prop_matrix <- function (featureid,varkey, datasite = '') {
  if (datasite == '') {
    if (site == '' ) {
      datasite = 'http://deq1.bse.vt.edu/d.dh'
    } else {
      datasite <- site
    }
    
  }
  library(jsonlite) #required for transforming json data to dataframe format 
  library(dplyr) #required for renaming dataframe columns 
  
  #featureid <- '397299'
  #varkey <- 'ifim_habitat_table'
  matrix_url <- paste(site,"dh-properties-json/dh_feature",featureid,varkey, sep="/")
  
  print(paste("Using ", matrix_url, sep=''));
  
  raw_data <- fromJSON(matrix_url) #return entire property 
  prop_matrix_json <- raw_data$entity_properties$property$prop_matrix #return property prop_matrix only 
  json_file <- fromJSON(prop_matrix_json) #convert to json list 
  
  #unlist json objects 
  json_file <- lapply(json_file, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  matrix_dataframe <- do.call("rbind", json_file) #bind objects into rows 
  matrix_dataframe <- data.frame(matrix_dataframe) #convert rows of objects to dataframe 
  
  #transform all dataframe values to numeric 
  for (z in 1:length(matrix_dataframe)) {
    matrix_dataframe[,z] <-as.numeric(as.character(matrix_dataframe[,z]))
  }
  
  matrix_dataframe <- matrix_dataframe #return dataframe object
}