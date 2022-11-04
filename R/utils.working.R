
fn_get_properties <- function(inputs, site, token){
  print(inputs)
  #Convert varkey to varid - needed for REST operations 
  if (!is.null(inputs$varkey)) {
    vardef <- fn_get_vardef_view(inputs$varkey, site, token)
    varid <- vardef$varid
    if (!varid) {
      # we got a bad variable id so we should return FALSE
      return(FALSE)
    }
    inputs$varid = varid
  }
  # now, verify that we have either a proper varid OR a propname OR a featureid
  if (is.null(inputs$varid) & is.null(inputs$propname) & is.null(inputs$featureid)) {
    # we were sent a bad variable id so we should return FALSE
    if(is.null(inputs$pid)) {
      return(FALSE)
    } 
  }
  
  pbody = list(
    #bundle = 'dh_properties',
    featureid = inputs$featureid,
    entity_type = inputs$entity_type 
  );
  if (!is.null(inputs$varid)) {
    pbody$varid = inputs$varid
  }
  if (!is.null(inputs$bundle)) {
    pbody$bundle = inputs$bundle
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
  }
  
  prop <- data.frame(
    proptext=character(),
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
    field_dh_matrix=character(),
    stringsAsFactors=FALSE
  ) 
  # set morepages to true to start, if multipage = FALSE, this gets reset immediately after 1st retrieval
  morepages = TRUE
  while (morepages == TRUE) {
    prop_rest <- GET(
      paste(site,"/dh_properties.json",sep=""), 
      add_headers(HTTP_X_CSRF_TOKEN = token),
      query = pbody, 
      encode = "json"
    );
    prop_cont <- content(prop_rest);
    if (length(prop_cont$list) != 0) {
      print(paste("Number of properties found: ",length(prop_cont$list),sep=""))
      
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
          "field_dh_matrix" = "",
          "status" = if (is.null(prop_cont$list[[i]]$status)){""} else {prop_cont$list[[i]]$status},
          "module" = if (is.null(prop_cont$list[[i]]$module)){""} else {prop_cont$list[[i]]$module},
          stringsAsFactors=FALSE
        )
        # handle data_matrix
        if (!is.null(prop_cont$list[[i]]$field_dh_matrix$value)) {
          dfl = prop_cont$list[[i]]$field_dh_matrix$value
          df <- data.frame(matrix(unlist(dfl), nrow=length(dfl), byrow=T))
          prop_i$field_dh_matrix <- jsonlite::serializeJSON(df);
        }
        prop  <- rbind(prop, prop_i)
      }
      precs <- length(prop[,1])
      if (precs >= pbody$limit) {
        morepages = FALSE
      } else {
        morepages = TRUE
        pbody$page = pbody$page + 1
      }
    } else {
      precs <- length(prop[,1])
      if (precs == 0) {
        message("----- This property does not exist")
        prop = FALSE
      } else {
        message(paste("Total =", precs))
        morepages=FALSE
      }
    }
  }
  return(prop)
}
