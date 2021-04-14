#' Global object store holds references to all objects of a given domain
#' and pointers to remote data source if applicable.
#' if remote data source is set, all data can be synched back to it
#' no mixing of data sources is allowed
#' @description Data Source containing tables and methods of Model data objects
#' @details Provides integrated, queryable universe of features, observations and meta data
#' @importFrom R6 R6Class  
#' @param baseurl URL of remote data store with REST capabilities, optional
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export RomDataSource
RomDataSource <- R6Class(
  "RomDataSource",
  # Note: do not document private fields, since they are private
  private = list(
    token = NULL # rest token if remote rest source is used
  ),
  public = list(
    #' @field site URL of some RESTful repository
    site = NULL,
    #' @param site URL of some RESTful repository
    #' @return object instance
    initialize = function(site) {
      self$site = site
    },
    #' @return nothing sets internal private token
    get_token = function() {
      if (!is.character(self$site) ) {
        warning("Base URL to REST repository not supplied.")
      }
      private$token <- om_vahydro_token(self$site)
    },
    # this could actually live in the RomTS object
    #' @param varkey = variable key
    #' @param force_update Check remote repository for new info? 
    #' @param debug show info
    #' @return nothing sets internal private token
    get_vardef = function(varkey, force_update = FALSE, debug = FALSE) {
      # NOt yet tested,
      # check local store, if not there, check remote
      var_def <- fn_search_vardefs(config, self$var_defs)
      if (is.logical(var_def)) {
        # none exists locally, so query
        force_refresh = TRUE
      }
      if (!is.null(self$site) & force_refresh) {
        var_def <- fn_get_vardef_view(varkey, self$site, private$token, debug)
        # TBD
        # var_def <- RomVarDef$new(self,var_one)
        # var_def <- var_def$to_list()
         self$set_vardef(var_def)
      } else {
        # TBD
        #var_def <- RomVarDef$new(self, config)
        #var_def <- var_def$to_list()
        #self$set_vardef(ts)
      }
      # after retrieval, store locally
      return(var_def)
    },
    # get properties
    #' @param config = list(entity_type, featureid, tid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
    #' @param return_type 'df' (data.frame) or 'object'
    #' @param force_refresh if this ds has a remote source, whether to pull anew
    #' @return nothing sets internal private token
    get_props = function(config, return_type = 'df', force_refresh = FALSE) {
      props <- fn_get_properties(config, self$site, private$token)
      return(props)
    },
    # need get_ts - get data frame of ts values matching criteria
    # load_object - load entity single object config
    # get_ts method description
    # this could actually live in the RomTS object
    #' @param config = list(entity_type, featureid, tid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
    #' @param return_type 'df' (data.frame) or 'object'
    #' @param force_refresh if this ds has a remote source, whether to pull anew
    #' @return nothing sets internal private token
    get_ts = function(config, return_type = 'data.frame', force_refresh = FALSE) {
      # return_type = 'data.frame' or 'object', default to list to maintain
      # force_refresh = if FALSE, use local value if we already have one
      #    easier backwards compatibility, lower data needs?
      # if returning object, only take first value? 
      # or, return FALSE with message that df is only option? Hmmmm.
      # or return 
      # search first in 
      tsvalues <- fn_search_tsvalues(config, self$tsvalues)
      if (is.logical(tsvalues)) {
        # none exists locally, so query
        force_refresh = TRUE
      }
      if (!is.null(self$site) & force_refresh) {
        ts_vals <- fn_get_timeseries(config, self$site, private$token)
        for (i in nrow(ts_vals)) {
          print(paste("handling ts", i))
          tsi <- as.list(ts_vals[i,])
          tsi <- RomTS$new(self,tsi)
          if (return_type == 'object') {
            ts <- tsi
          } 
          tsi <- tsi$to_list()
          if (return_type == 'data.frame') {
            ts <- tsi
          } 
          self$set_ts(tsi)
        }
      } else {
        ts <- RomTS$new(self, config)
        ts <- ts$to_list()
        self$set_ts(ts)
      }
      # add to tsvalues data frame
      return(ts)
    },
    #' @param uri remote address to retrieve data
    #' @param content_type http content-type
    #' @param delim delimiter
    #' @param enc encoding
    #' @return result of web request
    auth_read = function(uri, content_type = "text/csv", delim = ",", enc = "xml") {
      auth_result <- om_auth_read(uri, private$token, content_type, delim, enc)
      return(auth_result)
    },
    #' @param ts = list(entity_type, featureid, tid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
    #' @return nothing sets internal private token
    set_ts = function(ts) {
      # check uniqueness
      # search for existing based on uniqueness
      # uniqueness is variable def related, not arbitrary 
      print(ts)
      ts_check = FALSE
      if (!is.na(ts$tid)) {
        if (ts$tid > 0) {
          ts_check = fn_search_tsvalues(list(tid = ts$tid), self$tsvalues)
          #print(ts_check)
        }
      }
      if (is.logical(ts_check)) {
        # not found, so add
        message("Storing TS")
        self$tsvalues <- rbind(self$tsvalues, as.data.frame(ts))
      } else {
        # update 
        message("Found, trying to load")
        self$tsvalues[ts$ID] <- ts
      }
    },
    #' @param vardef = list(varid, varkey, varname, varunits, varcode,...)
    #' @return local df index?
    set_vardef = function(var_def) {
      # check uniqueness
      # search for existing based on uniqueness
      # uniqueness is variable def related, not arbitrary 
      # Just return, the remainder is TBD (based on working ts value code)
      return(TRUE)
      print(var_def)
      ts_check = FALSE
      if (!is.na(var_def$varid)) {
        if (var_def$varid > 0) {
          var_check = fn_search_vardefs(list(varid = var_def$varid), self$var_defs)
          #print(ts_check)
        }
      }
      if (is.logical(var_check)) {
        # not found, so add
        message("Storing Var")
        self$tsvalues <- rbind(self$var_defs, as.data.frame(var_def))
      } else {
        # update 
        message("Found, trying to load")
        self$var_defs[var_def$ID] <- var_def
      }
    },
    #' @param vardef = list(varid, varkey, varname, varunits, varcode,...)
    #' @return local df index?
    post = function(entity_type, pk, config) {
      message(paste("site",self$site))
      message(paste("token", private$token))
      fn_post_rest(entity_type, pk, config, self$site, private$token)
    },
    #' @field timeline for default time series data
    timeline = NULL,
    # todo: these should be defined in the RomTS object so that there is one and only one 
    #       place to maintain a list of fields that need to exist in this table.
    #       Alternatively, this can live here, but then RomTS must look here for template.
    #' @field tsvalues table of time series data
    tsvalues = data.frame(
      tid=character(),
      entity_type=character(),
      varid=integer(),
      featureid=integer(),
      tstime=integer(),
      tsendtime=integer(),
      tsvalue=numeric(),
      tscode=character(),
      #modified=integer(),
      #tlid=integer(),
      #uid=integer(),
      # todo: add bundles?
      #bundle=character(),
      stringsAsFactors=FALSE
    ),
    #' @field props table of object properties (can be contained by objects)
    props = data.frame(
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
    ),
    #' @field features table of physical features
    features = data.frame(),
    #' @field var_defs table of variable definitions
    var_defs = data.frame(
      varid = integer(),
      varname = character(),
      vardesc = character(),
      vocabulary = character(),
      varunits = character(),
      varkey = character(),
      datatype = character(),
      varcode = character(),
      isregular = character(),
      timestep = numeric(),
      timeunits = character(),
      nodataval = numeric(),
      status = character(),
      data_entry = character(),
      plugin = character(),
      options = character(),
      varabbrev = character(),
      multiplicity = character()
    ),
    #' @field admin_features table of adminreg features
    admin_features = data.frame(),
    #' @field ts_cache list of ts objects instantiated
    ts_cache = list(),
    #' @field feature_cache list of feature objects instantiated
    feature_cache = list(),
    #' @field prop_cache list of prop objects instantiated
    prop_cache = list(),
    #' @field var_cache list of var objects instantiated
    var_cache = list()
  )
)
