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
    #' @field rest_uname username to connect to RESTful repository
    rest_uname = NULL,
    #' @param site URL of some RESTful repository
    #' @param rest_uname username to connect to RESTful repository
    #' @return object instance
    initialize = function(site, rest_uname = NULL) {
      self$site = site
      self$rest_uname = rest_uname
    },
    #' @param table which table. Default 'all'
    #' @return nothing clears data tables
    reset = function(table) {
      for (t in c('props', 'features', 'tsvalues', 'var_defs')) {
        if ( (table == 'all') | (t == table) ) {
          self[[t]] <- self[[t]][0,]
        }
      }
    },
    #' @return nothing sets internal private token
    get_token = function() {
      if (!is.character(self$site) ) {
        warning("Base URL to REST repository not supplied.")
      }
      private$token <- om_vahydro_token(self$site, self$rest_uname)
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
    #' @param return_type 'data.frame' or 'list'
    #' @param force_refresh if this ds has a remote source, whether to pull anew
    #' @return nothing sets internal private token
    get_prop = function(config, return_type = 'data.frame', force_refresh = FALSE) {
      props = FALSE
      propvalues <- fn_search_properties(config, self$propvalues)
      
      if (is.logical(propvalues)) {
        # none exists locally, so query
        force_refresh = TRUE
      }
      if (!is.null(self$site) & force_refresh) {
        propvalues <- fn_get_rest('dh_properties', 'pid', config, self$site, private$token)
        if (!is.logical(propvalues)) {
          if (nrow(propvalues) >= 1) {
            prop <- as.list(propvalues[1,])
          }
        }
      }
      # return either the raw fn_get_timeseries/fn_search_propvalues 
      # or a the first found item
      if (return_type != 'data.frame') {
        return(prop)
      } else {
        return(propvalues)
      }
    },
    # need get_ts - get data frame of ts values matching criteria
    # load_object - load entity single object config
    # get_ts method description
    # this could actually live in the RomTS object
    #' @param config = list(entity_type, featureid, tid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
    #' @param return_type 'data.frame' or 'list'
    #' @param force_refresh if this ds has a remote source, whether to pull anew
    #' @return nothing sets internal private token
    get_ts = function(config, return_type = 'data.frame', force_refresh = FALSE) {
      # return_type = 'list', or 'data.frame'
      # default to data.frame to maintain compatibility with getTimeseries
      # force_refresh = if FALSE, use local value if we already have one
      # easier backwards compatibility, lower data needs?
      # if returning object, only take first value? 
      # or, return FALSE with message that df is only option? Hmmmm.
      # or return 
      # search first in 
      ts = FALSE
      tsvalues <- fn_search_tsvalues(config, self$tsvalues)
      if (is.logical(tsvalues)) {
        # none exists locally, so query
        force_refresh = TRUE
      }
      if (!is.null(self$site) & force_refresh) {
        # todo: switch to generic get method if possible
        tsvalues <- fn_get_timeseries(config, self$site, private$token)
        if (!is.logical(tsvalues)) {
          if (nrow(tsvalues) >= 1) {
            # stash the first one in case we only want a single
            ts <- as.list(tsvalues[1,])
            # store all features in local db
          }
        }
      }
      # return either the raw fn_get_timeseries/fn_search_tsvalues 
      # or a the first found item
      if (return_type != 'data.frame') {
        return(ts)
      } else {
        return(tsvalues)
      }
      
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
    #' @param prop = list(entity_type, featureid, pid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
    #' @return nothing seprop internal private token
    set_prop= function(prop) {
      # check uniqueness
      # search for existing based on uniqueness
      # uniqueness is variable def related, not arbitrary 
      print(prop)
      prop_check = FALSE
      if (!is.na(prop$pid)) {
        if (prop$pid > 0) {
          prop_check = fn_search_properties(list(pid = prop$pid), self$propvalues)
          #print(prop_check)
        }
      }
      if (is.logical(prop_check)) {
        # not found, so add
        message("Storing prop")
        self$propvalues <- rbind(self$propvalues, as.data.frame(prop))
      } else {
        # update 
        message("Found, trying to load")
        self$propvalues[prop$ID] <- prop
      }
    },
    #' @param var_def = list(varid, varkey, varname, varunits, varcode,...)
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
    #' @param entity_type = dh_feature, dh_properties, ...
    #' @param pk = primary key column name, e.g. hydroid, pid, ...
    #' @param config = contents of record to post in list(pid, propname, propvalue, ...)
    #' @return local df index?
    get = function(entity_type, pk, config) {
      fn_get_rest(entity_type, pk, config, self$site, private$token)
    },
    #' @param entity_type = dh_feature, dh_properties, ...
    #' @param pk = primary key column name, e.g. hydroid, pid, ...
    #' @param config = contents of record to post in list(pid, propname, propvalue, ...)
    #' @return local df index?
    post = function(entity_type, pk, config) {
      message(paste("site",self$site))
      message(paste("token", private$token))
      return_id = fn_post_rest(entity_type, pk, config, self$site, private$token)
      return(return_id)
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
    #' @field propvalues table of object properties (can be contained by objects)
    propvalues = data.frame(
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
