#' @name Hydro Data Source Data Source Bin
#' @description RomDataSource is an object that contains tables and methods of
#'   model data objects. It serves as a global object store that references to
#'   all objects of a given domain and pointers to remote data source if
#'   applicable. It offers the flexibility of synching (via POST or
#'   INSERT/UPDATE) to a remote data source OR allows users to update local data
#'   bases or sources. It does not permit a mix of these data streams, keeping
#'   the RomDataSource object consistent.
#' @details Provides integrated, queryable universe of features, observations
#'   and meta data
#' @importFrom R6 R6Class
#' @param baseurl URL of remote data store with REST capabilities, optional
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples 
#' #Get new datasource via odbc
#' ds <- RomDataSource$new(site,
#'                         rest_uname = odbc_uname,
#'                         connection_type = 'odbc',
#'                         dbname = databaseName)
#' ds$get_token(rest_pw = odbc_pw)
#' #Pointer to external db
#' ds$connection
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
    #' @field json_obj_url URL for retrieving full objects
    json_obj_url = NULL,
    #' @field connection_type rest or odbc
    connection_type = NULL, 
    #' @field connection rest or odbc
    connection = NULL, 
    #' @field rest_uname username to connect to RESTful repository
    rest_uname = NULL,
    #' @field dbname DATABASE TO USE IN odbC CONNECTION
    dbname = NULL,
    #' @param site URL of some RESTful repository or the host of the target
    #'   database. At DEQ, this is defined in the default config files.
    #' @param rest_uname username to connect to RESTful repository or database.
    #'   At DEQ, this should be defined within local config files
    #' @param connection_type String, either 'rest' or 'odbc' depending on
    #'   target database
    #' @param dbname Used only when connection_type = 'odbc'. This is the
    #'   database name of the database that supports the ODBC connection. At
    #'   DEQ, this should generally target the active dbase, not alpha, although
    #'   both are supported
    #' @return Instance of RomDataSource, now with populated site, rest_uname,
    #'   connection_type, and dbname data
    initialize = function(site, rest_uname = NULL, connection_type = 'rest', dbname = NULL) {
      self$site = site
      self$rest_uname = rest_uname
      self$connection_type = connection_type
      self$dbname = dbname
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
    #' @param rest_pw to use, if NULL will prompt
    #' @param odbc_port to use, if NULL will prompt
    #' @return nothing sets internal private token
    get_token = function(rest_pw = NULL, odbc_port = 5431) {
      if (!is.character(self$site) ) {
        warning("Base URL to REST repository not supplied.")
      }
      if (self$connection_type == 'rest') {
        private$token <- om_vahydro_token(self$site, self$rest_uname, rest_pw)
      } else {
        self$connection <- dbConnect(
          RPostgres::Postgres(),
          dbname = self$dbname,
          host = httr::parse_url(self$site)$hostname,
          port = odbc_port,
          user = self$rest_uname,
          password = rest_pw
        )
      }
    },
    # this could actually live in the RomTS object
    #' @param varkey = variable key
    #' @param force_refresh Check remote repository for new info? 
    #' @param debug show info
    #' @return nothing sets internal private token
    get_vardef = function(varkey, force_refresh = FALSE, debug = FALSE) {
      # NOt yet tested,
      config = list()
      #print(paste("Handling ", varkey))
      if (is.na(as.integer(varkey))) {
        config$varkey = varkey
      } else {
        config$hydroid = as.integer(varkey)
        #if ()
      }
      # check local store, if not there, check remote
      vardef <- fn_search_vardefs(config, self$var_defs)
      if (is.logical(vardef)) {
        # none exists locally, so query
        force_refresh = TRUE
      }
      if (!is.null(self$site) & force_refresh) {
        if (self$connection_type == 'odbc') {
          config$limit = 1
          vardef <- self$get('dh_variabledefinition', 'hydroid', config)
          vardef <- as.list(vardef[1,])
          if ('hydroid' %in% names(vardef)) {
            vardef$varid <- vardef$hydroid
          }
        } else {
          vardef <- fn_get_vardef_view(varkey, self$site, private$token, debug)
          # TBD
          # vardef <- RomVarDef$new(self,var_one)
          # vardef <- vardef$to_list()
        }
        # after retrieval, store locally
        self$set_vardef(vardef)
      }
      return(vardef)
    },
    # get properties
    #' @param config = list(entity_type, featureid, tid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
    #' @param return_type 'data.frame' or 'list'
    #' @param force_refresh if this ds has a remote source, whether to pull anew
    #' @param obj optional object which can supply more specific query info for odbc
    #' @return nothing sets internal private token
    get_prop = function(config, return_type = 'data.frame', force_refresh = FALSE, obj = FALSE) {
      prop = FALSE
      # odbc has robust query handling so we don't need to do this
      if (self$connection_type == 'odbc') {
        propvalues <- self$get('dh_properties', 'pid', config, obj)
      } else {
        # todo: all entities should be able to be searched by the odbc methods
        #       so eventually all this will be phased out, since the odbc methods
        #       have robust querying, and should be able to query against the datasource
        #       using it's names as an environment.  We can make the propvalues
        #       point to dh_properties on the datasource
        #       and also tsvalues point to dh_timeseries_values
        propvalues <- fn_search_properties(config, self$propvalues)
        
        if (is.logical(propvalues)) {
          # none exists locally, so query
          force_refresh = TRUE
        }
        if (!is.null(self$site) & force_refresh) {
          propvalues <- fn_get_rest('dh_properties', 'pid', config, self$site, private$token)
        }
      }
      if (!is.logical(propvalues)) {
        if (nrow(propvalues) >= 1) {
          prop <- as.list(propvalues[1,])
        }
      } else {
        prop <- propvalues
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
    #' @param obj optional object which can supply more specific query info for odbc
    #' @return nothing sets internal private token
    get_ts = function(config, return_type = 'data.frame', force_refresh = FALSE, obj = FALSE) {
      # return_type = 'list', or 'data.frame'
      # default to data.frame to maintain compatibility with getTimeseries
      # force_refresh = if FALSE, use local value if we already have one
      # easier backwards compatibility, lower data needs?
      # if returning object, only take first value? 
      # or, return FALSE with message that df is only option? Hmmmm.
      # or return 
      # search first in 
      # odbc has robust query handling so we don't need to us fn_get_timeseries
      if (self$connection_type == 'odbc') {
        ts_obj = RomTS$new(self)
        tsvalues <- self$get('dh_timeseries', 'tid', config, ts_obj)
      } else {
        # todo: all entities should be able to be searched by the odbc methods
        #       so eventually all this will be phased out, since the odbc methods
        #       have robust querying, and should be able to query against the datasource
        #       using it's names as an environment.  We can make the propvalues
        #       point to dh_properties on the datasource
        #       and also tsvalues point to dh_timeseries_values
        ts = FALSE
        tsvalues <- fn_search_tsvalues(config, self$tsvalues)
        if (is.logical(tsvalues)) {
          # none exists locally, so query
          force_refresh = TRUE
        }
        if (!is.null(self$site) & force_refresh) {
          # todo: switch to generic get method if possible
          tsvalues <- fn_get_timeseries(config, self$site, private$token)
        }
      }
      if (!is.logical(tsvalues)) {
        if (nrow(tsvalues) >= 1) {
          # stash the first one in case we only want a single
          ts <- as.list(tsvalues[1,])
          # store all features in local db
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
      #message(ts)
      ts_check = FALSE
      if (!is.na(ts$tid)) {
        if (ts$tid > 0) {
          ts_check = fn_search_tsvalues(list(tid = ts$tid), self$tsvalues)
          #message(ts_check)
        }
      }
      if (is.logical(ts_check)) {
        # not found, so add
        #message("Storing TS")
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
      #message(prop)
      if (is.data.frame(prop)) {
        prop <- self$insure_cols(prop, self$propvalues)
        propvalue_tmp <- self$propvalues
        # we handle this a little differently, and it may have multiples
        dsl <- sqldf(
          "select * from prop 
           where pid not in (
             select pid from propvalue_tmp
          )"
        )
        self$propvalues = rbind(self$propvalues, dsl)
        
      } else {
        
        prop_check = FALSE
        if (!is.na(prop$pid)) {
          if (prop$pid > 0) {
            prop_check = fn_search_properties(list(pid = prop$pid), self$propvalues)
            #message(prop_check)
          }
        }
        prop <- self$insure_cols(as.data.frame(prop), self$propvalues)
        if (is.logical(prop_check)) {
          # not found, so add
          message("Storing prop")
          self$propvalues <- rbind(self$propvalues, as.data.frame(prop))
        } else {
          # update 
          message("Found, trying to load")
          self$propvalues[prop$ID] <- prop
        }
      }
    },
    #' @param src_df = df to verify/insure
    #' @param dest_df = df template to supply valid names
    #' @return local df index?
    insure_cols = function(src_df, dest_df) {
      name_check <- names(dest_df)[
        which(!(names(dest_df) %in% names(src_df)))
      ]
      # add missing columns if they exist
      if (length(name_check) > 0) {
        #message("Warning: all src columns must be present in data frame to do batch insert.")
        #message("Adding", cat(names(dest_df)[which(!(names(dest_df) %in% names(src_df)))],sep=","))
        for (n in names(dest_df)[which(!(names(dest_df) %in% names(src_df)))]) {
          src_df[,n] <- NA
        }
      }
      # eliminate superfluous and sort in the same order
      src_df <- src_df[,names(dest_df)]
    },
    #' @param vardef = list(varid, varkey, varname, varunits, varcode,...)
    #' @return local df index?
    set_vardef = function(vardef) {
      # check uniqueness
      # search for existing based on uniqueness
      # uniqueness is variable def related, not arbitrary 
      # Just return, the remainder is TBD (based on working ts value code)
      if (!is.data.frame(vardef)) {
        vardef = as.data.frame(vardef)
      }
      name_check <- names(self$var_defs)[
        which(!(names(self$var_defs) %in% names(vardef)))
      ]
      if (is.na(vardef$hydroid)) {
        message("Bad vardef for variable")
        return(FALSE)
      }
      # add missing columns if they exist
      if (length(name_check) > 0) {
        message("Warning: all variable definition columns should be present in data frame to do batch insert.")
        # this is a useful message, but the use of the `cat` statement causes output
        # to hit the console, which goofs up use as a script
        #message("Adding", cat(names(self$var_defs)[which(!(names(self$var_defs) %in% names(vardef)))],sep=","))
        for (n in names(self$var_defs)[which(!(names(self$var_defs) %in% names(vardef)))]) {
          vardef[,n] <- NA
        }
      }
      # eliminate superfluous and sort in the same order
      vardef <- vardef[,names(self$var_defs)]
      var_defs <- self$var_defs
      # we handle this a little differently, and it may have multiples
      veq = "select * from vardef 
         where hydroid not in (
           select hydroid from var_defs
        )"
      dsl <- sqldf(
        veq
      )
      if (nrow(dsl) > 0) {
        self$var_defs = rbind(self$var_defs, dsl)
      }
    },
    #' @param features = list(entity_type, featureid, pid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
    #' @return nothing seprop internal private token
    store_features = function(features) {
      # prototype.  Testing, does it work for multiple features and single features retrieved?
      if (nrow(features) > 0) {
        for (i in 1:nrow(features)) {
          feature = features[i,]
          self$set_feature(feature)
        }
      }
    },
    #' @param feature = list(entity_type, featureid, pid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
    #' @return nothing seprop internal private token
    set_feature = function(feature) {
      # check uniqueness
      # search for existing based on uniqueness
      # uniqueness is variable def related, not arbitrary 
      #message(feature)
      feature_check = FALSE
      if (!is.na(feature$hydroid)) {
        if (feature$hydroid > 0) {
          feature_check = fn_search_features(list(hydroid = feature$hydroid), self$features)
          #message(feature_check)
        }
      }
      if (is.logical(feature_check)) {
        # not found, so add
        #message("Storing feature")
        self$features <- rbind(self$features, as.data.frame(feature))
      } else {
        # update 
        message("Found, trying to load")
        self$features[feature$ID] <- feature
      }
    },
    #' @param entity_type = dh_feature, dh_properties, ...
    #' @param pk = primary key column name, e.g. hydroid, pid, ...
    #' @param config = contents of record to post in list(pid, propname, propvalue, ...)
    #' @param obj = (optional) object class calling this routine, can supply extra info
    #' @return local df index?
    get = function(entity_type, pk, config, obj = FALSE) {
      if (self$connection_type == 'rest') {
        retvals = fn_get_rest(entity_type, pk, config, self$site, private$token)
      } else {
        retvals = fn_get_odbc(entity_type, pk, config, self$connection, obj)
      }
      
      return(retvals)
    },
    #' @param entity_type = dh_feature, dh_properties, ...
    #' @param pk = primary key column name, e.g. hydroid, pid, ...
    #' @param config = contents of record to post in list(pid, propname, propvalue, ...)
    #' @return local df index?
    post = function(entity_type, pk, config) {
      if (self$connection_type == 'rest') {
        return_id = fn_post_rest(entity_type, pk, config, self$site, private$token)
      } else {
        return_id = fn_post_odbc(entity_type, pk, config, self$connection)
      }
      return(return_id)
    },
    #' @param entity_type = dh_feature, dh_properties, ...
    #' @param pk = primary key column name, e.g. hydroid, pid, ...
    #' @param config = contents of record to post in list(pid, propname, propvalue, ...)
    #' @param obj = (optional) object class calling this routine, can supply extra info
    #' @return local df index?
    delete = function(entity_type, pk, config, obj = FALSE) {
      if (self$connection_type == 'rest') {
        retvals = fn_delete_rest(entity_type, pk, config, self$site, private$token)
      } else {
        retvals = fn_delete_odbc(entity_type, pk, config, self$connection, obj)
      }
      
      return(retvals)
    },
    #' @param pid = object pid
    #' @return unserialized json as list, with object stored in ds$prop_json_cache
    get_json_prop = function(pid) {
      if (self$connection_type == 'rest') {
        model_obj_url <- paste(self$json_obj_url, pid, sep="/")
        model_info <- self$auth_read(model_obj_url, "text/json", "")
        if (!is.logical(model_info)) {
          model <- jsonlite::fromJSON(model_info)[[1]]
          self$prop_json_cache[[pid]] <- model
          return(model)
        } else {
          return(FALSE)
        }
      } else {
        # use ODBC approach
        model_tree <- RomPropertyTree$new(self, list(root_pid=pid), TRUE)
        model <- self$get_nested_export(self, pid, model_tree$prop_list)[[1]]
        self$prop_json_cache[[pid]] <- model
        return(model)
      }
    },
    #' @param ds = satasource object, kept for posterity, as this may not always live here
    #' @param featureid = object pid
    #' @param props = container for stashing
    #' @param depth = depth limit for nesting (rarely used)
    #' @return unserialized json as list, with object stored in ds$prop_json_cache
    get_nested_export = function(ds, featureid, props, depth=0) {
      propatts <- as.list(props[which(props$pid == featureid),])
      thisobject = RomProperty$new(ds, propatts, FALSE )
      export = list()
      if (!is.null(thisobject$vardef)) {
        plugin <- thisobject$vardef$get_plugin(thisobject)
        export[[thisobject$propname]] = plugin$exportOpenMI(thisobject)
        children = props[which(props$featureid == featureid),]
        # note: this sqldf below is a version that uses sqldf to recursively trace the 
        #       property tree.  This is hugely inefficient, like 3,000% increase in execution time.
        #       This is kept for posterity and as an example of what not to do.
        #children = sqldf(paste("select * from props where featureid =", featureid), method="raw")
        if (nrow(children) > 0) {
          for (i in 1:nrow(children)) {
            thischild <- children[i,]
            sub_export <- self$get_nested_export(ds, thischild$pid, props, depth)
            export[[thisobject$propname]][[thischild$propname]] <- sub_export[[thischild$propname]]
          }
        }
        return(export)
        
      } else {
        message(paste("Cannot export", thisobject$base_entity_type, "object vardef is null"))
        return(FALSE)
      }
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
      data_matrix=character(),
      stringsAsFactors=FALSE
    ),
    #' @field features table of physical features
    features = data.frame(
      hydroid = integer(),
      name = character(),
      hydrocode = character(),
      ftype = character(),
      fstatus = character(),
      bundle = character(),
      geom = character()
    ),
    #' @field var_defs table of variable definitions
    var_defs = data.frame(
      hydroid = integer(),
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
    #' @field prop_json_cache list of json objects retrieved, keyed by ID
    prop_json_cache = NULL,
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
