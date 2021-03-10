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
    # need get_ts - get data frame of ts values matching criteria
    # load_object - load entity single object config
    # get_ts method description
    # this could actually live in the RomTS object
    #' @param config = list(entity_type, featureid, tid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
    #' @param return_type 'df' (data.frame) or 'object'
    #' @param force_refresh if this ds has a remote source, whether to pull anew
    #' @return nothing sets internal private token
    get_ts = function(config, return_type = 'data.frame', force_refresh = FALSE) {
      # return_type = 'list' or 'object', default to list to maintain
      # force_refresh = if FALSE, use local value if we already have one
      #    easier backwards compatibility, lower data needs?
      # if returning object, only take first value? 
      # or, return FALSE with message that df is only option? Hmmmm.
      # or return 
      # search first in 
      tsvalues <- fn_search_tsvalues(config, self$tsvalues)
      if (is.boolean(tsvalues)) {
        # none exists locally, so query
        force_refresh = TRUE
      }
      if (!is.null(self$site)) {
        ts <- fn_getTimeseries(config, self$site)
      } else {
        ts <- RomTS$new(self, config)
        ts <- rbind(ts$to_list())
      }
      # add to tsvalues data frame
      return(ts)
    },
    #' @field timeline for default time series data
    timeline = NULL,
    #' @field tsvalues table of time series data
    tsvalues = data.frame(
      tid=character(),
      entity_type=character(),
      featureid=character(),
      varid=character(),
      tstime=character(),
      tsendtime=character(),
      tsvalue=character(),
      tscode=character(),
      modified=character(),
      uid=character(),
      status=character(),
      bundle=character(),
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
    var_defs = data.frame(),
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