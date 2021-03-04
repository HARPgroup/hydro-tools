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
#' @export
RomDataSource <- R6Class(
  "RomDataSource",
  # Note: do not document private fields, since they are private
  private = list(
    token = NULL # rest token if remote rest source is used
  ),
  public = list(
    #' @field base_url URL of some RESTful repository
    base_url = NULL,
    #' @param base_url URL of some RESTful repository
    #' @return object instance
    initialize = function(base_url) {
      self$base_url = base_url
    },
    #' @return nothing sets internal private token
    get_token = function() {
      if (!is.character(self$base_url) ) {
        warning("Base URL to REST repository not supplied.")
      }
      private$token <- om_vahydro_token(self$base_url)
    },
    #' @field timeline for default time series data
    timeline = NULL,
    #' @field tsvalues table of time series data
    tsvalues = data.frame(
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
    admin_features = data.frame()
  )
)