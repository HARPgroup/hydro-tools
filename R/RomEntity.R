#' Base entity data object
#' @description Object for storing a single entity with attribute and timeseries related
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param datasource optional RomDataSource for remote and local storage
#' @param config list of attributes to set
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export RomEntity
RomEntity <- R6Class(
  "RomEntity",
  public = list(
    #' @field base_entity_type kind of entity
    base_entity_type = NA,
    #' @return get_id the unique id of this entity alias to remote pkid, subclassed as function
    get_id = function() {
      return(NULL)
    },
    #' @return propvalues unique properties of this entity
    #' @param propname optional name to filter
    #' @param varid option variable to filter
    propvalues = function(propname = NULL, varid = NULL) {
      ps <- self$datasource$get_prop(list(featureid = self$get_id(), propname = propname, varid = varid, entity_type=self$base_entity_type))
      return(ps)
    },
    #' @field tsvalues values for this entity
    tsvalues = NA,
    #' @field datasource RomDataSource
    datasource = NA,
    #' @param site URL of some RESTful repository
    #' @param config list of attributes to set, see also: to_list() for format
    #' @param load_remote automatically query REST data source for matches?
    #' @return object instance
    initialize = function(datasource = NULL, config = list(), load_remote = FALSE) {
    }
  )
)


