#' Nested Property Object
#' @description Object for storing a single feature with attribute and timeseries related
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param datasource optional RomDataSource for remote and local storage
#' @param config list of attributes to set
#' @return feature class of type RomFeature
#' @seealso NA
#' @examples NA
#' @export RomDataGroup
RomDataGroup <- R6Class(
  "RomDataGroup",
  inherit = RomEntity,
  public = list(
    #' @field pk_name the name of this entity's pk column
    pk_name = 'id',
    #' @param datasource RESTful repository object
    #' @param config list of attributes to set, see also: to_list() for format
    #' @param load_remote automatically query REST dataa source for matches?
    #' @return object instance
    initialize = function(datasource = NULL, config, load_remote = FALSE) {
      super$initialize(datasource, config, load_remote)
    }
  )
)


