#' Feature entity data object
#' @description Object for storing a single feature with attribute and timeseries related
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param datasource optional RomDataSource for remote and local storage
#' @param config list of attributes to set
#' @return feature class of type RomAdminFeature
#' @seealso NA
#' @examples NA
#' @export RomAdminFeature
RomAdminFeature <- R6Class(
  "RomAdminFeature",
  inherit = RomEntity,
  public = list(
    #' @field base_entity_type kind of entity
    base_entity_type = 'dh_adminreg_feature',
    #' @field pk_name the name of this entity's pk column
    pk_name = 'adminid',
    #' @field name of this entity
    name = NA,
    #' @field admincode alpha code for this entity from original dataset
    admincode = NA,
    #' @field ftype feature type
    ftype = NA,
    #' @field adminid unique ID (integer)
    adminid = NA,
    #' @field bundle main content type, i.e. facility, well, intake, ...
    bundle = NA,
    #' @field fstatus entity status
    fstatus = NA,
    # list of object references?  or list format name, value, ...
    #' @field description notes field
    description = NA,
    #' @field mps linked features
    mps = NA,
    #' @field geom feature geometry WKT
    geom = NA,
    #' @param datasource RESTful repository object
    #' @param config list of attributes to set, see also: to_list() for format
    #' @param load_remote automatically query REST data source for matches?
    #' @return object instance
    initialize = function(datasource = NULL, config, load_remote = FALSE) {
      #col.names(self$properties <-
      super$initialize(datasource, config, load_remote)
    },
    #' @return get_id the unique id of this entity alias to remote pkid, subclassed as function
    get_id = function() {
      return(self$adminid)
    },
    #' @return list of object attributes suitable for input to new() and from_list() methods
    to_list = function() {
      # returns as a list, which can be set and fed back to 
      # from_list() or new(config)
      t_list <- list(
        adminid = self$adminid,
        name = self$name,
        admincode = self$admincode,
        ftype = self$ftype,
        fstatus = self$fstatus,
        bundle = self$bundle
      )
      return(t_list)
    },
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return NULL
    from_list = function(config) {
      for (i in names(config)) {
        if (i == "adminid") {
          self$adminid = as.integer(as.character(config$adminid))
        } else if (i == "name") {
          self$name = as.character(config$name)
        } else if (i == "admincode") {
          self$admincode = as.character(config$admincode)
        } else if (i == "ftype") {
          self$ftype = as.character(config$ftype)
        } else if (i == "bundle") {
          self$bundle = as.character(config$bundle)
        } else if (i == "fstatus") {
          self$fstatus = as.character(config$fstatus)
        } else if (i == "dh_geofield") {
          self$geom = as.character(config$dh_geofield)
        }
      }
    },
    #' @param thismp mp entity
    #' @return add a connected MP. TBD
    add_mp = function (thismp) {
      j = length(self$mps) + 1
      #self$mps[j] <- thismp
      self$mps[j] <- list('obj' = thismp)
    }
  )
)


