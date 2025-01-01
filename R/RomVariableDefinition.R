#' Nested Property Object
#' @description Object for storing a single feature with attribute and timeseries related
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param datasource optional RomDataSource for remote and local storage
#' @param config list of attributes to set
#' @return feature class of type RomFeature
#' @seealso NA
#' @examples NA
#' @export RomVariableDefinition
RomVariableDefinition <- R6Class(
  "RomVariableDefinition",
  inherit = RomEntity,
  public = list(
    #' @field base_entity_type kind of entity
    base_entity_type = 'dh_variabledefinition',
    #' @field pk_name the name of this entity's pk column
    pk_name = 'hydroid',
    #' @field varkey The unique alphanumeric key for this variable
    varkey = NA,
    #' @field varname the name of this entity
    varname = NA,
    #' @field varcode alpha code for this entity from original dataset
    varcode = NA,
    #' @field varid duplicate of hydroid, more common name
    varid = NA,
    #' @field hydroid unique ID (integer)
    hydroid = NA,
    #' @field varunits cfs/MGD, ...
    varunits = NA,
    #' @field plugin handler code for advanced operations
    plugin = NA,
    #' @field vardesc notes field
    vardesc = NA,
    #' @field vocabulary group that this belongs to
    vocabulary = NA,
    #' @field datatype type of data
    datatype = NA,
    #' @field multiplicity determine allowable uniqueness
    multiplicity = NA,
    #' @param datasource RESTful repository object
    #' @param config list of attributes to set, see also: to_list() for format
    #' @param load_remote automatically query REST dataa source for matches?
    #' @return object instance
    initialize = function(datasource = NULL, config, load_remote = FALSE) {
      super$initialize(datasource, config, load_remote)
    },
    #' @return get_id the unique id of this entity alias to remote pkid, subclassed as function
    get_id = function() {
      return(self$hydroid)
    },
    #' @return list of object attributes suitable for input to new() and from_list() methods
    to_list = function() {
      # returns as a list, which can be set and fed back to 
      # from_list() or new(config)
      t_list <- list()
      return(t_list)
    },
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return NULL
    from_list = function(config) {
      for (i in names(config)) {
        if (i %in% names(self)) {
          if (typeof(self[[i]]) != 'closure') {
            # this is not a function, so we can set it
            self[[i]] = config[[i]]
          }
        }
      }
    },
    #' @param push_remote update locally only or push to remote database
    #' @return NULL
    save = function(push_remote=FALSE) {
      # object class responsibilities
      # - know the required elemenprop such as varid, featureid, entity_type
      #   fail if these required elemenprop are not available 
      if (push_remote) {
        message("save() is not yet available")
      }
      self$datasource$set_vardef(self$to_list())
    },
    #' @param config 
    #' @param load_remote automatically query remote data source for matches?
    #' @returns the data from the remote connection
    load_data = function(config, load_remote) {
      if (is.data.frame(config)) {
        # try to load directly
        self$datasource$set_vardef(config)
      } else {
        super$load_data(config, load_remote)
      }
    },
    #' @param thisobject an instance of RomEntity with vardef enabled 
    #' @returns instance of dHVariablePlugin 
    get_plugin = function(thisobject) {
      # get_plugin_class() is defined in dHVariablePluginDefault.R
      # we pass the object in case the plugin needs it on instantiation
      plugin = get_plugin_class(thisobject$vardef$plugin, thisobject)
      return(plugin)
    }
    
  )
)


