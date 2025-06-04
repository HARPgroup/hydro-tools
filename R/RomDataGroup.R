#' Nested Property Object
#' @description Object for storing a group of properties
#' @details Has standard methods for managing data and meta data related to
#'   groups of related properties (e.g. children and parents). Typically called
#'   from RomPropertyTree, which inherits this object class
#' @importFrom R6 R6Class  
#' @param datasource optional RomDataSource for remote and local storage
#' @param config list of attributes to set
#' @return Instance of RomDataGroup or, more likely, RomPropertyTree which is
#'   the only Rom object to inherit this object class
#' @seealso NA
#' @examples NA
#' @export RomDataGroup
RomDataGroup <- R6Class(
  "RomDataGroup",
  public = list(
    #' @field datasource RomDataSource typically estbalished via the DEQ config
    #'   file and should contain any relevant database connections
    datasource = NA,
    #' @field pk_name the name of this entity's primary key column. Most often
    #'   root_pid, but consult the Hydrotools ReadMe for more information
    pk_name = 'id',
    #' @param datasource  URL of some RESTful repository or the host of the
    #'   target database. At DEQ, this is defined in the default config files.
    #' @param config list of attributes to get, see also: to_list() for format.
    #'   These are passed to RomDataSource$get()
    #' @param load_remote automatically query REST data source or remote
    #'   datasource for matches?
    #' @return object instance of RomDataGroup or instance of whatever object
    #'   inherited this class, most likely RomPropertyTree. It will be populated
    #'   with relevant fields.
    initialize = function(datasource = NULL, config, load_remote = FALSE) {
      stopifnot(class(datasource)[[1]] == "RomDataSource")
      self$datasource <- datasource 
      config <- self$handle_config(config)
      if (is.logical(config)) {
        message("Configuration information faild validation. Returning.")
        return(FALSE)
      }
      # if requested, we try to load will return all features
      if (load_remote) {
        feature <- self$datasource$get(self$base_entity_type, self$pk_name, config, self)
        # merge config with prop
        message("Found")
        if (!is.logical(feature)) {
          #Grab ALL features returned
          config = feature
        }
      }
      self$load_data(config, load_remote)
    },
    #' @param config #A list of requests to pass to RomDataSource$get()
    #' @returns an updated config if necessary or FALSE if it fails
    handle_config = function(config) {
      return(config)
    },
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return NULL
    from_list = function(config) {
      return(TRUE)
    },
    #' @param config 
    #' @param load_remote automatically query remote data source for matches?
    #' @returns the data from the remote connection
    load_data = function(config, load_remote) {
      #message(paste("load_data() called "))
      self$from_list(config)
      #message(paste("Loaded object: "))
      #message(self)
      self$get_vardef()
      #message(paste("Loaded vardef: "))
      self$load_plugin()
      #message(paste("Loaded plugin: "))
      #message("self$pk_name=(below)")
      #message(self$pk_name)
      # this should be handled better.  We need to decide if we want to 
      # still use the local datasource as a repository for remote data
      # at first the thinking was no with ODBC, but maybe that's not correct?
      # in other words, it was thought that ODBC replaced the local storage...
      if (!is.na(self[[self$pk_name]]) && (load_remote == TRUE) ) {
        # stash a copy in the local datasource database 
        # if this was a valid retrieval from remote
        message("Saving to local db")
        #if (self$datasource$connection_type != 'odbc') {
        self$save(FALSE) 
        #}
      } else {
        #message("not saving to local")
      }
      return(TRUE)
    },
    #' @param config list of attributes to set, see also: to_list() for format
    #' @param refresh automatically refresh var info?
    #' @returns the variable definition object for this entity
    get_vardef = function(config = FALSE, refresh=FALSE) {
      if (self$has_vardef == FALSE) {
        return(FALSE)
      }
      if (!is.null(self$vardef) & !refresh) {
        return(self$vardef)
      }
      #message(paste("config$varkey =",config$varkey,"self$varid =", self$varid))
      if (!is.logical(config)) {
        if ( !('varkey' %in% names(config)) && !('varid' %in% names(config)) ) {
          return(FALSE)
        }
        vardef = self$datasource$get_vardef(config$varkey)
      } else {
        if (is.null(self$varid) && is.null(self$varkey)) {
          return(FALSE)
        }
        if (is.null(self$varid) || is.na(self$varid)) {
          vardef = self$datasource$get_vardef(self$varkey)
        } else {
          vardef = self$datasource$get_vardef(self$varid)
        }
      }
      if (is.logical(vardef)) {
        return(FALSE)
      }
      #message("vardef retrieved, creating RomVar object")
      self$vardef = RomVariableDefinition$new(self$datasource,as.list(vardef))
      return(self$vardef)
    },
    #' @return nothing, but loads the objects plugin
    load_plugin = function() {
      if (is.null(self$vardef)) {
        # this is only valid for types that have vardefs
        return(FALSE)
      }
      # get_plugin method is 
      self$plugin = self$vardef$get_plugin(self)
    }
    
  )
)


