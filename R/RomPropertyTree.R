#' Nested Property Object
#' @description Object for storing a property and all of its sub-properties.
#' @details Has standard methods for managing data and meta data. This object
#'   class will use a recursive query to navigate a graph style database (e.g.
#'   VA Hydro) to get the requested property and all first-, second-, etc. order
#'   proerpties below it.
#' @importFrom R6 R6Class  
#' @param datasource optional RomDataSource for remote and local storage
#' @param config list of attributes to set. These will be passed to
#'   RomDataGroup$new() and ultimately to RomDataSource$get()
#' @return Instance of type RomPropertyTree
#' @seealso NA
#' @examples NA
#' @export RomPropertyTree
RomPropertyTree <- R6Class(
  "RomPropertyTree",
  inherit = RomDataGroup,
  public = list(
    #' @field base_entity_type kind of entity
    base_entity_type = 'dh_properties',
    #' @field pk_name the name of this entity's pk column, always root_pid
    pk_name = 'root_pid',
    #' @field prop_list The raw property rows returned. This will contain all
    #'   subproperties of the property requested upon initialization of this
    #'   instance
    prop_list = NA,
    #' @field mps linked features
    mps = NA,
    #' @field geom feature geometry WKT
    geom = NA,
    #' @field sql_select_from Enhanced form of query overrides default SQL guess
    #'   from hydrotools:::fn_guess_sql(). This will use a recurvsive query to
    #'   get property and all subproperties
    sql_select_from = "
      WITH RECURSIVE prop_tree AS (
      SELECT [root_pid] as root_pid, p.pid, vp.varkey, p.featureid, 
        p.propname, p.propcode, p.propvalue, 
        p.varid, p.startdate, p.enddate, 
        p.bundle, p.modified, p.entity_type,
        p.proptext, p.data_matrix
      FROM dh_properties_fielded as p 
      LEFT OUTER JOIN dh_variabledefinition as vp
      ON (vp.hydroid = p.varid)
      WHERE p.pid = [root_pid]
      UNION
      SELECT [root_pid] as root_pid, c.pid, cp.varkey, c.featureid, 
        c.propname, c.propcode, c.propvalue, 
        c.varid, c.startdate, c.enddate, 
        c.bundle, c.modified, c.entity_type,
        c.proptext, c.data_matrix
      FROM dh_properties_fielded as c 
      LEFT OUTER JOIN dh_variabledefinition as cp
      ON (cp.hydroid = c.varid)
      inner join prop_tree as p
      on (c.featureid = p.pid and c.entity_type = 'dh_properties')
    )
    SELECT * from prop_tree",
    #' @param datasource RESTful repository object
    #' @param config list of attributes to set, see also: to_list() for format.
    #'   These should contain identifying information about the requested
    #'   property and will ultimatley be passed to RomDataGroup$new() and to
    #'   RomDataSource$get()
    #' @param load_remote automatically query REST dataa source for matches?
    #' @return object instance of RomPropertyTree, populated with details of
    #'   requested property and all subproperties (in propvalues())
    initialize = function(datasource = NULL, config, load_remote = FALSE) {
      #col.names(self$properties <-
      if (!'root_pid' %in% names(config)) {
        return(FALSE)
      }
      # first, change the sql to reflect the desired pid base, then proceed
      self$sql_select_from <- str_replace_all(self$sql_select_from, '\\[root_pid\\]', as.character(config$root_pid))
      #message(paste("Base Query:",self$sql_select_from))
      super$initialize(datasource, config, load_remote)
      # experimental support for automatic local caching
      #self$datasource$set_feature(self$to_list())
      #self$mps = list()
    },
    #' @return get_id the unique id of this entity alias to remote pkid, subclassed as function
    get_id = function() {
      return(self$pid)
    },
    #' @return list of object attributes suitable for input to new() and from_list() methods
    to_list = function() {
      # returns as a list, which can be set and fed back to 
      # from_list() or new(config)
      t_list <- list()
      return(t_list)
    },
    #' @param config list of properties to set, see also: to_list() for format
    #' @return NULL
    from_list = function(config) {
      # TBD
      # startdate, enddate, modified, value, name 
      message("from_list() is not yet available")
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
    },
    #' @param config list of properties from RomDataGroup$new()
    #' @param load_remote automatically query remote data source for matches?
    #' @return Nothing, but will set the properties on this instance of
    #'   RomPropertyTree with appropriate variable definitions in prop_list()
    #'   and propvalues() via self$datasource$set_prop()
    load_data = function(config, load_remote) {
      self$prop_list = config
      self$datasource$set_prop(config)
      #print("Loading vardefs")
      # now, load all associated variable definitions if possible
      if ('varid' %in% names(config)) {
        vars <- as.data.frame(unique(config$varid))
        #message(vars)
        if (nrow(vars) > 0) {
          for (v in 1:nrow(vars)) {
            vid <- as.integer(vars[v,])
            self$datasource$get_vardef(vid)
          }
        }
      }
      #print(vars)
      for (v in 1:nrow(vars)) {
        vid <- as.integer(vars[v,])
        self$datasource$get_vardef(vid)
      }
    }
  )
)


