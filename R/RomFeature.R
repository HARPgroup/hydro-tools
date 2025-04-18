#' Feature entity data object
#' @description Object for storing a single feature with attribute and timeseries related
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param datasource optional RomDataSource for remote and local storage
#' @param config list of attributes to set
#' @return feature class of type RomFeature
#' @seealso NA
#' @examples NA
#' @export RomFeature
RomFeature <- R6Class(
  "RomFeature",
  inherit = RomEntity,
  public = list(
    #' @field base_entity_type kind of entity
    base_entity_type = 'dh_feature',
    #' @field pk_name the name of this entity's pk column
    pk_name = 'hydroid',
    #' @field name of this entity
    name = NA,
    #' @field hydrocode alpha code for this entity from original dataset
    hydrocode = NA,
    #' @field ftype feature type
    ftype = NA,
    #' @field hydroid unique ID (integer)
    hydroid = NA,
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
    #' @field sql_select_from syntax to use to select via an odbc or other SQL based datasource
    sql_select_from = "
      select * from dh_feature_fielded
    ",
    #' @field base_only - how to export to list in case of complex multi table entity and ODBC
    base_only = FALSE,
    #' @param datasource RESTful repository object
    #' @param config list of attributes to set, see also: to_list() for format
    #' @param load_remote automatically query REST dataa source for matches?
    #' @return object instance
    initialize = function(datasource = NULL, config, load_remote = FALSE) {
      #col.names(self$properties <-
      super$initialize(datasource, config, load_remote)
      # experimental support for automatic local caching
      self$datasource$set_feature(self$to_list())
      self$mps = list()
    },
    #' @return get_id the unique id of this entity alias to remote pkid, subclassed as function
    get_id = function() {
      return(self$hydroid)
    },
    #' @param config 
    #' @param load_remote automatically query remote data source for matches?
    #' @returns the data from the remote connection
    load_data = function(config, load_remote) {
      if (is.data.frame(config)) {
        if (nrow(config) > 1) {
          config = config[1,]
        }
        config = as.list(config)
      }
      super$load_data(config, load_remote)
    },
    #' @param base_only include only base table columns (TRUE) or add fields (FALSE)
    #' @return list of object attributes suitable for input to new() and from_list() methods
    to_list = function(base_only=FALSE) {
      # returns as a list, which can be set and fed back to 
      # from_list() or new(config)
      t_list <- list(
        hydroid = self$hydroid,
        name = self$name,
        hydrocode = self$hydrocode,
        ftype = self$ftype,
        fstatus = self$fstatus,
        bundle = self$bundle,
        geom = self$geom
      )
      # accounts for ODBC
      if (base_only == FALSE) {
        t_list$geom = self$geom
      }
      return(t_list)
    },
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return NULL
    from_list = function(config) {
      for (i in names(config)) {
        if (i == "hydroid") {
          if (is.na(config$hydroid)) {
            self$hydroid = NA
          } else {
            self$hydroid = as.integer(as.character(config$hydroid))
          }
        } else if (i == "name") {
          self$name = as.character(config$name)
        } else if (i == "hydrocode") {
          self$hydrocode = as.character(config$hydrocode)
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
    #' @return a dataframe of connected MPs
    get_mps = function () {
      if (self$datasource$connection_type == 'odbc') {
        sql = paste("select * from dh_feature_fielded where parent_id =",self$get_id())
        mps <- sqldf(sql,connection = self$datasource$connection)
        return(mps)
      } else {
        message("get_mps() is not enabled for non-ODBC data sources")
        return(FALSE)
      }
    },
    #' @param thismp mp entity
    #' @return add a connected MP. TBD
    add_mp = function (thismp) {
      j = length(self$mps) + 1
      #self$mps[j] <- thismp
      self$mps[j] <- list('obj' = thismp)
    },
    #' @param push_remote update locally only or push to remote database
    #' @return NULL
    save = function(push_remote=FALSE) {
      # object class responsibilities
      # - know the required elemenprop such as varid, featureid, entity_type
      #   fail if these required elemenprop are not available 
      if (push_remote) {
        finfo <- self$to_list(self$base_only)
        hydroid = self$datasource$post('dh_feature', 'hydroid', finfo)
        if (!is.logical(hydroid)) {
          self$hydroid = hydroid
        }
      }
    },
    #' @param target_entity what type to relate to (default dh_feature)
    #' @param inputs criteria to search for (list key = value format)
    #' @param operator what type of spatial function,default = st_contains
    #' @param return_geoms FALSE will return a smaller dataframe
    #' @param query_remote FALSE will search on in local datasource
    #' @return dataframe of spatially related entities
    find_spatial_relations = function(
        target_entity = 'dh_feature', 
        inputs = list(
          bundle = NA,
          ftype = NA
        ),
        operator = 'st_contains',
        return_geoms = FALSE,
        query_remote = TRUE
      ) {
      # todo: should we move this to the ODBC functions?  Needs more generic handling.
      # currently only supports dh_feature, but could later support others
      target_geomcol = 'dh_geofield_geom'
      base_geomcol = 'dh_geofield_geom'
      if (operator == 'overlaps') {
        spatial_join = paste0(' (base.', base_geomcol, ' && target.', target_geomcol,') ')
      } else if ( operator == 'st_contains' ) {
        spatial_join = paste0(' (st_contains(base.', base_geomcol, ', target.', target_geomcol,')) ')
      } else if ( operator == 'st_within' ) {
        spatial_join = paste0(' (st_within(base.', target_geomcol, ', target.', base_geomcol,')) ')
      }
      # include this in inputs for odbc routines
      input_where = paste0(
        " base.hydroid = ", self$get_id()
      ) 
      if (length(inputs[!is.na(inputs)]) > 0) {
        input_where = paste0(
          " AND ",
          fn_guess_sql_where(self$base_entity_type, self$pk_name, inputs, "target")
        )
      }
      sql = paste0("select target.*
             from dh_feature_fielded as base
             left outer join dh_feature_fielded as target
             on ( ", spatial_join, ")",
             " WHERE ", input_where
      )
      if (query_remote == FALSE) {
        message("Warning: query_remote = FALSE is not yet supported for spatial relations")
      }
      message(sql)
      related_entities <- dbGetQuery(conn = self$datasource$connection, sql)
      if (return_geoms == FALSE) {
        retcols = unlist(names(related_entities))
        retcols <- retcols[-which(retcols == "dh_geofield")]
        retcols <- retcols[-which(retcols == "dh_geofield_geom")]
        related_entities <- related_entities[,retcols]
      }
      return(related_entities)
    }
  )
)


