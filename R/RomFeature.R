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
    #' @field nextdown_id feature geometry WKT
    nextdown_id = NA,
    #' @field parent_id feature geometry WKT
    parent_id = NA,
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
    #' @return the data from the remote connection
    load_data = function(config, load_remote) {
      #message(paste("load_data() called "))
      if (is.data.frame(config)) {
        if (nrow(config) >= 1) {
          config <- as.list(config[1,])
        }
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
        nextdown_id = self$nextdown_id,
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
        } else if (i == "nextdown_id") {
          self$nextdown_id = as.integer(config$nextdown_id)
        } else if (i == "parent_id") {
          self$parent_id = as.integer(config$parent_id)
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
        #Dont send geometry to dh_feature, won't exist as a field
        finfo <- finfo[names(finfo) != 'geom']
        finfo <- finfo[names(finfo) != 'nextdown_id']
        hydroid = self$datasource$post('dh_feature', 'hydroid', finfo)
        if (!is.logical(hydroid)) {
          self$hydroid = hydroid
        }
      }
    },
    #' @param target_entity what type to relate to (default dh_feature)
    #' @param inputs criteria to search for (list key = value format)
    #' @param operator what type of spatial function,default = st_contains
    #'   Other options are 'overlaps' or 'st_within'
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
      } else if ( operator == 'st_overlaps' ) {
        # same as overlaps
        spatial_join = paste0(' (base.', base_geomcol, ' && target.', target_geomcol,') ')
      } else if ( operator == 'st_contains' ) {
        spatial_join = paste0(' (st_contains(base.', base_geomcol, ', target.', target_geomcol,')) ')
      } else if ( operator == 'st_within' ) {
        spatial_join = paste0(' (st_within(base.', target_geomcol, ', target.', base_geomcol,')) ')
      } else if ( operator == 'st_contains_centroid' ) {
        spatial_join = paste0(' (st_contains(base.', target_geomcol, ', st_centroid(target.', base_geomcol,'))) ')
      }
      # include this in inputs for odbc routines
      input_where = paste0(
        " base.hydroid = ", self$get_id()
      ) 
      if (length(inputs[!is.na(inputs)]) > 0) {
        input_where = paste0(
          input_where,
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
    },
    #' @param varkey what variable to retrieve
    #' @param starttime begin tstime (default FALSE)
    #' @param endtime last tsendtime (default FALSE)
    #'   Other options are 'overlaps' or 'st_within'
    #' @param band which band to query (default = 1)
    #' @param aggregate FALSE will return 1 record for every date
    #' @param metric default mean, which aspect of the ST_SUMMARIZE() to return
    #' @return dataframe of spatially related entities
    get_raster_ts = function(
        varkey = 'prism_mod_daily',
        starttime = FALSE,
        endtime = '2025-10-13',
        band = '1',
        aggregate = FALSE,
        metric = 'mean'
      ) {
      # looks at the table dh_timeseries_weather
      # coverage raster summary
      ftype = self$ftype
      bundle = self$bundle
      hydrocode = self$hydrocode
      startclause = "(1 = 1)"
      endclause = "(1 = 1)"
      if (!is.logical(starttime)) {
        startclause = fn$paste0("met.tstime >= extract(epoch from '$starttime'::date)")
      }
      if (!is.logical(endtime)) {
        endclause = fn$paste0("met.tsendtime <= extract(epoch from '$endtime'::date)")
      }
      if (!is.logical(aggregate)) {
        rastercalc = fn$paste0("$aggregate((ST_summarystats(st_clip(met.rast, fgeo.dh_geofield_geom), 1, TRUE)).$metric)")
        groupby = "GROUP BY met.featureid"
        startcol = "to_timestamp(min(met.tsendtime))"
        endcol = "to_timestamp(max(met.tsendtime))"
      } else {
        rastercalc = fn$paste0("(ST_summarystats(st_clip(met.rast, fgeo.dh_geofield_geom), 1, TRUE)).$metric")
        groupby = ""
        startcol = "to_timestamp(met.tsendtime)"
        endcol = "to_timestamp(met.tsendtime)"
      }
      sql = fn$paste0(
        "WITH feature_coverages AS (
        SELECT * 
          FROM  dh_feature
        WHERE hydrocode = '$hydrocode'
        AND ftype = '$ftype'
        AND bundle = '$bundle'
        ),
        metUnion as (
          Select met.featureid, met.tsendtime,
          st_union(met.rast) as rast
          FROM feature_coverages as f
          left outer join field_data_dh_geofield as fgeo
          on (
            fgeo.entity_id = f.hydroid
            and fgeo.entity_type = 'dh_feature' 
          ) 
          JOIN(
            select *
              from dh_timeseries_weather as met
            left outer join dh_variabledefinition as b
            on (met.varid = b.hydroid) 
            WHERE $startclause
            AND $endclause
            and b.varkey='$varkey'
          ) AS met
          ON ST_Intersects(ST_ConvexHull(met.rast),fgeo.dh_geofield_geom)
          
          group by met.featureid, met.tsendtime
        )
        SELECT met.featureid, 
          $startcol as start_date,
          $endcol as end_date,
          $rastercalc as value
        FROM feature_coverages as f
        left outer join field_data_dh_geofield as fgeo
        on (
          fgeo.entity_id = f.hydroid
          and fgeo.entity_type = 'dh_feature' 
        ) 
        JOIN metUnion AS met
        ON ST_Intersects(ST_ConvexHull(met.rast),fgeo.dh_geofield_geom)
        $groupby"
      )
      # to debug set ds$debug = TRUE instead of message(sql)
      raster_records <- dbGetQuery(conn = self$datasource$connection, sql)
      return(raster_records)
    }
  )
)


