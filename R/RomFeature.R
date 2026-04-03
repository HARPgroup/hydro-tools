# RomFeature ####
#' Feature entity data object
#' @description Object for storing a single feature with attribute and
#'   timeseries related
#' @details Has standard methods for managing data and meta data for features in
#'   the dh_feature table within the OWS database. \code{RomFeature} provides
#'   users an easy way to query for a feature and then get additional data like
#'   measuring points, properties, and spatial information. Inherits additional
#'   methods from \code{RomEntity}
#' @importFrom R6 R6Class  
#' @param datasource optional RomDataSource for remote and local storage
#'   (required; often provided in DEQ config files)
#' @param config list of attributes to use to query for the feature. May include
#'   a hydroid, a name, a hydrocode, a bundle, an ftype, or other
#'   \code{RomFeature} fields
#' @return Instance of RomFeature populated by returned query attributes or by
#'   config inputs
#' @seealso RomProperty
#' @examples \dontrun{
#'#Get new datasource via odbc
#'feat <- RomFeature$new(ds, config = list(hydroid = 68048), TRUE)
#'#Pointer to external db
#'feat$plot_feat(TRUE)
#'}
#' @export RomFeature
RomFeature <- R6Class(
  "RomFeature",
  inherit = RomEntity,
  public = list(
    #' @field base_entity_type kind of entity (general OWS database structure)
    base_entity_type = 'dh_feature',
    #' @field pk_name The name of this entity's primary key column that can be
    #'   used as a unique identifier for this entity
    pk_name = 'hydroid',
    #' @field name The name of the feature
    name = NA,
    #' @field hydrocode Code for this entity from original dataset (sometimes
    #'   alpha code)
    hydrocode = NA,
    #' @field ftype Feature type. Depends on the bundle. See hydrotools ReadMe
    #'   for additional infomraiton but these may include use types and much
    #'   more
    ftype = NA,
    #' @field hydroid unique ID (integer) to identify features (primary key)
    hydroid = NA,
    #' @field bundle main content type, i.e. facility, well, intake, ...
    bundle = NA,
    #' @field fstatus entity status e.g. active, inactive, abandonded, etc.
    fstatus = NA,
    # list of object references?  or list format name, value, ...
    #' @field description notes field
    description = NA,
    #' @field mps linked features that may represent measuring points like
    #'   intakes, wells, etc.
    mps = NA,
    #' @field geom feature geometry WKT
    geom = NA,
    #' @field geom_CRS What is the coordinate system to use to convert geom
    #'   field to SF?
    geom_CRS = 4326,
    #' @field feat_sf An SF data frame to represent the feature
    feat_sf = NA,
    #' @field nextdown_id "Downstream" entity where applicable
    nextdown_id = NA,
    #' @field parent_id "Upstream" entity where applicable
    parent_id = NA,
    #' @field sql_select_from syntax to use to select via an odbc or other SQL based datasource
    sql_select_from = "
      select * from dh_feature_fielded
    ",
    #' @field base_only - how to export to list in case of complex multi table entity and ODBC
    base_only = FALSE,
    #' @description
    #' Initialize an instance of RomFeature. This method will create an
    #' instance of RomFeature populated with fields based on the user query
    #' inputs passed via config. This method first calls the RomEntity inherited
    #' \code{RomEntity$initialize()} to populate fields then posts the feature
    #' in the internal feature database in the datasource (via
    #' \code{self$datasource$set_feature()}). 
    #' @param datasource RESTful repository object
    #' @param config list of attributes to set, see also: to_list() for format
    #' @param load_remote automatically query REST dataa source for matches?
    #' @return object instance with populated fields based on user config and
    #'   methods to manipulate the feature
    initialize = function(datasource = NULL, config, load_remote = FALSE) {
      #col.names(self$properties <-
      super$initialize(datasource, config, load_remote)
      # experimental support for automatic local caching
      self$datasource$set_feature(self$to_list())
      self$mps = list()
    },
    #' @description A simple function that returns the primary key associated
    #'   with this feature. There should be a \code{get_id} on all children of
    #'   \code{RomEntity} objects to allow easy query
    #' @return get_id the unique id of this entity alias to remote pkid,
    #'   subclassed as function.
    get_id = function() {
      return(self$hydroid)
    },
    #' @description
        #' Populates fields on the object based on the queried results from the
        #' user config. It will save a copy of the feature to the local
        #' repository. The feature plugin and variable definitions will be
        #' loaded automatically as the inherited \code{RomEntity$load_data()} is
        #' called. The SF field will then be populated by any geometry on the
        #' feature
    #' @param config Populated by the config field (provided by user)
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
      self$wkt_to_sf()
    },
    #' @description
    #' Create a list of all the fields on this feature object instance to allow
    #' easy data sharing with R
    #' @param base_only include only base table columns (TRUE) or add fields
    #'   (FALSE)
    #' @return list of object attributes suitable for input to \code{new()} and
    #'   \code{from_list()} methods
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
    #' @description Populate fields of this object based on the named entries in
    #'   list config. Searches specifically for hydroid, name, hydrocode, ftype,
    #'   bundle, fstatus, dh_geofield, nextdown_id, parent_id
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return Object instance now with fields populated by config
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
    #' @description Queries dh_feature_fielded for any entities that have a
    #'   parent_id equal to this objects ID. These will include a facility's
    #'   measuring points (wells, intakes, etc.)
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
    #' @description Add a connected MP to local mp storage (mps field). This
    #'   will hopefully be expanded to include remote saving.
    #' @param thismp mp entity
    #' @return add a connected MP to local mp storage.
    add_mp = function (thismp) {
      j = length(self$mps) + 1
      #self$mps[j] <- thismp
      self$mps[j] <- list('obj' = thismp)
    },
    #' @description
    #' Save changes to this feature to remote or local database based on
    #' push_remote by first getting all fields via \code{self$to_list()} and
    #' then subsequently using the \code{self$datasource$post()} as described in
    #' \code{RomDataSource()}
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
    #' @description Find target entities via spatial query of this RomFeature
    #' @param target_entity what type to relate to (default dh_feature)
    #' @param inputs Criteria to search for entered as a list key = value
    #'   format. For instance, the list may contain a bundle or ftype via
    #'   \code{list(bundle = 'watershed', ftype = 'vahydro')}
    #' @param operator what type of spatial function to perform to search for
    #'   entities, default to 'st_contains'. Other options include
    #'   'st_contains_centroid' (is the centroid of the other feature contained
    #'   in this feature?),'st_centroid_within' (is the centroid of this feature
    #'   in another feature?), 'overlaps', or 'st_within'
    #' @param return_geoms FALSE will return a smaller dataframe by excluding
    #'   the geometry fields. "SF" will return an SF and TRUE will return a data
    #'   frame
    #' @param query_remote FALSE will search on in local datasource only, TRUE
    #'   will search the DS connection
    #' @param plot_results If TRUE, a plot will be included in the return that
    #'   shows the base feature and the related entities returned from the query
    #' @return If plot_results is FALSE, a dataframe or SF dataframe of
    #'   spatially related entities is returned bsaed on returns_geom. If
    #'   plot_results is TRUE, then a list is returned with an entry plot that
    #'   is the ggplot of the feature and queried entities and the other entry,
    #'   spatial_relations, is a data.frame or SF data.frame of the related
    #'   entities
    find_spatial_relations = function(
        target_entity = 'dh_feature', 
        inputs = list(
          bundle = NA,
          ftype = NA
        ),
        operator = 'st_contains',
        return_geoms = FALSE,
        query_remote = TRUE,
        plot_results = FALSE
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
      } else if ( operator == 'st_centroid_within' ) {
        spatial_join = paste0(' (st_contains(target.', target_geomcol,', st_centroid(base.', base_geomcol, '))) ')
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
      query_results <- dbGetQuery(conn = self$datasource$connection, sql)
      #If returns_geom is FALSE, remove the geometry fields
      if (return_geoms == FALSE) {
        retcols = unlist(names(query_results))
        retcols <- retcols[-which(retcols == "dh_geofield")]
        retcols <- retcols[-which(retcols == "dh_geofield_geom")]
        related_entities <- query_results[,retcols]
      }else if(return_geoms == "SF"){
        #If returns_geom is SF, try to convert to an SF object to return
        tryCatch(
          {
            related_entities <- sf::st_as_sf(
              wkt = "dh_geofield", crs = self$geom_CRS,
              x = query_results)
            sf::st_geometry(related_entities) <- "geom"
          }, error = function(e) {
            message("Could not create SF object from WKT due to:")
            message(e)
          }
        )
      }
      #If the user wishes to plot the results, plot the feature and the returned
      #data frame. then, return a list with the plot and the results of the
      #query. Otherwise, just return spatial_relations
      if(plot_results){
        #Create an sf object plot_entities, if not already created
        if(!inherits(related_entities, "sf")){
          plot_entities <- sf::st_as_sf(
            wkt = "dh_geofield", crs = self$geom_CRS,
            x = query_results)
          sf::st_geometry(plot_entities) <- "geom"
        }else{
          plot_entities <- related_entities
        }
        p <- self$plot_feat(useggplot = TRUE, 
                       other_geom = list("Spatial_Relations" = plot_entities))
        return(list(spatial_relations = related_entities, plot = p))
      }else{
        return(related_entities) 
      }
    },
    #' @description Get raw or summarized raster values for this RomFeature from
    #'   dh_timeseries_weather using PostGIS
    #' @param varkey What variable to retrieve? This should be a varkey that has
    #'   been used to create raster summaries in dh_timeseries_weather. Relevant
    #'   meteorology varkeys include 'prism_mod_daily', 'daymet_mod_daily',
    #'   'nldas2_obs_hourly', 'nldas2_precip_hourly_tiled_16x16',
    #'   'nldas2_precip_daily', 'amalgamate_simple_lm', 'amalgamate_storm_vol'
    #' @param starttime begin date-time to limit query (default FALSE to remove a
    #'   tstime filter)
    #' @param endtime last date-time to limit query (default to Sys.Date())
    #' @param band which raster band to query (default = 1)?
    #' @param aggregate Should data be aggregated and if so, how? FALSE will
    #'   return 1 record for every date. Otheriwse, can use basic aggregate
    #'   functions like mean, min, max to summarize data
    #' @param metric default mean, which aspect of the ST_SummaryStats() to return
    #' @param touched Defaults to FALSE. Should PostGIS ST_Clip() use the
    #'   touched argument to return all raster cells touched by the feature? Or,
    #'   if FALSE, should only pixels that have centroids within the feature be
    #'   considered?
    #' @return dataframe of timeseries values
    get_raster_ts = function(
        varkey = 'prism_mod_daily',
        starttime = FALSE,
        endtime = Sys.Date(),
        band = '1',
        aggregate = FALSE,
        metric = 'mean',
        touched = FALSE
      ) {
      # looks at the table dh_timeseries_weather
      # coverage raster summary
      ftype = self$ftype
      bundle = self$bundle
      hydrocode = self$hydrocode
      #Default to all records unless otherwise input by user
      startclause = "(1 = 1)"
      endclause = "(1 = 1)"
      #If a start or endtime has been provided, set a psql where clause that
      #ensures tstime or tsendtime comply with user input filter
      if (!is.logical(starttime)) {
        startclause = paste0("met.tstime >= extract(epoch from '",starttime,"'::date)")
      }
      if (!is.logical(endtime)) {
        endclause = paste0("met.tsendtime <= extract(epoch from '",endtime,"'::date)")
      }
      #If the user wishes to apply an aggregate function, group the data by the
      #featureid and apply the aggregate and metric arguments. Otherwise, just
      #get the metric from st_summarystats. The data will be clipped to the
      #feature and the user can choose to apply the touched argument from
      #postGIS st_Clip
      if(touched){
        clipSQL <- paste0("st_clip(met.rast, fgeo.dh_geofield_geom,touched => true)")
      }else{
        clipSQL <- paste0("st_clip(met.rast, fgeo.dh_geofield_geom,touched => false)")
      }
      if (!is.logical(aggregate)) {
        rastercalc = fn$paste0("$aggregate((ST_summarystats($clipSQL, 1, TRUE)).$metric)")
        groupby = "GROUP BY met.featureid"
        startcol = "to_timestamp(min(met.tsendtime))"
        endcol = "to_timestamp(max(met.tsendtime))"
      } else {
        rastercalc = fn$paste0("(ST_summarystats($clipSQL, 1, TRUE)).$metric")
        groupby = ""
        startcol = "to_timestamp(met.tsendtime)"
        endcol = "to_timestamp(met.tsendtime)"
      }
      #Query the DB
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
      raster_records <- DBI::dbGetQuery(conn = self$datasource$connection, sql)
      return(raster_records)
    },
    #' @description Convert the WKT field geom of this entity to an SF data
    #'   frame for easy R GIS analysis.
    #' @returns Nothing, but will try to set the feat_sf field. Will message
    #'   errors if encountered.
    wkt_to_sf = function() {
      #Grab all feature fields including geometry
      feat_data <- self$to_list()
      #Convert to an SF object within a try-catch. If an error is thrown, warn
      #user that SF was not created
      tryCatch(
        {
          self$feat_sf <- sf::st_as_sf(
            wkt = "geom", crs = self$geom_CRS,
            as.data.frame(feat_data)
          )
        }, error = function(e) {
          message("Could not create SF object from WKT due to:")
          message(e)
          return(NA)
        }
      )
    },
    #' @description Plot this feature on a basic GIS map for QC purposes. If the
    #'   package ggspatial is loaded, plot may be returned as a ggplot
    #' @param useggplot Defaults to TRUE. If TRUE, a ggplot is returned using
    #'   ggspatial; if FALSE, a plot is sent to the viewer
    #' @param other_geom Named list of other SF data frames to include on plot
    #'   (only used in ggplot)
    #' @returns If useggplot is FALSE, nothing is returned but a plot is
    #'   printed to the viewer. If useggplot is TRUE, a ggplot object is
    #'   returned
    plot_feat = function(useggplot = TRUE, other_geom = list()) {
      if(any(!is.na(self$feat_sf))){
        #Use Esri base map
        tileProvider <- "Esri.NatGeoWorldMap"
        #A base object to use to get map tiles. May be replaced by bounding box
        #for a polygon if multiple geometries are given
        maptiles_obj <- self$feat_sf
        #Bounding box of feature or of the largest other geometry
        bbox <- sf::st_bbox(self$feat_sf)
        if(is.list(other_geom) &
            length(other_geom) > 0 &
            !is.null(names(other_geom))){
          #Get the bboxes of each object in other_geom and add to a matrix with
          #the bbox from self$feat_sf
          bboxs <- do.call(rbind,
                          c(list(bbox),
                            lapply(
                              lapply(other_geom,st_geometry),
                              st_bbox))
          )
          #Get the bbox with the maximum extent
          maptiles_obj <- st_bbox(
            c(xmin = min(bboxs[,1]), ymin = min(bboxs[,2]),
              xmax = max(bboxs[,3]), ymax = max(bboxs[,4])),
            crs = self$geom_CRS
          )
        }
        
        #Get background map tiles from Esri at an appropriate zoom
        tiles <- maptiles::get_tiles(maptiles_obj,
                                     zoom = set_zoom(bbox), 
                                     crop = FALSE, verbose = FALSE, 
                                     provider = tileProvider)
        if(useggplot){
          #Alternate, but requires ggspatial:
          p <- ggplot2::ggplot() +
            ggspatial::layer_spatial(tiles) +
            ggplot2::geom_sf(data = self$feat_sf, fill = NA, lwd = 1) + 
            ggplot2::theme_minimal()
          #If other geometries are provided in a named list, plot them with
          #unique colors
          if(is.list(other_geom) &
             length(other_geom) > 0 &
             !is.null(names(other_geom))){
            #For each SF in other_geom, add a ggplot geom_sf layer with a new
            #color controlled by that list entry name
            new_layers <- lapply(
              X = 1:length(other_geom),
              other_geom = other_geom,
              FUN = function(X, other_geom){
                geom_sf(data = other_geom[[X]], fill = NA, lwd = 1,
                        mapping = aes(color = names(other_geom)[X])
                )
              })
            #Create unique colors based on the length of other_geom
            plot_colors <- grDevices::heat.colors(length(other_geom))
            #Add a legend and apply colors to the new_layers
            p <- p + new_layers + 
              ggplot2::scale_color_manual(values = plot_colors) + 
              ggplot2::labs(color = "Key")
          }
          return(p)
        }else{
          #Plot feature and tiles
          terra::plotRGB(tiles, axes = TRUE,mar = c(2,1,1,1))
          plot(add = TRUE,self$feat_sf$geom)
          if(is.list(other_geom) & length(other_geom) > 0){
            #Create unique colors based on the length of other_geom
            plot_colors <- grDevices::heat.colors(length(other_geom))
            lapply(
              X = 1:length(other_geom),
              other_geom = other_geom,
              plot_colors = plot_colors,
                   FUN = function(X,other_geom, plot_colors){
                     plot(add = TRUE, other_geom[[X]]$geom,
                          border = plot_colors[X])
              })
          }
        }
      }else{
        warning("feat_sf object must be a valid SF object to plot")
      }
    }
  )
)


