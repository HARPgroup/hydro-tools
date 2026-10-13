# Steps for NHDPlus object:
# Steps for NHDPlus allocations:
#I. Initialize base object
#II. Get downscaled metric for each segment (area weighted if rseg)
#   1. Via rseg incremental flow via user selected metric
#   2. Via lseg l90_runit or some equivalent user selected metric
#III. Method to identify intakes in the area from the foundation dataset and to
#    create data frames of withdrawals and PS returns flows 
#      -> Should this be its own function? We do this a fair amount, at least the intersection?
#   1. Needs options to be based on 5 yr avg, intake capacity, pre 89 and PS to
#   use avg, max, or that from a certain month?
#IV. Full network compilation to incoporate withdrawals/PS data frames
#V. Water availability accumulation with options for % of base metric to use






# nhdPlusNetwork ####
#' NHDPlus Query + Network Class
#' @title nhdPlusNetwork
#' @description Utility class for finding and navigating NHDPlus segments within
#' a particular POINT or feature
#' @details This R6 object has standard methods for managing the data and meta
#'   data related to NHDPlus catchment network with VAA attributes. On
#'   initialize, the object finds intersecting NHDPlus catchments with a user
#'   input feature or POINT and can optionally find upstream catchments as well.
#'   There are methods to create \code{RomMetricNetwork} objects of the
#'   catchments and to create a leaflet to visualize the queries catchments.
#' @importFrom R6 R6Class
#' @param ds RomDataSource often provided in DEQ config files and features a
#'   local data storage or a connection to a database/RESTful service
#' @param src_feature Either a RomFeature or a POINT simple feature collection
#'   (\code{SFC}) or a one-row simple feature (\code{SF}). This feature is used
#'   to intersect the NHDPlus network. If an SFC/SF POINT is provided, all
#'   upstream catchments of the intersected catchment are returned.
#' @param find_rseg_upstream Logical, default TRUE. If \code{src_feature} is a
#'   \code{RomFeature} of a river segment (as identifed via bundle and ftype),
#'   should the NHDPlus network consist of only those intersecting src_feature
#'   (FALSE) or those intersecting src_feature and all upstream river segments
#'   (TRUE)?
#' @param handle_overlap Character, default "smallest_area". If multiple
#'   features of src_feature (when \code{find_rseg_upstream} is TRUE) intersect
#'   a single NHDPlus segment, it will be duplicated in the results.
#'   \code{handle_overlap} is to address these duplicates, with "smallest_area"
#'   keeping the intersection that occurred with the feature with the smallest
#'   area (often a subwatershed in the case of river segments)
#' @param maximum_divergence Integer, default 2. The highest divergence number
#'   allowed in the network. Divergences can assist with network connectivity,
#'   but be aware that divergences with value 9 may be harder to integrate
#' @return R6 Object of class nhdPlusNetwork
#' @export nhdPlusNetwork
#' @examples \dontrun{
#'  library(hydrotools)
#'  basepath <- '/var/www/R'
#'  source(paste0(basepath,'/config.R'))
#'  #Shenandoah Basin
#'  rseg_outlet <- RomFeature$new(ds, config = list(hydroid = 68117), TRUE)
#'  
#'  #All NHDPlus in the Shenandoah basin
#'  shenandoah_nhd <- nhdPlusNetwork$new(ds, rseg_outlet,find_rseg_upstream = TRUE,
#'                                       maximum_divergence = 2)
#'  nrow(shenandoah_nhd$nhd_df)
#'  
#'  #Create the RomMetricNetwork
#'  shenandoah_network <- shenandoah_nhd$create_network()
#'  #View network
#'  shenandoah_network$render_force_diagram()
#'  #Verify network routing
#'  accumulate_area <- shenandoah_feat$calc_cumulative("AreaSqKM","CumulativeAreaSqKM")
#'  #Some small error anticipated if network is missing some boundary segments
#'  max(accumulate_area$CumulativeAreaSqKM)
#'  shenandoah_nhd$nhd_df$TotDASqKM[which.max(shenandoah_nhd$nhd_df$TotDASqKM)]
#'  
#'  #Equivalent point query
#'   #All NHDPlus in the Shenandoah basin
#'  outlet_point <- sf::st_as_sfc("POINT (-77.78 39.27)",crs = 4326)
#'  shenandoah_wkt <- nhdPlusNetwork$new(ds, src_feature = outlet_point,
#'                                       maximum_divergence = 2)
#'  
#'  #Verify network routing
#'  accumulate_area <- shenandoah_wkt$calc_cumulative("AreaSqKM","CumulativeAreaSqKM")
#'  
#'  #Map by area:
#'  shenandoah_wkt$render_leaflet(value_col = "CumulativeAreaSqKM")
#'  #Can also color by character data:
#'  shenandoah_wkt$nhd_sf$StreamOrde <- as.character(shenandoah_wkt$nhd_sf$StreamOrde)
#'  class(shenandoah_wkt$nhd_sf$StreamOrde)
#'  shenandoah_wkt$render_leaflet(value_col = "StreamOrde")
#'  
#'  #Or with user provided colors:
#'  col_vec <- sample(x = colors()[!grepl("gr(e|a)y",colors())],size = nrow(shenandoah_wkt$nhd_sf), replace = TRUE)
#'  col_vec <- rgb(t(col2rgb(col_vec)),maxColorValue = 255)
#'  shenandoah_wkt$render_leaflet(color_vector = col_vec)
#' }
nhdPlusNetwork <- R6::R6Class(
  "nhdPlusNetwork",
  public = list(
    #' @field ds RomDataSource often provided in DEQ config files and features a
    #'   local data storage or a connection to a database/RESTful service
    ds = NA,
    #'@field src_feature sf or sfc. The spatial object that represents the
    #'  feature or point the user used to initiate the object and intersect with
    #'  the nhdplus network. This includes upstream river segments if requested
    #'  by user.
    src_feature = NA,
    #'@field max_divergence Integer. The highest divergence number allowed in
    #'  the network. Divergences can assist with network connectivity, but be
    #'  aware that divergences with value 9 may be harder to integrate
    max_divergence = NA,
    #'@field nhd_df data.frame of intersecting NHDPlus V2 catchments with VAA
    #'attributes. Will include catchments with zero area that serve to
    #'artifically connect the network.
    nhd_df = NA,
    #'@field nhd_sf sf of intersecting NHDPlus V2 catchments with VAA attributes
    #'Does not include catchments with zero area that serve to artifically
    #'connect the network.
    nhd_sf = NA,
    #'@field network \code{RomMetricNetwork} of the intersecting NHDPlus
    #'  catchments including artifical catchments of zero drainage area from
    #'  NHDPlus VAA to increase network connectivity
    network = NA,
    #' @description
    #' Initialize an instance of nhdPlusNetork by providing a valid datasource
    #' to query and a source feature. This feature can be a POINT (sfc or 1-row
    #' sf) or a RomFeature and is used to intersect the nhdPlus V2 network in
    #' the datasource. All nhdPlus features intersected will be returned. If
    #' src_feature is a riversegment, users may additionally choose to search
    #' upstream. If src_feature is a point, a recursive query is used to
    #' automatically find all upstream segments.
    #' @param ds RomDataSource often provided in DEQ config files and features a
    #'   local data storage or a connection to a database/RESTful service
    #' @param src_feature Either a RomFeature or a POINT simple feature collection
    #'   (\code{SFC}) or a one-row simple feature (\code{SF}). This feature is
    #'   used to intersect the NHDPlus network. If an SFC/SF POINT is provided,
    #'   all upstream catchments of the intersected catchment are returned.
    #' @param find_rseg_upstream Logical, default TRUE. If \code{src_feature} is a
    #'   \code{RomFeature} of a river segment (as identifed via bundle and ftype),
    #'   should the NHDPlus network consist of only those intersecting
    #'   src_feature (FALSE) or those intersecting src_feature and all upstream
    #'   river segments (TRUE)?
    #' @param handle_overlap Character, default "smallest_area". If multiple
    #'   features of src_feature (when \code{find_rseg_upstream} is TRUE)
    #'   intersect a single NHDPlus segment, it will be duplicated in the
    #'   results. \code{handle_overlap} is to address these duplicates, with
    #'   "smallest_area" keeping the intersection that occurred with the feature
    #'   with the smallest area (often a subwatershed in the case of river
    #'   segments)
    #' @param maximum_divergence Integer, default 2. The highest divergence
    #'   number allowed in the network. Divergences can assist with network
    #'   connectivity, but be aware that divergences with value 9 may be harder
    #'   to integrate
    #' @return object instance, with the \code{nhd_df} and \code{nhd_sf}
    #'   populated by results
    initialize = function(ds, src_feature,
                          find_rseg_upstream = TRUE,
                          handle_overlap = "smallest_area",
                          maximum_divergence = 2){
      self$ds <- ds
      
      if(inherits(src_feature, "RomFeature")){
        # RomFeature Init ####
        #If the user uses a RomFeature, find intersecting nhdplus geometry and
        #allow for upstream basin searches if the user enters a river segment
        src_sf <- src_feature$feat_sf
        if(!inherits(src_sf, "sf")){
          stop("No geometry found for selected src_feature",src_feature$name,
               ". This class requires a feature with valid geometry or a WKT/sf ",
               "point to identify relevant NHDPlus segments")
        }
        
        ## Find upstream rsegs ####
        if(src_feature$bundle == "watershed" & src_feature$ftype == "vahydro" &
           find_rseg_upstream){
          #Upstream segments
          rseg_network <- hydrotools::get_all_riversegs(self$ds, as_RomNetwork = TRUE)
          us_nodes <- rseg_network$get_node_relation(value = "riverseg",
                                                     src_nodes = gsub(".+_.+_.+_(.+)_.*","\\1",src_feature$hydrocode),
                                                     direction = "upstream")
          src_hydroid <- rseg_network$network_data$featureid[rseg_network$network_data$riverseg %in% us_nodes]
        }else{
          src_hydroid <- src_feature$hydroid
        }
        
        ## Intersect NHDPlus ####
        #Get the nhdplusv2 catchments that intersect with our basin
        all_nhd_geom <- DBI::dbGetQuery(
          self$ds$connection,
          paste0(
            "SELECT nhd.hydroid as nhd_hydroid, nhd.hydrocode as nhd_hydrocode
                   ,nhd.dh_geofield as geometry
                   ,user_feat.hydrocode as feat_hydrocode
                   ,user_feat.hydroid as feat_hydroid
                   ,user_feat.feat_area
                   ,user_feat.dh_geofield as feat_geometry
             FROM dh_feature_fielded as nhd
             LEFT JOIN (
               SELECT hydroid, hydrocode, dh_geofield_geom, dh_geofield,
                      ST_Area(dh_geofield_geom) as feat_area
               FROM dh_feature_fielded
               WHERE hydroid IN (",paste0(src_hydroid, collapse = ","),")
               ORDER BY ST_Area(dh_geofield_geom)
            ) as user_feat
              ON ST_Intersects(user_feat.dh_geofield_geom, ST_Centroid(nhd.dh_geofield_geom))
            WHERE nhd.bundle = 'watershed'
              AND nhd.ftype = 'nhdplusv2'
              AND user_feat.hydroid IS NOT NULL"
          )
        )
        
        ## handle_overlap ####
        if(handle_overlap == "smallest_area"){
          #Duplicates arise because of polygon overlap e.g. subwatersheds are not
          #actually removed from the polygon of their parent watershed. We need to
          #remove duplicates and remove the larger rseg area
          all_nhd_geom <- all_nhd_geom[order(all_nhd_geom$feat_area, decreasing = FALSE),]
          all_nhd_geom <- all_nhd_geom[!duplicated(all_nhd_geom$nhd_hydrocode),]
        }
        
        ## Update src_feature ####
        src_feature_df <- all_nhd_geom[,c("feat_hydroid", "feat_hydrocode", "feat_geometry")]
        src_feature_df <- src_feature_df[!duplicated(src_feature_df$feat_hydroid),]
        
        self$src_feature <- sf::st_as_sf(src_feature_df[!is.na(src_feature_df$feat_geometry),],
                                         crs = 4326,
                                         wkt = "feat_geometry")
        self$src_feature$map_popup <- paste0("Hydroid: ", self$src_feature$feat_hydroid,"<br>",
                                         "Hydrocode: ", self$src_feature$feat_hydrocode)
        
        #Remove now uneeded column to prevent confusion
        all_nhd_geom$feat_area <- NULL
        all_nhd_geom$feat_geometry <- NULL
        
      }else if(
        inherits(src_feature, "sfc_POINT") |
        (inherits(src_feature, "sf") && sf::st_geometry_type(src_feature) == "POINT")
      ){
        # SF Init ####
        #Alternatively, user can use a sfc or sf POINT to find a watershed. If
        #sf is used, only first enntry is used
        if(inherits(src_feature, "sf")){
          message("Only the first geometry of src_feature was used to intersect nhdPlus")
          src_feature <- src_feature[1,]
        }
        #Convert point to WKT to make it easier to paste to postgis
        src_wkt <- sf::st_as_text(src_feature)
        
        ## Update src_feature ####
        self$src_feature <- src_feature
        
        ## Intersect NHDPlus Recursively ####
        #Find all nhdplus segments
        all_nhd_geom <- DBI::dbGetQuery(
          self$ds$connection,
          paste0(
            "WITH RECURSIVE category_tree AS (
              SELECT f. hydroid, f.hydrocode, 
                hseq.propvalue as seq,
                dseq.propvalue as dseq,
                f.dh_geofield
              FROM dh_feature_fielded as f
              LEFT JOIN dh_properties as hseq
                ON hseq.featureid = f.hydroid
                AND hseq.propname = 'Hydroseq'
	            LEFT JOIN dh_properties as dseq
                ON dseq.featureid = f.hydroid
                AND dseq.propname = 'DnHydroseq'
	            WHERE f.bundle = 'watershed'
	              AND f.ftype = 'nhdplusv2'
	              AND ST_Intersects(ST_GeomFromText('",src_wkt,"', 4326), f.dh_geofield_geom)
              UNION ALL
              SELECT child.hydroid, child.hydrocode, child.seq, child.dseq, child.dh_geofield
              FROM (
                SELECT f. hydroid, f.hydrocode,
                  hseq.propvalue as seq,
                  dseq.propvalue as dseq,
                  f.dh_geofield
	              FROM dh_feature_fielded as f
		            LEFT JOIN dh_properties as hseq
		              ON hseq.featureid = f.hydroid
		              AND hseq.propname = 'Hydroseq'
		            LEFT JOIN dh_properties as dseq
		              ON dseq.featureid = f.hydroid
		              AND dseq.propname = 'DnHydroseq'
		            WHERE f.bundle = 'watershed' 
		              AND f.ftype = 'nhdplusv2'
              ) as child
              INNER JOIN category_tree as parent
                ON parent.seq = child.dseq
            )
            SELECT hydroid as nhd_hydroid,
              hydrocode as nhd_hydrocode,
              '",src_wkt,"' as feat_hydrocode
              ,NULL as feat_hydroid
              ,dh_geofield as geometry
            FROM category_tree"
          )
        )
        
        
      }
      
      # Properties ####
      #Get additional nhdplus properties
      all_nhd_props <- DBI::dbGetQuery(
        self$ds$connection,
        paste0(
          "SELECT f.hydroid, p.propname, p.propvalue 
           FROM dh_feature_fielded as f
           LEFT JOIN dh_properties as p
             ON p.featureid = f.hydroid
           WHERE f.hydroid IN (",paste0(all_nhd_geom$nhd_hydroid,collapse = ","),")"
        )
      )
      
      #Pivot properties wider to get all properties as their own column
      all_nhd <- all_nhd_props |> 
        tidyr::pivot_wider(id_cols = hydroid,
                           names_from = propname,
                           values_from = propvalue) |> 
        dplyr::left_join(all_nhd_geom, by = c("hydroid" = "nhd_hydroid")) |> 
        as.data.frame()
      
      # Divergence ####
      #Remove divergences below the user threshold (divergence of 9 are tricky)
      all_nhd <- all_nhd[all_nhd$Divergence <= maximum_divergence,]
      self$max_divergence <- maximum_divergence
      
      # SF version ####
      #Remove NA geometries that may be introduced in the point initialization
      #as dbase has the VAA artificial catchments with no geometry
      all_nhd_sf <- sf::st_as_sf(all_nhd[!is.na(all_nhd$geometry),],
                                 wkt = "geometry", crs = 4326)
      
      
      # Set on Self ####
      self$nhd_df <- all_nhd
      self$nhd_sf <- all_nhd_sf
    },
    
    #'@description Create a \code{RomMetricNetwork} instance populated by the
    #'  NHDPlus network derived from the initialization of this instance (or as
    #'  provided by user). This method will attempt to fill any gaps in the
    #'  network by searching NHDPlus VAA attributes for catchments with zero
    #'  drainage area that have hydroseq codes referenced by other catchments in
    #'  \code{nhd_df}. The network object is stored on this instance and the
    #'  \code{nhd_df} and \code{nhd_sf} fields are updated based on the reparied
    #'  netowrk.
    #'@param nhd_df A dataframe of NHDPlus containing, at a minimum, hydroid and
    #'  hydrocode of the NHDPlus segments. This dataframe will be converted to a
    #'  \code{RomMetricNetwork} object.
    #'@return A \code{RomMetricNetwork} object populated by the NHDPlus network
    #'provided by user or initialized in this instance.
    create_network = function(nhd_df = self$nhd_df){
      # Missing Catchments ####
      #Find potentially missing catchments - those with no drainage area that
      #assist in network connection. We do this by querying for segments below the
      #diversion threshol that have sequences in the up or down columns but not
      #in the hydrosequence column itself
      potential_missing <- DBI::dbGetQuery(
        self$ds$connection,
        paste0(
          "SELECT nhd.hydroid as nhd_hydroid, nhd.hydrocode as nhd_hydrocode
           FROM dh_feature as nhd
           LEFT JOIN dh_properties as hseq
             ON hseq.featureid = nhd.hydroid
             AND hseq.propname = 'Hydroseq'
           LEFT JOIN dh_properties as div
             ON div.featureid = nhd.hydroid
             AND div.propname = 'Divergence'
           WHERE nhd.bundle = 'watershed'
             AND nhd.ftype = 'nhdplusv2'
             AND div.propvalue <= ",self$max_divergence,"
             AND 
               (hseq.propvalue NOT IN (",paste0(nhd_df$Hydroseq, collapse = ","),")
                 AND
                   (hseq.propvalue IN (",
                     paste0(nhd_df$DnHydroseq[nhd_df$TotDASqKM != max(nhd_df$TotDASqKM, na.rm = TRUE)],collapse = ","),")
                    OR hseq.propvalue IN (",
                    paste0(nhd_df$UpHydroseq,collapse = ","),"))
            )"
        )
      )
      
      #Get nhdplus properties if any missing segments were identified
      if(nrow(potential_missing) > 0){
        ## Missing Hydroids ####
        missing_nhd_props <- DBI::dbGetQuery(
          self$ds$connection,
          paste0(
            "SELECT f.hydroid,f.hydrocode as nhd_hydrocode, p.propname, p.propvalue 
             FROM dh_feature_fielded as f
             LEFT JOIN dh_properties as p
               ON p.featureid = f.hydroid
             WHERE f.hydroid IN (",
            paste0(potential_missing$nhd_hydroid,collapse = ","),")"
          )
        )
        
        ## Missing Properties ####
        #Pivot properties wider to have them each in separate columns and
        #prepare data frame for merging with the rest of the nhd data
        potential_missing <- missing_nhd_props |> 
          tidyr::pivot_wider(id_cols = hydroid:nhd_hydrocode,
                             names_from = propname,
                             values_from = propvalue) |> 
          #Create the other fields needed for merge
          dplyr::mutate(
            feat_hydroid = NA_integer_,
            feat_hydrocode = NA_character_,    
            geometry = NA_character_
          ) |> 
          as.data.frame()
        
        all_nhd <- rbind(
         self$nhd_df[,names(self$nhd_df) %in% names(potential_missing)],
          potential_missing
        )
        
        ## Update fields ####
        ### SF version ####
        all_nhd_sf <- sf::st_as_sf(all_nhd[!is.na(all_nhd$geometry),],
                                   wkt = "geometry", crs = 4326)
        
        ### Set on Self ####
        self$nhd_df <- all_nhd
        self$nhd_sf <- all_nhd_sf
      }
      
      # Network Creation ####
      #Create a subnetwork object
      nhdplus_network <- hydrotools::RomMetricNetwork$new(
        ds = ds, datasource =  self$nhd_df,
        dest_node_col = "DnHydroseq",
        src_node_col = "Hydroseq",
        use_numeric_nodes = TRUE
      )
      ## Set on self ####
      self$network <- nhdplus_network
      
      # Return ####
      return(self$network)
    },
    #' @description
    #' Calculate the cumulative sum of a value defined by value_col from all
    #' upstream nodes of each node (defined by \code{get_node_relation()}).
    #' Inherited from \code{RomMetricNetwork}
    #' @param value_col Character. The value in network_data to sum across
    #'   upstream nodes
    #' @param out_col_name Character. Default "cumulative_sum" and represents
    #'   where output network cumulative sum should be stored.
    #' @return A data frame of network_data now containing a field named by
    #'   out_col_name for the cumulative sum of the values in value_col. Also
    #'   updates nhd_df and nhd_sf on this object.
    #' @seealso [RomMetricNetwork()]
    calc_cumulative = function(value_col, out_col_name = "cumultive_sum"){
      if(!inherits(self$network, "RomMetricNetwork")){
        self$create_network(nhd_df = self$nhd_df)
      }
      out <- self$network$calc_cumulative(value_col = value_col,
                                          out_col_name = out_col_name)
      
      # SF version ####
      #Remove NA geometries that may be introduced in the point initialization
      #as dbase has the VAA artificial catchments with no geometry
      out_sf <- sf::st_as_sf(out[!is.na(out$geometry),],
                             wkt = "geometry", crs = 4326)
      
      
      # Set on Self ####
      self$nhd_df <- out
      self$nhd_sf <- out_sf
      
      return(out)
    },
    #'@description Create a leaftlet of the returned nhdplus data. The user may
    #'  optionally color the data by a field in the nhd_sf field on this object
    #'  instance or may provide a vector of colors. The source features used to
    #'  initialize this instance may optionally be included on the leaflet.
    #'@param value_col Character, default NA. Optional. If provided, the nhdplus
    #'  polygons will be colored by the numeric or character data in value_col.
    #'  This will not be used if color_vector is also provided.
    #'@param color_vector Character, default NA. Optional. If provided, the
    #'  nhdplus polygons will be colored by the colors in color_vector. These
    #'  should be provided as color names or hexcolors.
    #'@param include_src_feature Logical. Defaults to TRUE. Should the source
    #'  feature used to initialize this object instance and identify relevant
    #'  nhdplus catchments be included on the leaflet?
    #'@return A leaflet map object
    render_leaflet = function(value_col = NA, 
                              color_vector = NA,
                              include_src_feature = TRUE){
      if(!require(leaflet)){
        warning("Leaflet is required for render_leaflet.")
      }
      ## Base map ####
      #Base leaflet map following WSPA app
      leaf_out <- leaflet::leaflet() |> 
        leaflet::addProviderTiles(providers$Esri.WorldTopoMap,group = "Esri Topo") |>
        leaflet::addProviderTiles(providers$USGS.USTopo,group = "USGS Topo") |>
        leaflet::addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") |>
        leaflet::addProviderTiles(providers$OpenStreetMap,group='Open Street Map') 
      
      # Colors ####
      #No colors provided or no user input found
      leaf_col <- "#03F"
      value_popup <- NULL
      
      #Check if user wishes to color polygons
      if(inherits(color_vector,"character") && 
         (length(color_vector) == 1 || length(color_vector) == nrow(self$nhd_sf))
      ){
        #If users has provided a vector of colors, use directly
        leaf_col <- color_vector
      }else if(!is.na(value_col)){
        if(value_col %in% names(self$nhd_sf)){
          #If user has provided value, is it numeric or character?
          value_data <- sf::st_drop_geometry(self$nhd_sf[,value_col])[,value_col]
          if(all(is.numeric(value_data))){
            ### Continuous color data####
            col_fun <- leaflet::colorNumeric("RdYlBu",
                                             domain = value_data)
            
          }else{
            ### Factor colored data####
            col_fun <- leaflet::colorFactor("Dark2",
                                            domain = factor(value_data))
          }
          #Color using function
          leaf_col <- col_fun(value_data)
          value_popup <- paste0("value_col: ", value_data,"<br>")
        }else{
          message("No value_col ", value_col," found in self$nhd_sf
          data.frame. Leaflet polygons will not be colored.")
        }
      }
      
      ## Add src polygons ####
      mapGroups <- "NHDPlusV2"
      if(include_src_feature){
        mapGroups <- c("Source Feature", mapGroups)
        #Check the geometry type to ensure the data is properly added to leaflet
        if(sf::st_geometry_type(self$src_feature, by_geometry = FALSE) == "POINT"){
          #Point data
          leaf_out <- leaf_out |> 
            leaflet::addCircleMarkers(
              data = self$src_feature, 
              group = "Source Feature", weight = 2, color = "black",
              fillColor = "grey20") 
        }else if(all(sf::st_geometry_type(self$src_feature) %in% c("POLYGON","MULTIPOLYGON"))){
          #Polygon data
          leaf_out <- leaf_out |> 
            leaflet::addPolygons(
              data = self$src_feature, 
              popup = self$src_feature$map_popup,
              group = "Source Feature", weight = 2,
              fillColor = "#777B7E",
              color = "#48494B") 
        }
      }
      
      ## Add nhd polygons ####
      leaf_out <- leaf_out |> 
        leaflet::addPolygons(
          data = self$nhd_sf, 
          popup = paste0("Hydroid:", self$nhd_sf$hydroid,"<br>",
                         "Hydrocode: ", self$nhd_sf$nhd_hydrocode,"<br> ",
                         "Hydroseq: ", self$nhd_sf$Hydroseq," -> ", self$nhd_sf$DnHydroseq,"<br>",
                         value_popup,
                         "Local D.A. -> Total D.A.:<br>",
                         round(self$nhd_sf$AreaSqKM * 100000^2 / 2.54^2 / 12^2 / 5280^2,2)," sq mi -> ",
                         round(self$nhd_sf$TotDASqKM * 100000^2 / 2.54^2 / 12^2 / 5280^2,2)," sq mi"),
          group = "NHDPlusV2", weight = 2, fillColor = leaf_col) 
      
      ## Controls ####
      leaf_out <- leaf_out |> 
        addLayersControl(baseGroups = c(
                           'Esri Topo',
                           'USGS Topo',
                           'Esri World Imagery',
                           'Open Street Map'),
                         overlayGroups = mapGroups,
                         options = layersControlOptions(collapsed = FALSE),
                         position = 'topleft')
      if(require(leafem)){
        leaf_out <- leaf_out |> 
          leafem::addMouseCoordinates()
      }
    
      return(leaf_out)
    }
    
  ) # End public list
) # End R6

