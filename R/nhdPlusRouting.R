# nhdPlusRouting ####
#' NHDPlus Query + Network + Flow Routing Class
#' @title nhdPlusRouting
#' @description Utility class for finding and navigating NHDPlus segments within
#' a particular POINT or feature and further routing flow across scenarios
#' @details 
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
#' @return R6 Object of class nhdPlusRouting
#' @export nhdPlusRouting
#' @examples \dontrun{

#' }
nhdPlusRouting <- R6::R6Class(
  "nhdPlusRouting",
  inherit = nhdPlusNetwork,
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
    #' @description Initialize an instance of nhdPlusRouting to get a network of
    #'   nhdPlus V2 catchments that intersect the user provided feature using
    #'   the inherited methods of \code{nhdPlusNetwork}. These will initialize
    #'   with a baseline metrics scaled by area (riverseg) or through unit area
    #'   compilation (lrseg) and be provided in a separate field. 
    #' @param ds RomDataSource often provided in DEQ config files and features a
    #'   local data storage or a connection to a database/RESTful service
    #' @param src_feature Either a RomFeature or a POINT simple feature
    #'   collection (\code{SFC}) or a one-row simple feature (\code{SF}). This
    #'   feature is used to intersect the NHDPlus network. If an SFC/SF POINT is
    #'   provided, all upstream catchments of the intersected catchment are
    #'   returned.
    #' @param find_rseg_upstream Logical, default TRUE. If \code{src_feature} is
    #'   a \code{RomFeature} of a river segment (as identifed via bundle and
    #'   ftype), should the NHDPlus network consist of only those intersecting
    #'   src_feature (FALSE) or those intersecting src_feature and all upstream
    #'   river segments (TRUE)?
    #' @param handle_overlap Character, default "smallest_area". If multiple
    #'   features of src_feature (when \code{find_rseg_upstream} is TRUE)
    #'   intersect a single NHDPlus segment, it will be duplicated in the
    #'   results. \code{handle_overlap} is to address these duplicates, with
    #'   "smallest_area" keeping the intersection that occurred with the feature
    #'   with the smallest area (often a subwatershed in the case of river
    #'   segments)
    #' @param baseline_method Character. Default "riverseg". Should the baseline
    #'   for each nhdplus catchment be calculated by downscaling the incremental
    #'   metric at each river segment ("riverseg") or by area aggregation in
    #'   each land-river segment ("lrseg").
    #' @param baseline_metric
    #' @param baseline_runid
    #' @param maximum_divergence Integer, default 2. The highest divergence
    #'   number allowed in the network. Divergences can assist with network
    #'   connectivity, but be aware that divergences with value 9 may be harder
    #'   to integrate
    #' @return An instance of nhdPlusRouting with the data frame, SF, and
    #'   network fields populated
    initialize = function(ds, src_feature,
                          find_rseg_upstream = TRUE,
                          handle_overlap = "smallest_area",
                          maximum_divergence = 2,
                          baseline_method = "riverseg",
                          baseline_metric = "l90_Qout",
                          baseline_runid = 1000){
      self$ds <- ds
      #Initialize the base class to find relevant NHDPlus catchments
      super$initialize(ds, src_feature = src_feature,
                       handle_overlap = handle_overlap,
                       find_rseg_upstream = find_rseg_upstream,
                        maximum_divergence = maximum_divergence)
      #Create the RomMetricNetwork and fill in artiical VAA Catchments
      super$create_network(nhd_df = self$nhd_df)
      
      #Calculate baseline metric
      if(baseline_method == "riverseg"){
        # Get NHDPlus Baseline Flow ####
        ## base rseg network ####
        rsegs <- hydrotools::get_all_riversegs(ds)
        names(rsegs) <- c("riverseg","featureid","hydrocode","propname")
        rseg_network <- hydrotools::RomMetricNetwork$new(
          ds = ds, datasource =  rsegs,
          force_unique_src = TRUE,
          src_node_col = "riverseg"
        )
        
        ## rseg_data ####
        #Get all riversegments and geometries in a dataframe with their
        #incremental baseline flow and incremental DA spatial join onto the
        #nhdplus nf_nhd sf. Now you have DA and baseline flow per row so easier
        #to divie out to each cathcment
        rseg_data <- DBI::dbGetQuery(
          conn = ds$connection,
          paste0("SELECT feat.hydroid, feat.hydrocode, feat.name
            ,model.pid
            ,runprop.propvalue as Qbaseline
            ,ST_AsText(feat.dh_geofield) as rseg_wkt
            ,COALESCE(lcarea.propvalue, rctotalarea.propvalue) as coalesce_totalarea
          FROM dh_feature_fielded as feat
          LEFT JOIN dh_properties as model
            ON model.featureid = feat.hydroid
            AND model.propcode = 'vahydro-1.0'
            AND model.varid = 1418
          --Join runid on to get the baseline metric from baseline runid
          LEFT JOIN dh_properties as runid
            ON model.pid = runid.featureid
            AND runid.propname = 'runid_",baseline_runid,"'
          LEFT JOIN dh_properties as runprop
            ON runprop.featureid = runid.pid
            AND runprop.propname = '",baseline_metric,"'
          --Join river channel on for nodes to get total area and incremental area
          LEFT JOIN dh_properties as rc
            ON rc.featureid = model.pid
            AND rc.propname = '0. River Channel'
          LEFT JOIN dh_properties as rcitarea
            ON rcitarea.featureid = rc.pid
            AND rcitarea.propname = 'area'
          LEFT JOIN dh_properties as rctotalarea
            ON rctotalarea.featureid = rc.pid
            AND rctotalarea.propname = 'drainage_area'
          --Join local channel on for subwatersheds to get area
          LEFT JOIN dh_properties as lc
            ON lc.featureid = model.pid
            AND lc.propname = 'local_channel'
          LEFT JOIN dh_properties as lcarea
            ON lcarea.featureid = lc.pid
            AND lcarea.propname = 'area'
          --Only features in upstream list
          WHERE feat.hydroid IN (",paste0(self$src_feature$feat_hydroid,collapse = ","),")
         ORDER BY COALESCE(lcarea.propvalue, rctotalarea.propvalue)")
        )
        
        #Use network to calculate an incremental flow
        rseg_data$src_node <- gsub("vahydrosw_wshed_.+_([0-9]+)_(.+)","\\1",rseg_data$hydrocode)
        rseg_data$uprseg <- rseg_network$get_node_relation(value = "hydrocode",neighbor_search = TRUE,
                                                              max_dist = 1, min_dist = 1,
                                                              src_nodes = rseg_data$src_node,
                                                              direction = "upstream")
        
        #Subtract upstream flow to get incremental flow and area for this segment
        rseg_data$upstreamFlow <- mapply(thisUS = rseg_data$uprseg,
                                            MoreArgs = list(rseg_data = rseg_data),
                                            FUN= function(thisUS, rseg_data){
                                              if(length(thisUS) > 0){
                                                usflow <- rseg_data$qbaseline[rseg_data$hydrocode %in% thisUS]
                                                out <- sum(usflow)
                                              }else{
                                                out <- 0
                                              }
                                              return(out)
                                            })
        rseg_data$upstreamArea <- mapply(thisUS = rseg_data$uprseg,
                                            MoreArgs = list(rseg_data = rseg_data),
                                            FUN= function(thisUS, rseg_data){
                                              if(length(thisUS) > 0){
                                                usflow <- rseg_data$coalesce_totalarea[rseg_data$hydrocode %in% thisUS]
                                                out <- sum(usflow)
                                              }else{
                                                out <- 0
                                              }
                                              return(out)
                                            })
        
        rseg_data$incrementalFlow <- rseg_data$qbaseline - rseg_data$upstreamFlow
        rseg_data$incrementalArea <- rseg_data$coalesce_totalarea - rseg_data$upstreamArea
        
        ## nhd_feat ####
        nhd_feat <- self$nhd_df |> 
          dplyr::left_join(rseg_data, by = c("feat_hydroid" = "hydroid",
                                                "feat_hydrocode" = "hydrocode"))
        ## Downscale Flow ####
        nhd_feat$nhd_Qbaseline <- 0 
        nhd_feat$incrementalArea[is.na(nhd_feat$incrementalArea)] <- 0
        nhd_feat$nhd_Qbaseline[nhd_feat$incrementalArea > 0] <- 
          nhd_feat$incrementalFlow[nhd_feat$incrementalArea > 0] * (nhd_feat$AreaSqKM[nhd_feat$incrementalArea > 0] * 100000^2 / 2.54^2 / 12^2 / 5280^2) / nhd_feat$incrementalArea[nhd_feat$incrementalArea > 0]
        
      }else if(baseline_method == "lrseg"){
        ## lseg ####
        lrseg_data <- DBI::dbGetQuery(
          conn = ds$connection,
          paste0("SELECT nhd.hydroid as nhd_hydroid, nhd.hydrocode as nhd_hydrocode
            ,feat.hydroid as feat_hydroid, feat.hydrocode as feat_hydrocode
            , feat.name as feat_name, feat.ftype
            ,model.pid
            ,runprop.propvalue as runit_baseline
            ,ST_Area(
              ST_Transform(
                ST_Intersection(feat.dh_geofield_geom, nhd.dh_geofield_geom),
                26918
              )
             ) * (10000 / 2.54 / 2.54 /12 /12 / 5280 / 5280) as intersectArea
            ,ST_AsText(feat.dh_geofield) as lseg_wkt
          FROM dh_feature_fielded as nhd
          LEFT JOIN (
             SELECT * 
             FROM dh_feature_fielded
             WHERE bundle = 'landunit'
             AND ftype IN ('cbp532_lrseg', 'cbp6_lrseg')
          ) as feat
          ON ST_Intersects(feat.dh_geofield_geom, nhd.dh_geofield_geom)
          LEFT JOIN dh_properties as model
            ON model.featureid = feat.hydroid
            AND model.propcode = 'vahydro-1.0'
            --AND model.varid IN (1360, 1394)
          --Join runid on to get the baseline metric from baseline runid
          LEFT JOIN dh_properties as runid
            ON model.pid = runid.featureid
            AND runid.propname = 'runid_",baseline_runid,"'
          LEFT JOIN dh_properties as runprop
            ON runprop.featureid = runid.pid
            AND runprop.propname = '",baseline_metric,"'
          WHERE nhd.hydroid IN (",paste0(self$nhd_df$hydroid,collapse = ","),")
          AND model.propcode IS NOT NULL
         ")
        )
        #A lot of 532 lrsegs dont seem to have vahydro-1.0 models. Maybe they
        #only exist in the Southern rivers? Would need to test.
        ## Downscale Flow ####
        #Calculate baseline flow as intersecting area of the lrseg and nhdplus
        #catchment by unit area runoff
        lrseg_data$lrseg_Qbaseline <- lrseg_data$intersectarea * lrseg_data$runit_baseline
        #Total flow into this segment should then be the sum of the unit area
        #runoff multiplied by the area of the nhdplus catchment. So, total unit
        #area runoff is:
        lrseg_runit <- lrseg_data |> 
          dplyr::group_by(nhd_hydroid) |> 
          dplyr::summarize(nhd_Qbaseline = sum(lrseg_Qbaseline, na.rm = TRUE))
        
        
        ## nhd_feat ####
        nhd_feat <- self$nhd_df |> 
          dplyr::left_join(lrseg_runit, by = c("hydroid" = "nhd_hydroid"))
      }
      
      # Set updated dfs and network on self ####
      super$create_network(nhd_df = nhd_feat)
    }
    
  ) # End public list
) # End R6

