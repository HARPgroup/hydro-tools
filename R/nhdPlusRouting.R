# nhdPlusRouting ####
#' NHDPlus Query + Network + Flow Routing Class
#' @title nhdPlusRouting
#' @description Utility class for finding and navigating NHDPlus segments within
#' a particular POINT or feature and further routing flow across scenarios
#' @details 
#' @importFrom R6 R6Class 
#' @importFrom rlang .data
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
#' @param baseline_metric Character. The model metric to use to derive
#'   baseline metric. Default "l90_Qout" for baseline_method "riverseg". For
#'   "lrseg", a useful default is "Runit_l90"
#' @param baseline_runid Integer. Default 1000. The model scenario to derive
#'   the baseline_metric from.
#' @param maximum_divergence Integer, default 2. The highest divergence
#'   number allowed in the network. Divergences can assist with network
#'   connectivity, but be aware that divergences with value 9 may be harder
#'   to integrate
#' @return R6 Object of class nhdPlusRouting
#' @export nhdPlusRouting
#' @examples \dontrun{
#'   library(hydrotools)
#'   basepath <- '/var/www/R'
#'   source(paste0(basepath,'/config.R'))
#'   
#'   #ods connect 
#'   conn_DBI <- DEQmethods::databaseConnect("PROD")
#'   
#'   #Shenandoah Basin
#'   rseg_outlet <- RomFeature$new(ds, config = list(hydroid = 68117), TRUE)
#'   
#'   #All NHDPlus in the Shenandoah basin
#'   shen_nhd <- nhdPlusRouting$new(ds = ds, 
#'      src_feature = rseg_outlet,
#'      baseline_method = "riverseg",
#'      baseline_metric = "l90_Qout",
#'      baseline_runid = 1000,
#'      find_rseg_upstream = TRUE,
#'      maximum_divergence = 2)
#'   
#'   #Get intakes wds and ps
#'   all_dfs <- shen_nhd$get_intake_wshd_df(
#'      onedrive_location = onedrive_location,
#'      default_ps_location = 1,
#'      conn_ODS = conn_DBI)
#'      
#'   #Manually set a few PS to be realistic
#'   #City of Winchester Percy D Miller plant sends water to Opequon Creek
#'   nextID <- max(as.numeric(all_dfs$full_network_df$Hydroseq), na.rm = TRUE) + 1
#'   all_dfs$full_network_df$DnHydroseq[all_dfs$full_network_df$hydroid == 59773 & all_dfs$full_network_df$object_class == "facility_ps"] <- nextID
#'   #Frederick County Sanitation Authority - Utility Cr sends water to Opequon Creek
#'   all_dfs$full_network_df$DnHydroseq[all_dfs$full_network_df$hydroid == 60602 & all_dfs$full_network_df$object_class == "facility_ps"] <- nextID
#'   
#'   #All data:
#'   full_network_df <- all_dfs$full_network_df
#'   
#'   #Set up a scenario wd column:
#'   full_network_df$wd_intake <- full_network_df$intake_capacity
#'   full_network_df$wd_intake[is.na(full_network_df$wd_intake)] <- full_network_df$fiveyr_avg_mgd[is.na(full_network_df$wd_intake)]
#'   full_network_df$wd_intake <- full_network_df$wd_intake * 1000000*231/12/12/12/86400
#'   #Equivalent PS column
#'   full_network_df$ps_intake <- full_network_df$wd_intake * (1 - full_network_df$consumption_frac)
#'   
#'   #Ensure no duplicate withdrawals and that withdrawals are subtracted
#'   full_network_df$wd_intake <- -1  * full_network_df$wd_intake
#'   full_network_df$wd_intake[full_network_df$object_class != "facility_wd"] <- NA
#'   
#'   #Set a percentage of baseline flow to serve as available flow
#'   full_network_df$qavail_baseline <- 0.1 * full_network_df$nhd_Qbaseline
#'   
#'   #Update all network object fields
#'   shen_nhd$create_network(full_network_df, fill_network = FALSE)
#'   
#'   shen_avail <- shen_nhd$calc_routing(
  #'   nhd_baseline_col = "qavail_baseline",
  #'   wd_col = "wd_intake",
  #'   ps_col = "ps_intake",
  #'   route_col_name = "nhd_routing",
  #'   cum_route_col_name = "cumulative_routing"
#'   )
#'   shen_nhd$render_leaflet(value_col = "cumulative_routing")
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
    #' @param baseline_metric Character. The model metric to use to derive
    #'   baseline metric. Default "l90_Qout" for baseline_method "riverseg". For
    #'   "lrseg", a useful default is "Runit_l90"
    #' @param baseline_runid Integer. Default 1000. The model scenario to derive
    #'   the baseline_metric from.
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
    },
    #' @description Run a spatial intersection of the nhdplus watersheds
    #'   initialized on object with all active intakes. Return data frames of
    #'   (1) the intake, withdrawal, and nhdplus segment, (2) the intake,
    #'   potential point source return based on assumed consumption, and
    #'   estimated return flow nhdplus segment, and (3) a cleanup nhdplus data
    #'   frame that can be binded to the other data frames to compile a full
    #'   netowrk of facilities and watersheds. Each data frame will have three
    #'   potential baseline flows based on facility five year averages, intake
    #'   capacity, or pre-1989 values
    #' @param onedrive_location Character. The location of the users Onedrive
    #'   that should hold relevant foundation data
    #' @param conn_ODS Pool database connection to the ODS data base.
    #' @param default_ps_location Integer. Default 1 (next downstream segment).
    #'   How many segments downstream should point sources be returned?
    #' @return A list of four dataframes. full_network_df consists of watersheds,
    #' withdrawals, and point sources distinguished by object_class. Hydroseq
    #' and DnHydroseq may be used to navigate the network. nhdplus_df is a
    #' cleaned up version of the nhd_df field, facility_wd_df is a data frame of
    #' all withdrawals, facility_ps_df is a dataframe of all point sources based
    #' on estimated consumption and return location.
    get_intake_wshd_df = function(onedrive_location, conn_ODS, 
                                  default_ps_location = 1){
      ## nhdplus DF ####
      nhdplus_df <- make_network_df(entity_df = self$nhd_df,
                                    entity_type = "nhdplus",
                                    consumption_df = NULL)
      
      ## Foundation data ####
      found_sf <- process_foundation(onedrive_location, conn_ODS)
      
      ## Intakes ####
      #Find intakes within the NF Shenandoah
      all_intakes <- sf::st_join(self$nhd_sf, 
                                found_sf,
                                left = FALSE)
      
      ### WD Return DF ####
      facility_wd_df <- make_network_df(entity_df = all_intakes,
                                        entity_type = "wd",
                                        consumption_df = NULL)
      
      ## PS ####
      ### Consumption ####
      #Find point source consumption info:
      consumption <- get_consumption(
        ds = ds, 
        hydroids = all_intakes$facility_hydroid[!is.na(all_intakes$facility_hydroid)]
      )
        
      ### PS Return DF ####
      facility_ps_df <- make_network_df(entity_df = all_intakes,
                                        entity_type = "ps",
                                        consumption_df = consumption)
      
      ### PS Location ####
      #We can adjust where the point source is returned using the DNHydroseq
      ds_seg <- self$network_data
      ds_seg$ds_seg <- self$network$get_node_relation(neighbor_search = TRUE,
                                              value = "Hydroseq",
                                              min_dist = default_ps_location,
                                              max_dist = default_ps_location,
                                              direction = "downstream")
      #Set point sources to return to next nhdplus segment
      facility_ps_df$NextDnHydroseq <- ds_seg$ds_seg[match(facility_ps_df$DnHydroseq, ds_seg$Hydroseq)]
      #Remove list col if only one downstream seg exists for every seg
      if(all(lengths(facility_ps_df$NextDnHydroseq) == 1)){
        facility_ps_df$DnHydroseq <- unlist(facility_ps_df$NextDnHydroseq)
      }
      #Clean-up
      facility_ps_df$NextDnHydroseq <- NULL
      
      #Warn user if the ps data.frame is not of the same length as the wd
      #data.frame This may occur when there are multiple models for a given
      #facility with the same riversegment
      if(nrow(facility_ps_df) != nrow(facility_wd_df)){
        warning("The point source data frame is larger than the withdrawal
        data frame. This is likely an error and may indicate one facility
        matched to multiple models (via a join using riversegment and
        hydroid). This can only occur when there are multiple models with the
        same riverseg property for a given facility. Check data.frame for
        unwanted duplicates.")
      }
      
      #Return all data frames, and the rbind of the three to return a full
      #network ready for routing
      return(
        list(
          full_network_df = rbind(nhdplus_df, facility_wd_df, facility_ps_df),
          nhdplus_df = nhdplus_df,
          facility_wd_df = facility_wd_df,
          facility_ps_df = facility_ps_df
        )
      )
    },
    #'@description Create a field that sums baseline flow, withdrawals, and
    #'  discharges and routes the values through the network
    #'@param nhd_baseline_col The field in nhd_df that contains the baseline
    #'  metric to be used to represent nhdplus catchments
    #'@param wd_col The field in nhd_df that contains the withdrawals
    #'  (typically negative) to represent intakes and water demand
    #'@param ps_col The field in nhd_df that contains the point sources to
    #'  represent discharges and return flows. This field is priorized over
    #'  consumption_col if both are provided
    #'@param route_col_name The field to create in nhd_df to represent the sum
    #'@param cum_route_col_name The field to create in nhd_df to represent the
    #'  cumulative sum of this analysis
    #' @param allow_negative_sum Logical. Default TRUE. Can the cumulative sum
    #'   be negative? if not, set to 0 and proceed downstream.
    #'@return a data.frame containing cum_route_col_name that contains the
    #'  cumulative values of the aggregate route_col_name across the network.
    #'  Values updated to object on network and nhd_df/nhd_sf
    calc_routing = function(
      nhd_baseline_col = "nhd_Qbaseline",
      wd_col = "fiveyr_avg_mgd",
      ps_col = NULL,
      route_col_name = "nhd_routing",
      cum_route_col_name = "cumulative_routing",
      allow_negative_sum = TRUE
    ){
      #Base data
      nhd_data <- self$nhd_df
      
      if(!all(c(nhd_baseline_col, wd_col, ps_col) %in% names(nhd_data))){
        stop("One of the selected columns is not present in nhd_df, please check
             inputs")
      }
      
      #Depending on user inputs, either add point sources or calculate them as a
      #fraction of withdrawal
      if(!is.null(ps_col)){
        nhd_data[,route_col_name] <- rowSums(
          na.rm = TRUE,
          cbind( 
            nhd_data[,nhd_baseline_col],
            nhd_data[,ps_col],
            nhd_data[,wd_col]
          )
        )
      }else{
        nhd_data[,route_col_name] <- rowSums(
          na.rm = TRUE,
          cbind( 
            nhd_data[,nhd_baseline_col],
            nhd_data[,wd_col]
          )
        )
      }
      self$nhd_df <- nhd_data
      self$create_network(fill_network = FALSE)
      #Calculate the cumulative routing and set on object
      out <- self$calc_cumulative(route_col_name, out_col_name = cum_route_col_name,
                                  allow_negative_sum = allow_negative_sum)
      
      return(out)
    }
    
  ) # End public list
) # End R6
