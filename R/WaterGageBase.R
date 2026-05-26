# WaterGageBase ####
#' Water Gage Data Information Object
#' @title WaterGageBase
#' @description Utility class for compiling query info for stream gage analysis
#' @details This R6 object has standard methods for managing the data and meta
#'   data related to stream gage data. On initialize, this object sets fields
#'   relevant for stream flow data querying and analysis. This object can retrieve
#'   spatial data about a gage, but is not used to query streamflow itself as it
#'   is instead inherited by subclasses like WaterGageDaily that can query data
#'   instead
#' @importFrom R6 R6Class  
#' @param config A named list containing the names of fields to set on the
#'   object. This may contain any of the named public fields on WaterGageBase or
#'   on the inherited object i.e. on WaterGageDaily
#' @param ds_in An optional RomDataSource to allow for querying of additional
#'   information. May be provided by OWS config files.
#' @return R6 Object of class WaterGageBase
#' @export WaterGageBase
#' @examples \dontrun{
#'  #Spatial Info:
#' SA_Ashland <- WaterGageBase$new(config = list(gage_id = "01672500", ds_in = ds,
#'                                 end_date = "2021-11-11",
#'                                 approval_status = "Approved"))
#'  SA_Ashland$load_sf_da()
#'  SA_Ashland$drainage_area
#'  #If data source provided:
#'  SA_Ashland$load_wshd_feat()
#' }
WaterGageBase <- R6::R6Class(
  "WaterGageBase",
  public = list(
    #' @field ds RomDataSource often provided in DEQ config files and features a
    #'   local data storage or a connection to a database/RESTful service
    ds = NA,
    #' @field data_source May be either USGS or the path to gage data. If a file
    #'   path is used, both the flow_col and date_col fields should be populated
    data_source = "USGS",
    #' @field gage_id The USGS ID of the gage
    gage_id = NA,
    #' @field date_col The column containing "date" R class date times for the
    #'   date_source
    date_col = "time",
    #'@field flow_col The column containing the flow values from the data_source
    flow_col = "value",
    #' @field drainage_area The drainage area of the gage, populated on
    #'   initialize if possible
    drainage_area = NA,
    #' @field gage_data_sf A simple feature of the gage data
    gage_data_sf = NA,
    #'@field gage_feature a VA Hydro feature of the gage if available from ds
    gage_feature = NA,
    #'@field start_date The first date to include in the data
    start_date = "",
    #'@field end_date The last date to include in the data
    end_date = "",
    #'@field parameter_code Which USGS paramter to analyze? Defaults to 00060
    #'  for discharge
    parameter_code = "00060",
    #'@field approval_status Either 'approved', 'provisional', or 'all'.
    #'  Determines what kind of data should be retrieved from USGS: all data or
    #'  only those marked approved or provisional?
    approval_status = "all",
    #' @description
    #' Initialize a WaterGageBase() instance populating all fields passed to
    #' object by the named list config. Only valid public fields are populated.
    #' The object will iterate over config to set fields.
    #' @param config A named list of public fields on waterGageBase with values
    #'   to set
    #' @return object instance, with fields populated by values in config
    initialize = function(config = list()){
      self$handle_config(config)
    },
    #' @description
    #' Handles the config passed in initialize to set all fields on the object
    #' by calling \code{self$handle_config_item()}
    #' @return NULL
    handle_config = function(config = list()){
      mapply(config, names(config), FUN = self$handle_config_item)
    },
    #' @description
    #' For a given item in the user config, check to see if the name of the
    #' config list item is a field on this object. If so, set it to the value of
    #' the config_item
    #' @return NULL
    handle_config_item = function(config_item, config_item_name){
      #Try to extract only fields from self by eliminating functions and
      #environments
      all_self_fields_methods <- names(self)
      all_self_fields <- unlist(lapply(all_self_fields_methods, function(x) (!is.function(self[[x]]) && !is.environment(self[[x]]))))
      all_self_fields <- all_self_fields_methods[all_self_fields]
      
      #If the name of the config item is a field on this object, set it
      if(config_item_name %in% names(self)){
        self[[config_item_name]] <- config_item
      }
      return(self[[config_item_name]])
    },
    #' @description
    #' If a valid USGS gage_id is set on object, use
    #' \code{dataRetrieval::read_waterdata_monitoring_location()} or
    #' \code{dataRetrieval::readNWISsite()} to get an sf data frame with
    #' additional details about the gage and use it to set the
    #' \code{gage_data_sf} and \code{drainage_area} fields
    #' @return Nothing, but sets \code{gage_data_sf} and \code{drainage_area}
    #'   fields on object
    load_sf_da = function(){
      #If user is using USGS or has provided a gage id, try to use dataRetrieval
      #to get additional info about gage/site
      if(!is.na(self$gage_id)){
        #Based on dataRetrieval pacakge version, use either new or deprecated
        #NWIS functions
        if(packageVersion("dataRetrieval") >= "2.7.23") {
          #New functions return an sf already. so just parse out drainage area
          #for separate field
          self$gage_data_sf <- dataRetrieval::read_waterdata_monitoring_location(paste0("USGS-",self$gage_id))
          self$drainage_area <- self$gage_data_sf$drainage_area
        }else{
          #NWIS functions return a data frame, so convert to SF using
          #appropriate coordinate fields
          site_info <- dataRetrieval::readNWISsite(self$gage_id)
          self$gage_data_sf <-  sf::st_as_sf(site_info, crs = 4326, 
                                             coords = c("dec_long_va", "dec_lat_va"))
          self$drainage_area <- site_info$drain_area_va
        }
      }else{
        message("Please set the gage_id field with a valid USGS site number to use automatic geometries.")
      }
    },
    #' @description
    #' If a valid RomDataSource has been provided and the gage_id field has been
    #' set, query the ds field for the corresponding USGS watershed feature
    #' @return The RomFeature if found or NA. Also sets \code{gage_feature} field on the object
    load_wshd_feat = function(){
      if(inherits(self$ds, "RomDataSource") && !is.na(self$gage_id)){
        found_feature <- RomFeature$new(
          ds, config = list(hydrocode = paste0("usgs_ws_",self$gage_id),
                            bundle = "watershed",
                            ftype = "usgs_full_drainage"),
          TRUE)
        if(is.na(found_feature$hydroid)){
          found_feature <- NA
          message("No feature for this watershed found")
        }
        self$gage_feature <- found_feature
        return(found_feature)
      }
    }
    
  )
)
