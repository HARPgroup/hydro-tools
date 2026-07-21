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
#' @importFrom dataRetrieval read_waterdata_monitoring_location readNWISsite
#' @param config A named list containing the names of fields to set on the
#'   object. This may contain any of the named public fields on WaterGageBase or
#'   on the inherited object i.e. on WaterGageDaily
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
    #'@field station_name The name of the monitoring station at which the flow
    #'  data was collected.
    station_name = NA, 
    #'@field parameter_code Which USGS paramter to analyze? Defaults to 00060
    #'  for discharge
    parameter_code = "00060",
    #'@field approval_status Either 'approved', 'provisional', or 'all'.
    #'  Determines what kind of data should be retrieved from USGS: all data or
    #'  only those marked approved or provisional?
    approval_status = "all",
    #'@field agwrc_lm_m The slope of the linear model of active groundwater
    #'  recession coefficient AGWRC ~ log(Q). Stored in database.
    agwrc_lm_m = NA,
    #'@field agwrc_lm_b The intercept of the linear model of active groundwater
    #'  recession coefficient AGWRC ~ log(Q). Stored in database.
    agwrc_lm_b = NA,
    #'@field agwrc_lm_limit list. Where stored, the lower and upper limits of
    #'  analysis for AGWRC are stored in a list with variables agwrc_reg_qlow
    #'  (lowest valid flow for regression), agwrc_reg_clow (corresponding lowest
    #'  AGWRC for regresison), agwrc_reg_qhigh, and agwrc_reg_chigh
    agwrc_lm_limit = list(),
    #' @description
    #' Initialize a WaterGageBase() instance populating all fields passed to
    #' object by the named list config. Only valid public fields are populated.
    #' The object will iterate over config to set fields.
    #' @param config A named list of public fields with values to set
    #' @return object instance, with fields populated by values in config
    initialize = function(config = list()){
      self$handle_config(config)
    },
    #' @description
    #' Handles the config passed in initialize to set all fields on the object
    #' by calling \code{self$handle_config_item()}
    #' @param config A named list of public fields with values to set
    #' @return NULL
    handle_config = function(config = list()){
      mapply(config, names(config), FUN = self$handle_config_item)
    },
    #' @description
    #' For a given item in the user config, check to see if the name of the
    #' config list item is a field on this object. If so, set it to the value of
    #' the config_item
    #' @param config_item A value to set on the field in config_item_name, if it
    #'   exists on this object
    #' @param config_item_name A field to set with the value config_item, if the
    #'   field exists on this object
    #' @return The target field, may be NULL if it does not exist
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
        if(utils::packageVersion("dataRetrieval") >= "2.7.23") {
          #New functions return an sf already. so just parse out drainage area
          #for separate field
          self$gage_data_sf <- dataRetrieval::read_waterdata_monitoring_location(paste0("USGS-",self$gage_id))
          self$drainage_area <- self$gage_data_sf$drainage_area
          self$station_name <- self$gage_data_sf$monitoring_location_name
        }else{
          #NWIS functions return a data frame, so convert to SF using
          #appropriate coordinate fields
          site_info <- dataRetrieval::readNWISsite(self$gage_id)
          self$gage_data_sf <-  sf::st_as_sf(site_info, crs = 4326, 
                                             coords = c("dec_long_va", "dec_lat_va"))
          self$drainage_area <- site_info$drain_area_va
          self$station_name <- site_info$station_nm
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
    },
    #'@description Read all scenario properties of a model on this object
    #'  RomEntity
    #'@param target_entity The RomEntity (RomFeature or RomProperty) that
    #'  contains the model of interest
    #'@param model_prop_code The propcode of the model on the feature
    #'@param scenario_prop_code The scenario propcode on the model property
    #'@return A data frame of all properties on the model scenario
    get_model_or_scenario_props = function(target_entity = self$gage_feature,
                                        model_prop_code,
                                        scenario_prop_code = NULL){
      
      #Read data in from data base
      if(!inherits(target_entity,"RomFeature") && !inherits(target_entity,"RomProperty")){
        message("target_entity must be a RomFeature or RomProperty")
        return(NULL)
      }
      model_prop <- target_entity$get_prop(propcode = model_prop_code)
      if(is.na(model_prop$pid)){
        message("No model with propcode ", model_prop_code," found") 
        return(NULL)
      }
      if(is.null(scenario_prop_code)){
        all_props <- model_prop$propvalues()
      }else{
        scen_prop <- model_prop$get_prop(propcode = scenario_prop_code)
        if(is.na(model_prop$pid)){
          message("No model scenario with propcode ", scenario_prop_code," found") 
          return(NULL)
        }
        all_props <- scen_prop$propvalues()
      }
      
      return(all_props)
    },
    #'@description Reads in JSON or csv file from a webserver
    #'@param omsite The base URL of the webserver, often
    #'  https://deq1.bse.vt.edu:443
    #'@param url_resource The remaining path to the file e.g. "/path/to/my.csv"
    #'@return A data frame of the resource read using \code{read.csv()} or
    #'  \code{jsonlite::read_json()}
    read_om_file = function(omsite = omsite, url_resource){
      if(grepl("\\.json$",url_resource)){
        out <- jsonlite::read_json(paste0(omsite,url_resource))
      }else{
        out <- tryCatch({
          read.csv(paste0(omsite,url_resource))
        },
        warning = function(w){
          print(w)
          return(NULL)
        },
        error = function(e){
          print(e)
          return(NULL)
        })
      }
      return(out)
    },
    #' @description Get relevant baseflow workflow data frames from web server
    #' @details The 2025 - 2027 HARP projects worked on compiling a bash-R
    #'   workflow that could analyze a gage record and identify periods of
    #'   baseflow. The recession of these baseflow events were characterized by
    #'   regressions to compile a relationship of flow vs AGWRC (active ground
    #'   water recession coefficient). The results of these analyses can be used
    #'   to create baseflow forecasts. This functions returns all data generated
    #'   from the AGWS workflow by reading in the relevant data from the VT
    #'   Apache web server
    #'@param omsite The URL of the base VT Apache webserver, often defined in
    #'  /var/www/R/config.R
    #'@return A list that returns different data frames from files from the
    #'  webserver defined by omsite
    baseflow_workflow_data = function(omsite){
      events_df <- self$read_om_file(omsite,paste0("/usgs/agws/baseflow_stats_",self$gage_id,".csv"))
      trimmed_events_df <- self$read_om_file(omsite,paste0("/usgs/agws/baseflow_trimmed_stats_",self$gage_id,".csv"))
      event_summary_df <- self$read_om_file(omsite,paste0("/usgs/agws/baseflow_summary_df_",self$gage_id,".csv"))
      lm_df <- self$read_om_file(omsite,paste0("/usgs/agws/baseflow_regression_df_",self$gage_id,".csv"))
      
      return(
        list(
          events_df = events_df, 
          trimmed_events_df = trimmed_events_df, 
          event_summary_df = event_summary_df, 
          lm_df = lm_df
        )
      )
    },
    #'@description Scatterplot of AGWRC vs Median Flow
    #'@details Loads data from the 2025-2027 HARP project to draw a scatter plot
    #'  of AGWRC vs Flow and plots the workflow regression
    #'@param CI logical. Should a confidence interval be calculated to add to
    #'  plot?
    #'@param omsite The URL of the base VT Apache webserver, often defined in
    #'  /var/www/R/config.R
    #'@return A ggplot scatterplot of median flow vs AGWRC
    plot_baseflow_agwrc = function(CI = FALSE, omsite){
      event_summary_df <- self$read_om_file(omsite, 
                                            paste0("/usgs/agws/baseflow_summary_df_",self$gage_id,".csv"))
      
      if((is.na(self$agwrc_lm_m) | is.na(self$agwrc_lm_b))){
        #Load feature if not yet loaded
        if(!inherits(self$gage_feature, "RomFeature")){
          self$load_wshd_feat()
        }
        #Get all AGWRC simple_lm properties for this gage and store the
        #regression coefficients
        lm_props <- self$get_model_or_scenario_props(
          target_entity = self$gage_feature,
          model_prop_code = "AGWRC-1.0",
          scenario_prop_code = "simple_lm")
        self$agwrc_lm_m <- lm_props$propvalue[lm_props$propname == "regression_m"]
        self$agwrc_lm_b <- lm_props$propvalue[lm_props$propname == "regression_b"]
        self$agwrc_lm_limit <- list(
          agwrc_reg_qlow = lm_props$propvalue[lm_props$propname == "agwrc_reg_qlow"],
          agwrc_reg_clow = lm_props$propvalue[lm_props$propname == "agwrc_reg_clow"],
          agwrc_reg_qhigh = lm_props$propvalue[lm_props$propname == "agwrc_reg_qhigh"],
          agwrc_reg_chigh = lm_props$propvalue[lm_props$propname == "agwrc_reg_chigh"]
        )
      }
      #Data for regression line for plot
      flow_seq <- seq(min(event_summary_df$median_flow, na.rm = TRUE),
                      max(event_summary_df$median_flow, na.rm = TRUE),
                      length.out = 1000)
      
      pred_df_workflow <- data.frame(
        median_flow = flow_seq,
        event_AGWRC = (self$agwrc_lm_m * log(flow_seq) + self$agwrc_lm_b),
        datagrp = "all"
      )
      #Plot the scatterplot of AGWRC vs Flow and include a line for the
      #regression calculated from the AGWRC workflow
      p <- ggplot2::ggplot() + 
        ggplot2::geom_point(data = event_summary_df, color = "black",
                           ggplot2::aes(x = .data$median_flow, y = .data$event_AGWRC,
                                        text = paste0(
                                          "GroupID: ", .data$GroupID,"<br>",
                                          "Median flow: ", round(.data$median_flow,1)," cfs<br>",
                                          "Event AGWRC: ", round(.data$event_AGWRC,3))
                           )
        ) + 
        ggplot2::geom_line(data = pred_df_workflow, 
                           ggplot2::aes(x = .data$median_flow, y = .data$event_AGWRC,
                                        group = .data$datagrp,
                                        color = "WSPA Regression",
                                        text = paste0(
                                          "Median flow: ", round(.data$median_flow,2)," cfs<br>",
                                          "Event AGWRC: ", round(.data$event_AGWRC,5)
                                        ))
        ) + 
        ggplot2::labs(x = "Median Event Flow", y = "Event AGWRC",
                      title = paste("AGWRC vs Flow (event-level)","\nUSGS",self$gage_id)) + 
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) 
      #Should confidence interval be included on plot?
      if(CI){
        reg_lm <- agws::fit_agwrc_regression(event_summary_df)
        if(is.logical(all.equal(as.numeric(coef(reg_lm)), c(self$agwrc_lm_b, self$agwrc_lm_m))) && 
          all.equal(as.numeric(coef(reg_lm)), c(self$agwrc_lm_b, self$agwrc_lm_m))){
          cint <- as.data.frame(predict(reg_lm, interval = "confidence"))
          cint$median_flow <- event_summary_df$median_flow
          cint$hoverText <- paste0(
            "Median flow: ", round(cint$median_flow,2)," cfs<br>",
            "Lower AGWRC: ", round(cint$lwr,5),"<br>",
            "Upper AGWRC: ", round(cint$upr,5)
          )
          cint$grp <- "all"
          p <- p + 
            ggplot2::geom_ribbon(data = cint,
                                        ggplot2::aes(x = .data$median_flow,
                                                     ymin = .data$lwr,
                                                     ymax = .data$upr),
                                 alpha = 0.1, fill = "lightsteelblue3",
                                 color = 'steelblue4') + 
            ggplot2::geom_line(data = cint,col = "steelblue4",
                               ggplot2::aes(x = .data$median_flow, y = .data$lwr,
                                            group = .data$grp, 
                                            text = paste0(
                                              "Median flow: ", round(.data$median_flow,2)," cfs<br>",
                                              "Lower Bound AGWRC: ", round(.data$lwr,5)
                                            ))
            ) + 
            ggplot2::geom_line(data = cint,col = "steelblue4",
                               ggplot2::aes(x = .data$median_flow, y = .data$upr,
                                            group = .data$grp,
                                            text = paste0(
                                              "Median flow: ", round(.data$median_flow,2)," cfs<br>",
                                              "Upper Bound AGWRC: ", round(.data$lwr,5)
                                            ))
            )
        }else{
          message("Linear model calculated is different from that in data
          source. No confidence interval computed.")
        }
      }
      
      return(p)
      
    },
    
    #' @description Query the db for AGWRC regression coefficients
    #' @details Get the statistical model coefficients to create a relationship
    #'   of active groundwater recession coefficients (AGWRC) vs log(Flow).
    #'   These are stored as scenario properties on the feature model. User can
    #'   optionally return a function that will calculate AGWRC from Q using the
    #'   regression coefficients, though this funciton inherits data from this
    #'   object that are not explicitly passed in.
    #'@param force_refresh logical, defaults to TRUE. Should the data base be
    #'  queried and the agwrc_lm_m and agwrc_lm_b fields on this object be
    #'  overwritten?
    #'@param return_fun logical, defaults to FALSE. Should a function be
    #'  returned to calculate AGWRC for a given flow?
    agwrc_fun = function(return_fun = FALSE, force_refresh = TRUE){
      #If user wishes to reload the agwrc lm coefficients, pull from DB
      if(force_refresh || (is.na(self$agwrc_lm_m) | is.na(self$agwrc_lm_b))){
        #Load feature if not yet loaded
        if(!inherits(self$gage_feature, "RomFeature")){
          self$load_wshd_feat()
        }
        #If no feature found, do not proceed
        if(!inherits(self$gage_feature, "RomFeature")){
          message("No watershed feature found.")
          return(NULL)
        }else{
          #Get all AGWRC simple_lm properties for this gage and store the
          #regression coefficients
          lm_props <- self$get_model_or_scenario_props(
            target_entity = self$gage_feature,
            model_prop_code = "AGWRC-1.0")
          self$agwrc_lm_m <- lm_props$propvalue[lm_props$propname == "regression_m"]
          self$agwrc_lm_b <- lm_props$propvalue[lm_props$propname == "regression_b"]
          self$agwrc_lm_limit <- list(
            agwrc_reg_qlow = lm_props$propvalue[lm_props$propname == "agwrc_reg_qlow"],
            agwrc_reg_clow = lm_props$propvalue[lm_props$propname == "agwrc_reg_clow"],
            agwrc_reg_qhigh = lm_props$propvalue[lm_props$propname == "agwrc_reg_qhigh"],
            agwrc_reg_chigh = lm_props$propvalue[lm_props$propname == "agwrc_reg_chigh"]
          )
          if(!return_fun){
            return(lm_props)
          }
        }
      }
      #Return a function to calc AGWRC ~ log(Q)
      if(return_fun){
        #Is this problematic? Below, in calc_agwrc() lm_props is not defined; it's
        #an implicit global in the parent.frame of agwrc_fun. We can either accept
        #this and hope R keeps the parent tidy despite possible values in the
        #global environment i.e. http://adv-r.had.co.nz/Environments.html 
        #OR we can write the WHOLE function as a string and use
        #eval(parse(function_text)) to "sub" in the values for these variables via
        #conversion to text
        #https://stackoverflow.com/questions/15260245/r-convert-text-field-to-function
        message("This function has a implicit/undeclared variable lm_props that
      is defined in the parent environment (e.g. self$agwrc_fun()). Use at
      your own risk.")
        calc_agwrc <- function(Q){
          if(!is.null(self$agwrc_lm_limit$agwrc_reg_qlow) &&
             Q < self$agwrc_lm_limit$agwrc_reg_qlow){
            out <- self$agwrc_lm_limit$agwrc_reg_clow
          }else if(!is.null(self$agwrc_lm_limit$agwrc_reg_qhigh) &&
             Q > self$agwrc_lm_limit$agwrc_reg_qhigh){
            out <- self$agwrc_lm_limit$agwrc_reg_chigh
          }else{
            out <- self$agwrc_lm_b + (log(Q) * self$agwrc_lm_m)
          }
          
          return(out)
        }
        return(calc_agwrc)
      }
    }
  )
)
