WaterGageBase <- R6Class(
  "waterGageBase",
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
    #' @field gage_data The gage data from data_source
    gage_data = NA,
    #' @field gage_data_sf A simple feature of the gage data
    gage_data_sf = NA,
    #'@field gage_feature a VA Hydro feature of the gage if available from ds
    gage_feature = NA,
    #' @field statistic_id The kind of stream data to pull
    statistic_id = "00003",
    #' @field usgs_properties The fields to pull from USGS
    usgs_properties = c("monitoring_location_id", "parameter_code",
                        "time", "value", "unit_of_measure"),
    #'@low_flows Critical low flows calculated by hydrotools::xQy() via
    #'  \comp(self$comp_xQy())
    low_flows = NA,
    initialize = function(data_source, gage_id, flow_col, date_col,
                          ds_in = NA, get_sf = FALSE){
      #If user is using USGS, use dataRetrieval
      if(data_source == "USGS"){
        if(packageVersion("dataRetrieval") >= "2.7.23") {
          self$gage_id <- paste0("USGS-",gage_id)
          self$get_gage_data()
        }else{
          #Deprecated with new dataRetrieval
          self$get_gage_data_old()
        }else if (is.character(data_source == "csv")){
          self$gage_data <- read.csv(data_source)
        }else{
          #If user simply provides the data
          self$gage_data <- data_source
        }
        if(is.na(ds_in)){
          self$ds <- ds_in
        }
      }
      
    },
    #new dataRetrieval
    get_gage_data = function(){
      gage_data_sf <- dataRetrieval::read_waterdata_daily(
        monitoring_location_id = self$gage_id, parameter_code = "00060",
        statistic_id = self$statistic_id, skipGeometry = FALSE,
        properties = c("monitoring_location_id", "parameter_code",
                       "time", "value", "unit_of_measure")
      )
      self$flow_col <- "value"
      self$date_col <- "time"
      
      
    },
    #Deprecated
    get_gage_data_old = function(){
      gage_data <- dataRetrieval::readNWISdv(
        siteNumbers = self$gage_id, parameter_code = "00060",
        statCd = self$statistic_id
      )
      self$gage_date <- dataRetrieval::renameNWISColumns(gage_data)
      self$flow_col <- "Flow"
      self$date_col <- "Date"
    },
    load_sf_da = function(){
      #If user is using USGS, use dataRetrieval
      if(data_source == "USGS"){
        if(packageVersion("dataRetrieval") >= "2.7.23") {
          #Need to create SF of the gage, get DA, and get the watershed feature (or optional as a load_sf())?
          self$gage_data_sf <- dataRetrieval::read_waterdata_monitoring_location(self$gage_id)
          self$drainage_area <- self$gage_data_sf$drainage_area
        }else{
          site_info <- dataRetrieval::readNWISsite(self$gage_id)
          self$gage_data_sf <-  sf::st_as_sf(site_info, crs = 4326, 
                                             coords = c("dec_long_va", "dec_lat_va"))
          self$drainage_area <- site_info$drain_area_va
        }
        
        if(!is.na(self$ds)){
          self$gage_feature <- RomFeature$new(
            ds, config = list(hydrocode = paste0("usgs_ws_",self$gage_id),
                              bundle = "watershed",
                              ftype = "usgs_full_drainage"),
            TRUE)
          if(is.na(self$gage_feature$hydroid)){
            self$gage_feature <- NA
            message("No feature for this watershed found")
          }
        }
        
      }else{
        message("Please provide a USGS gage ID to use automatic geometries.")
      }
    },
    comp_xQy = function(AYS = "04-01", AYE = "03-31",
                        startYear = NULL, endYear = NULL,
                        x = 7, y = 10){
      self$low_flows <- xQy(
        gageDataIn = self$gage_data, flowColumn = self$flowColumn, 
        dateColumn = self$dateColumn,
        AYS = AYS, AYE = AYE, startYear = startYear,
        endYear = endYear, x = 7, y = 10)
      
    },
    baseflow_analysis = function(){
      #HARP 2026 workflow here, maybe set and AGWRC to use in a forecast method
    },
    baseflow_forecast = function(){
      
    }
    
  )# End Public
) # End R6
    