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
    #' @field statistic_id The kind of stream data to pull
    statistic_id = "00003",
    #' @field usgs_properties The fields to pull from USGS
    usgs_properties = c("monitoring_location_id", "parameter_code",
                        "time", "value", "unit_of_measure"),
    #'@low_flows Critical low flows calculated by hydrotools::xQy() via
    #'  \comp(self$comp_xQy())
    low_flows = NA
    
    initialize = function(data_source, gage_id, flow_col, date_col){
      #If user is using USGS, use dataRetrieval
      if(data_source == "USGS"){
        self$gage_id <- paste0("USGS-",gage_id)
        self$get_gage_data()
        
        
        
        #Need to create SF of the gage, get DA, and get the watershed feature (or optional as a load_sf())?
      }
      
    },
    #new dataRetrieval
    get_gage_data = function(){
      self$gage_data <- dataRetrieval::read_waterdata_daily(
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
    load_da = function(){
      self$drainage_area <- NA
    },
    load_sf = function(){
      #Need to create SF of the gage, get DA, and get the watershed feature (or optional as a load_sf())?
    },
    comp_xQy = function(AYS = "04-01", AYE = "03-31", startYear = NULL, endYear = NULL,
                   x = 7, y = 10){
      self$low_flows <- xQy(
        gageDataIn = self$gage_data, flowColumn = self$flowColumn, 
        dateColumn = self$dateColumn,
        AYS = AYS, AYE = AYE, startYear = startYear,
        endYear = endYear, x = 7, y = 10),
      
    },
    baseflow_analysis = function(){
      #HARP 2026 workflow here, maybe set and AGWRC to use in a forecast method
    },
    baseflow_forecast = function(){
      
    },
    