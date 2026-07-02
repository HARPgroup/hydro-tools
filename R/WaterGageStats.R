# WaterGageStats ####
#' Water Gage Statistics Object
#' @title WaterGageStats
#' @description Utility class for querying stream gage statistics or developing
#'   monthly/annual statistics
#' @details This R6 object has standard methods for managing the data and meta
#'   data related to stream gage statistics. On initialize, this object can
#'   query USGS for flow statistics and will store on object. Other methods may
#'   be available to summarize existing daily stream data
#' @importFrom R6 R6Class  
#' @param data_source For now, this must be "USGS" but may be a data frame in
#'   later versions to allow statistics to be developed from a data frame
#' @param gage_id The gage ID. This should be a USGS site number if \code{data_source}
#'   is "USGS" such as 01671020
#' @param start_date The end of the period to consider for statistical
#'   summary. This should be in the format mm-dd if computation_type =
#'   'doy'; otherwise, should be in format yyyy-mm-dd
#' @param end_date The end of the period to consider for statistical
#'   summary. This should be in the format mm-dd if computation_type =
#'   'doy'; otherwise, should be in format yyyy-mm-dd
#' @param approval_status 'all', 'approved', or 'provisional'. What kind of
#'   data should be used from USGS? Defaults to that set in the field on
#'   this object, which itself defaults to 'all'. Please note USGS usually can
#'   only provide statistics for approved values.
#' @param statistic_type The kind of statistic analysis to run. Can by 'doy'
#'   to retrieve day-of-year statistics or can be 'daterange' to retrieve
#'   monthly, yearly, and water year statistics
#' @param computation_type The statistics to use when summarize data
#' @param ds_in An optional RomDataSource to allow for querying of additional
#'   information. May be provided by OWS config files.
#' @return R6 Object of class WaterGageBase
#' @export WaterGageStats
#' @examples \dontrun{
#'  #Spatial Info:
#' SA_Ashland <- WaterGageStats$new(gage_id = "01672500")
#' }
WaterGageStats <- R6::R6Class(
  "WaterGageStats",
  inherit = WaterGageBase,
  public = list(
    #' @field gage_data The gage data from data_source
    gage_data = NA,
    #' @field statistic_type The kind of statistic analysis to run. Can by 'doy'
    #'   to retrieve day-of-year statistics or can be 'daterange' to retrieve
    #'   monthly, yearly, and water year statistics
    statistic_type = "doy",
    #' @field computation_type The statistics to use when summarize data
    computation_type = c("arithmetic_mean", "maximum", "median", "minimum", "percentile"),
    #' @field start_date The start of the period to consider for statistical
    #'   summary. This should be in the format mm-dd if computation_type =
    #'   'doy'; otherwise, should be in format yyyy-mm-dd
    start_date = "01-01",
    #' @field end_date The end of the period to consider for statistical
    #'   summary. This should be in the format mm-dd if computation_type =
    #'   'doy'; otherwise, should be in format yyyy-mm-dd
    end_date = "12-31",
    #' @description
    #' Initialize a WaterGageStats() instance, returning an R6 object that is
    #' now populated with gage statistics. This object offers various standard
    #' methods to retrieve and analyze USGS gage statistics
    #' @param data_source For now, this must be "USGS" but may be a data frame in
    #'   later versions to allow statistics to be developed from a data frame
    #' @param gage_id The gage ID. This should be a USGS site number if \code{data_source}
    #'   is "USGS" such as 01671020
    #' @param start_date The end of the period to consider for statistical
    #'   summary. This should be in the format mm-dd if computation_type =
    #'   'doy'; otherwise, should be in format yyyy-mm-dd
    #' @param end_date The end of the period to consider for statistical
    #'   summary. This should be in the format mm-dd if computation_type =
    #'   'doy'; otherwise, should be in format yyyy-mm-dd
    #' @param approval_status 'all', 'approved', or 'provisional'. What kind of
    #'   data should be used from USGS? Defaults to that set in the field on
    #'   this object, which itself defaults to 'all'. Please note USGS usually can
    #'   only provide statistics for approved values.
    #' @param ds_in An optional RomDataSource to allow for querying of additional
    #'   information. May be provided by OWS config files.
    #' @return object instance
    initialize = function(data_source = "USGS", gage_id = NA,
                          start_date = self$start_date, end_date = self$end_date,
                          approval_status = self$approval_status,
                          computation_type = self$computation_type,
                          statistic_type = "doy",
                          ds_in = NA){
      #If the user has provided a RomDataSource ds, set the appropriate field
      if(inherits(ds_in, "RomDataSource")){
        self$ds <- ds_in
      }
      #Set basic fields by calling the parent initialize
      super$initialize(
        list(
          data_source = data_source,
          gage_id = gage_id,
          start_date = start_date,
          end_date = end_date, 
          approval_status = approval_status,
          statistics_type = statistic_type,
          computation_type = computation_type
        )
      )
      
      #If user is using USGS, use dataRetrieval
      if(inherits(data_source, "character") && data_source == "USGS"){
        #Use either the new dataRetrieval functions or those slated for
        #deprecation based on version number
        if(packageVersion("dataRetrieval") >= "2.7.23") {
          #If approval_status is all, set to NA for query
          if(tolower(self$approval_status) == "all"){
            approval_status <- NA_character_
          }else{
            approval_status <- self$approval_status
          }
          
          if(self$statistic_type == "doy"){
            self$gage_data <- dataRetrieval::read_waterdata_stats_por(
              monitoring_location_id = paste0("USGS-",self$gage_id),
              parameter_code         = self$parameter_code,
              computation_type       = self$computation_type,
              start_date             = self$start_date,
              end_date               = self$end_date,
              approval_status        = approval_status
            ) 
            self$flow_col <- "value"
            self$date_col <- "time_of_year"
          }else if(self$statistic_type == "daterange"){
            self$gage_data <- dataRetrieval::read_waterdata_stats_daterange(
              monitoring_location_id = paste0("USGS-",self$gage_id),
              parameter_code         = self$parameter_code,
              computation_type       = self$computation_type,
              start_date             = self$start_date,
              end_date               = self$end_date,
              approval_status        = approval_status
            ) 
            self$flow_col <- "value"
            self$date_col <- "end_date"
          }
        }else{
          if(self$statistic_type == "doy"){
            #Get doy statistics
            #statType = c("Min", "Max", paste0("p",c("05",10,25,50,75,90,95)))
            stats_data <- dataRetrieval::readNWISstat(
              siteNumbers = self$gage_id,
              parameterCd = self$parameter_code,
              statReportType = "daily",
              statType = self$computation_type,
              startDate = self$start_date, endDate = self$end_date
            )
            #Reorganize data frame to match formatting of new dataRetrieval:
            stat_data_long <- stats_data |> 
              #Create a time_of_year column that has format mm-dd
              dplyr::mutate("time_of_year" = paste0(sprintf("%02d",month_nu),"-",sprintf("%02d",day_nu))) |> 
              dplyr::select(-day_nu, -month_nu) |> 
              #Pivot longer to have a column of the statistic (computation) and
              #value
              tidyr::pivot_longer(c(min_va, p05_va,p10_va,p25_va,p50_va,p75_va,p90_va,p95_va,max_va),
                                  names_to = "computation") |> 
              #Match new dataRetrieval by creating columns that have a more
              #generalized statistic name and a column that has the percentile
              dplyr::mutate(percentile = gsub("p(.*)_va","\\1",computation)) |> 
              dplyr::mutate(
                percentile = dplyr::case_when(
                  computation == "max_va" ~ "100",
                  computation == "min_va" ~ "0",
                  TRUE ~ percentile
                )
              ) |> 
              dplyr::mutate(
                computation = dplyr::case_when(
                  computation == "min_va" ~ "minimum",
                  computation == "max_va" ~ "maximum",
                  TRUE ~ "percentile"
                )
              ) 
            
            self$gage_data <- stat_data_long
            self$flow_col <- "value"
            self$date_col <- "time_of_year"
          }else{
            #To populate later: old dataRetrieval monthly or yearly statistics
            message("Previous versions of dataRetrieval could not get monthly 
                    and annual percentiles. Please update dataRetrieval and try aagin.")
          }
        }
      }else{
        #To populate later: What if we provide this a data frame? Can we run
        #summaries on it?
        message("For now, WaterGageStats only supports data_source = 'USGS'")
      }
    }
  )# End Public
)# End R6
      
      
      