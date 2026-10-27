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
#' #Additional tests in tests/WaterGageStats.R
#'  #Spatial Info:
#' SA_stats <- WaterGageStats$new(gage_id = "01672500")
#' 
#' #Now initialize with a daily data frame using doy
#' SA_stats_monthly <- WaterGageStats$new(
#'   data_source = SA_stats$gage_data,
#'   date_col = SA_stats$date_col, flow_col = SA_stats$flow_col,
#'   statistic_type = "month",
#'   computation_type = c("percentile","minimum","maximum")
#' )
#' 
#' #Get Stats from a  WaterGageDaily
#' gage_obj$calculate_value_percentile(28, "2001-11-28","month","continuous")
#' gage_obj$calculate_value_percentile(90,"2000-12-01","month","interpolate_USGS")
#' }
WaterGageStats <- R6::R6Class(
  "WaterGageStats",
  inherit = WaterGageBase,
  public = list(
    #' @field gage_data The gage data from data_source
    gage_data = NA,
    #' @field statistic_type The kind of statistic analysis to run. For USGS
    #'   data source, Can by 'doy' to retrieve day-of-year statistics or can be
    #'   'daterange' to retrieve monthly, yearly, and water year statistics.
    #'   Otherwise May be 'doy' to calculate julian day statistics, 'month' to
    #'   calculate monthly statistics, or 'year' to calculate annual statistics
    statistic_type = "doy",
    #' @field computation_type The statistics to use when summarize data. For
    #'   now, only may be c("arithmetic_mean", "maximum", "median", "minimum",
    #'   "percentile") or some slice
    computation_type = c("arithmetic_mean", "maximum", "median", "minimum", "percentile"),
    #' @field start_date The start of the period to consider for statistical
    #'   summary. This should be in the format mm-dd if computation_type =
    #'   'doy'; otherwise, should be in format yyyy-mm-dd
    start_date = "01-01",
    #' @field end_date The end of the period to consider for statistical
    #'   summary. This should be in the format mm-dd if computation_type =
    #'   'doy'; otherwise, should be in format yyyy-mm-dd
    end_date = "12-31",
    #'@field percentile_funcs A list of functions to use to convert values to
    #'  percentile based on date and statistic_type. These are continuous
    #'  functions when data_source is a data.frame or linear approximation of
    #'  known percentiles if data_source is USGS
    percentile_funcs = list(),
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
    #' @param flow_col character. A column in the data frame provided as
    #'   data_source if not using a USGS gage. The data in this field should
    #'   represent the values of interest on which to conduct statistic
    #' @param date_col character. A column in the data frame provided as
    #'   data_source if not using a USGS gage. The data in this field should
    #'   represent Date class data that can be aggregated to the selected
    #'   statistic_type
    #' @param statistic_type Character, default 'doy'. The kind of statistic
    #'   analysis to run. For USGS data source, Can by 'doy' to retrieve
    #'   day-of-year statistics or can be 'daterange' to retrieve monthly,
    #'   yearly, and water year statistics. Otherwise May be 'doy' to calculate
    #'   julian day statistics, 'month' to calculate monthly statistics, or
    #'   'year' to calculate annual statistics
    #' @param  computation_type The statistics to use when summarize data. For
    #'   now, only may be c("arithmetic_mean", "maximum", "median", "minimum",
    #'   "percentile") or some slice
    #' @param approval_status 'all', 'approved', or 'provisional'. What kind of
    #'   data should be used from USGS? Defaults to that set in the field on
    #'   this object, which itself defaults to 'all'. Please note USGS usually can
    #'   only provide statistics for approved values.
    #' @param ds_in An optional RomDataSource to allow for querying of additional
    #'   information. May be provided by OWS config files.
    #' @return object instance
    initialize = function(data_source = "USGS", gage_id = NA,
                          start_date = self$start_date, end_date = self$end_date,
                          flow_col, date_col,
                          approval_status = self$approval_status,
                          computation_type = self$computation_type,
                          statistic_type = "doy",
                          ds_in = NA){
      ## Initialize ####
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
          statistic_type = statistic_type,
          computation_type = computation_type
        )
      )
      
      #If user is using USGS, use dataRetrieval
      if(inherits(data_source, "character") && data_source == "USGS"){
        ## USGS ####
        #Use either the new dataRetrieval functions or those slated for
        #deprecation based on version number
        if(packageVersion("dataRetrieval") >= "2.7.23") {
          ### Newer USGS ####
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
          ### percentiles ####
          self$get_percentile_funcs(agg_type = self$statistic_type)
          
        }else{
          ### Older USGS ####
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
            
            ### percentiles ####
            self$get_percentile_funcs(agg_type = self$statistic_type)
            
          }else{
            #To populate later: old dataRetrieval monthly or yearly statistics
            message("Previous versions of dataRetrieval could not get monthly 
                    and annual percentiles. Please update dataRetrieval and try aagin.")
          }
        }
        
        
      }else if(inherits(self$data_source, "data.frame")){
        ## data.frame ####
        #Set date and value cols:
        self$flow_col <- flow_col
        self$date_col <- date_col
        
        gage_data <- self$data_source
        #Decide how to calculate the aggregate column
        format_date <- list(doy = "%j", month = "%m", year = "%Y")[[self$statistic_type]]
        
        #Set aggregate column using julian day, month, or year
        if(format_date == "%j"){
          #Correct julian day for leap years:
          gage_data[,self$statistic_type] <- as.numeric(
            format(
              as.Date(format(gage_data[,self$date_col],"2004-%m-%d")),
              format_date)
          )
        }else{
          gage_data[,self$statistic_type] <- as.numeric(format(gage_data[,self$date_col], format_date))
        }
        ### Percentiles ####
        self$gage_data <- gage_data
        self$get_percentile_funcs(agg_type = self$statistic_type)
        gage_data <- self$gage_data
        
        ### Summary metrics ####
        if(any(c("arithmetic_mean", "maximum", "median", "minimum") %in% self$computation_type)){
          #Decide how to calculate the aggregate column
          user_funcs <- list(arithmetic_mean = function(x) mean(x, na.rm = TRUE), 
                             maximum = function(x) max(x, na.rm = TRUE),
                             minimum = function(x) min(x, na.rm = TRUE), 
                             median = function(x) median(x, na.rm = TRUE))[self$computation_type[self$computation_type != "percentile"]]
          #Calculate aggregate summaries over the statistic type column
          summary_metrics <- gage_data |> 
            dplyr::group_by(!!dplyr::sym(self$statistic_type)) |> 
            dplyr::summarize(
              dplyr::across(
                .fns = user_funcs,
                .names = paste0(self$statistic_type,"_{.fn}"),
                .cols = !!dplyr::sym(self$flow_col)
              )
            )
          
          #Join metrics on row by row
          gage_data <- gage_data |> 
            dplyr::left_join(
              summary_metrics, by = self$statistic_type
            )
          
          self$gage_data <- gage_data
        }
        
        
      }else{
        message("This datasource has not been implemented. Try to supply
        data_source = 'USGS' and a valid USGS station number as gage_id or a
        data.frame as data_source and a valid flow_col and date_col")
      }
    },
    #'@description Create and store functions to linearly approximate between
    #'  table entries in USGS data. If data_source is a data.frame, this is
    #'  completed in initialize and uses continuous distribution functions
    #'  instead
    #'@param agg_type Character. May be 'doy' or 'month' to aggregate data,
    #'  defaults to the object statistic_type. 'daterange' assumes 'doy'
    #'@return Nothing, but sets percentile_funcs on object
    get_percentile_funcs = function(agg_type = self$statistic_type){
      if(inherits(self$data_source, "character") && self$data_source == "USGS"){
        ## USGS ####
        #Get only the data of interest
        if(any(c("doy", "daterange") %in% agg_type)){
          gage_data <- self$gage_data[self$gage_data$time_of_year_type == "day_of_year",]
        }else if(agg_type == "month"){
          gage_data <- self$gage_data[self$gage_data$time_of_year_type == "month_of_year",]
          self$statistic_type <- "month"
        }else{
          stop("No agg_type of ",agg_type," implemented.")
        }
        
        #Unique aggregate values
        agg_over <- unique(gage_data$time_of_year)
        
        ### Percentiles ####
        #Get a list with a modified approx function for each doy to linearly
        #interpolate between USGS statistics
        percentile_approx <- lapply(
          X = as.list(agg_over),
          gage_data = gage_data,
          FUN = function(X, gage_data){
            thisi <- gage_data[gage_data$time_of_year == X,]
            out <- function(x){
              #Linear approximation
              new_val <- approx(x = thisi$value, y = thisi$percentile, xout = x)
              return(new_val$y)
            }
            return(out)
          }
        )
        
        if(any(c("doy", "daterange") %in% agg_type)){
          names(percentile_approx) <- paste0(
            "percentile_",
            as.numeric(format(as.Date(paste0("2004-",agg_over)),"%j"))
          )
        }else if(agg_type == "month"){
          names(percentile_approx) <- paste0("percentile_",as.numeric(agg_over))
        }

        
        #Store the percentile functions
        self$percentile_funcs <- percentile_approx
      }else if(inherits(self$data_source, "data.frame")){
        ## data.frame ####
        gage_data <- self$gage_data
        self$statistic_type <- agg_type
        
        #Decide how to calculate the aggregate column
        format_date <- list(doy = "%j", month = "%m", year = "%Y")[[self$statistic_type]]
        
        #Set aggregate column using julian day, month, or year
        if(format_date == "%j"){
          #Correct julian day for leap years:
          gage_data[,self$statistic_type] <- as.numeric(
            format(
              as.Date(format(gage_data[,self$date_col],"2004-%m-%d")),
              format_date)
          )
        }else{
          gage_data[,self$statistic_type] <- as.numeric(format(gage_data[,self$date_col], format_date))
        }
        
        #Unique aggregate values
        agg_over <- unique(gage_data[,self$statistic_type])
        
        
        ## percentile_funcs ####
        #Percentile calculations
        if("percentile" %in% self$computation_type){
          #Get a continuous function for each agg_over value (julian day, month,
          #year)
          value_ecdf <- lapply(
            X = as.list(agg_over),
            gage_data = gage_data,
            FUN = function(X, gage_data){
              thisi <- gage_data[gage_data[,self$statistic_type] == X,]
              ecdfi <- ecdf(thisi[,self$flow_col])
              out <- function(x) ecdfi(x) * 100
              return(out)
            }
          )
          #Give logical names for easier indexing
          names(value_ecdf) <- paste0("percentile_", agg_over)
          #Find the appropriate percentil function for each row in gage_data
          percentile_func <- value_ecdf[match(paste0("percentile_", gage_data[,self$statistic_type]), names(value_ecdf))]
          #Find the percentile of each value in flow_col based on the appropriate
          #ecdf function
          gage_data[,paste0(self$statistic_type, "_percentile")] <- mapply(
            f = percentile_func,
            x = gage_data[,self$flow_col],
            FUN = function(f, x){f(x)}
          )
          
          #Store the percentile functions
          self$percentile_funcs <- value_ecdf
        }
        ### Update gage data ####
        #Store results with updated fields
        self$gage_data <- gage_data
      }
    },
    #'@description Calculate the percentile (as percentage) for valueIn on
    #'  dateIn using the percentile_funcs on object
    #'@param valueIn A vecotr of values to be converted to percentile
    #'@param dateIn A vector of date class values to represent when valueIn
    #'  occurred
    #'@return numeric vector of percentiles in percent
    get_percentile_from_value = function(valueIn, dateIn){
      dateIn <- as.Date(dateIn)
      
      #Decide how to calculate the aggregate column
      format_date <- list(doy = "%j", month = "%m", year = "%Y", daterange = "%j")[[self$statistic_type]]
      
      #Find the appropriately converted date
      if(format_date == "%j"){
        #Correct julian day for leap years:
        format_date <- as.numeric(
          format(
            as.Date(format(dateIn,"2004-%m-%d")),
            format_date)
        )
      }else{
        #Format the input dates as values
        format_date <- as.numeric(format(dateIn, format_date))
      }
      #Find the relevant ecdf or approx function
      out_func <- self$percentile_funcs[ paste0("percentile_", format_date) ]
      
      if(is.null(out_func)){
        out <- NULL
        warning("No percentile function found for ",format_date,". Is it
        possible there is no gage data for this period?")
      }else{
        #Output the value
        out <- mapply(
          f = out_func,
          x = valueIn,
          USE.NAMES = FALSE,
          FUN = function(f,x) f(x)
        )
      }
      
      return(out)
    }
  )# End Public
)# End R6
      
      
      