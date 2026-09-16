#'@name auto_forecast_cs
#'@title auto_forecast_cs
#'@description Find potential historic case study forecasts for a given year
#'@details This function finds all potential start dates in March - August to
#'  run a summer no-rain baseflow forecast. It iterates through each potential
#'  start date and evaluates which forecast consecutively predicts the most
#'  troughs accurately or that which predicts the observed minimum flow in the
#'  user period the most accurately based on user input. A list is returned to
#'  the user with the best forecast plotted, a plot of all tested forecasts, a
#'  list of forecasts from each start date, and a list of the analysis data from
#'  each start date.
#' * Users can set \code{start_mmdd} and \code{end_mmdd} to determine the period
#'  between which minimum flows and troughs should be identified. Start dates
#'  are only identified in the period March - August
#' * \code{best_selection} determines how the functions decides which forecast 
#' performs the best. "minimum" results in the selection of the forecast that 
#' most closely predicts the observed minimum flow as long as no trough is 
#' overpredicted by more than \code{max_error}. "consecutive" selects the method
#' that most accurately predicts the most troughs consecutively based on user
#' error input (any trough out of range, even if its the first, iterates the 
#' function to the next potential). If no start date predicts troughs
#' consecutively, that which predicts the most overall (including
#' non-consecutive) is selected. The user is provided a warning if no gage was
#' found
#' * \code{min_error} and \code{max_error} can be set to specify trough/minimum 
#' flow prediction tolerance before program iterates to next start date
#'@param gage_obj A WaterGageDaily R6 object of a USGS gage
#'@param yr Numeric, default 2002. Which year should be analyzed for potential
#'  forecasting lookback
#'@param best_selection Character, default "minimum". How should the best forecast
#'be identified? If "minimum", the longest forecast that estimated the observed low flow
#'within user tolerance and does not overpredict any troughs outside of
#'tolerance is selected.
#'@param AGWRC A of numeric or character given to
#'  \code{WaterGageDaily$baseflow_forecast}. Value is the decay coefficient to
#'  regress the flow on start_date for forecast. May be a single numeric to
#'  allow for a constant forecast or a vector of numeric values to allow for a
#'  variable forecast but must be of length days. Otherwise, may be
#'  "lm_constant" to calculate a constant value from m and b or "lm_variable" to
#'  have a variable value.
#'@param start_mmdd Character, default "03-01". The start of the period in
#'  "mm-dd" to identify potential troughs and minimum observed flow (not used in
#'  start date detection, which is hard coded to March - August)
#'@param end_mmdd Character, default "10-01". The end of the period in "mm-dd"
#'  to identify potential troughs and minimum observed flow (not used in start
#'  date detection, which is hard coded to March - August)
#'@param min_error Numeric, default -20. The low error bound in % on trough/minimum
#'  flow prediction such that an acceptable forecast has an error of at least
#'  \code{min_error} (i.e. underprediction by at most 20%)
#'@param max_error Numeric, default 10. The high error bound in % on trough/minimum
#'  flow prediction such that an acceptable forecast has an error of at most
#'  \code{min_error} (i.e. overprediction by at most 10%)
#'@importFrom rlang .data
#'@return A list with five entries:
#' * beststart The start date of the best performing forecast
#' * all_data A list of data frames named by forecast start date. These 
#' data frames have 365 day forecasts with trough prediction performance for
#' each potential start date
#' * analysis_data A list of data frames named by forecast start date. These 
#' data frames are identical to all_data but have been filtered to only include 
#' trough or minimum flows to demonstrate how the algorithim selected beststart
#' * fc_plot A plot of the best selected forecast beginning at beststart and 
#' terminating either 90 days later or 14 days after the last trough/minimum
#'  flow
#' * fc_all_plot A plot of all tested forecasts with start dates as red points. 
#' The best forecast (which begins at beststart) is displayed in blue, and
#' forecasts before and after beststart are in orange and grey respectively
#'@examples \dontrun{
#'gage_obj <- hydrotools::WaterGageDaily$new(  gage_id = '02029000', ds_in = ds )
#'#Run iterative forecasts using variable AGWRC method and selecting best picked
#'start date via minimum observed flow performance
#'fc_cs <- auto_forecast_cs(
#'   gage_obj = gage_obj,
#'   yr = 1932,
#'   AGWRC = "lm_variable",
#'   best_selection = "minimum",
#'   start_mmdd = "03-01",
#'   end_mmdd = "09-30",
#'   min_error = -20,
#'   max_error = 20
#')
#'#Best forecast starts on:
#'fc_cs$beststart
#'#Show all tested forecasts:
#'fc_cs$fc_all_plot
#'}
#'@export auto_forecast_cs
auto_forecast_cs <- function(
    gage_obj,
    yr = 2002,
    best_selection = "minimum",
    AGWRC = "lm_variable",
    start_mmdd = "03-01",
    end_mmdd = "10-01",
    min_error = -20,
    max_error = 20
){
  #Filter to just target year and recreate R6 object
  gage_yr <- gage_obj$filter_data_by_date(start_date = paste0(yr,"-",start_mmdd),
                                          end_date = paste0(yr,"-",end_mmdd))
  # Initial trough selection - all local minimums
  gage_yr$gage_data$trough <- zoo::rollapply(gage_yr$gage_data[,gage_yr$flow_col],
                                             width = 15, fill = FALSE,
                                             function(x) x[8] <= min(x[-8]))
  #Find appropriate troughs by identifying those that are lower than the previous
  #we can accomplish this the cummin() function, which finds the cumulative
  #minimum of each element up to each index of the vector
  gage_yr$gage_data$trough[gage_yr$gage_data$trough] <-
    gage_yr$gage_data[gage_yr$gage_data$trough,gage_yr$flow_col] <=
    cummin(gage_yr$gage_data[gage_yr$gage_data$trough,gage_yr$flow_col])
  
  #Do not count a value as a trough if it ties with a previous trough. Do not
  #allow duplicate trough flows, in other words
  gage_yr$gage_data$trough[gage_yr$gage_data$trough] <-
    !duplicated(gage_yr$gage_data[gage_yr$gage_data$trough,gage_yr$flow_col])
  
  #Get data only in interested date range to find potential start dates:
  date_range <- as.Date(c(paste0(yr, "-03-01"), paste0(yr, "-08-31")))
  gage_df <- gage_yr$gage_data[gage_yr$gage_data[,gage_yr$date_col] <= max(date_range) &
                                 gage_yr$gage_data[,gage_yr$date_col] >= min(date_range),]
  
  #Initialize tracking variables:
  start_dates <- gage_df[gage_df$trough,gage_yr$date_col]
  #How many consecutive troughs hit within error tolerance?
  trough_hit <- numeric()
  #How many troughs hit within error tolerance total?
  trough_hit_alt <- numeric()
  #Total number of remaning troughs
  trough_total <- numeric()
  #Maximum consecutive troughs hit and associated date
  maxtroughs <- 0
  beststart <- NA
  #Collect forecast with error data
  all_data <- list()
  analysis_data <- list()
  
  #For each start date, run a forecast for 365 days. Join in the trough logical
  #column and keep only trough data. Then, find the longest consecutive sequence
  #of troughs hit (if more than 0 are hit). Record values in iterator trackers
  for (thisdate in start_dates){
    #Ensure date class
    thisdate <- as.Date(thisdate)
    
    #Run a forecast for 365 days
    thisfc <- gage_yr$baseflow_forecast(
      start_date = thisdate,
      forecast_days = 1:365,
      AGWRC = AGWRC,
      use_limits = TRUE
    ) |>
      #Join on the original data for this year, including the trough column
      dplyr::left_join(
        y = gage_yr$gage_data |>
          dplyr::select(!!dplyr::sym(gage_yr$date_col), .data$trough),
        by = c("Date" = gage_yr$date_col)
      ) |>
      #Calculate trough prediction performance
      dplyr::mutate(pcterror = 100 * (.data$Forecast - .data$obs_flow) / .data$obs_flow) |>
      #Create a logical field for whether or not the pcterror is considered within
      #range
      dplyr::mutate(trough_hit = ( (.data$pcterror < .data$max_error) & (.data$pcterror > .data$min_error) ) )
    
    #Store forecast results
    all_data <- c(all_data, list(thisfc))
    
    #Ignore non-trough data for analysis
    thisfc <- dplyr::filter(thisfc, .data$trough)
    
    if(best_selection == "minimum"){
      #Get lowest trough only. **Should** be minimum observed flow in period after
      #adjusting for the 15-day window.
      minfc <- thisfc[which.min(thisfc$obs_flow),]
      trough_hit_i <- as.numeric(minfc$trough_hit)
      trough_hit <- c(trough_hit, trough_hit_i)
      
      #Store data for results
      analysis_data <- c(analysis_data, list(minfc))
      
      #Set the beststart if the trough is hit and it hasn't already been set by an
      #earlier (longer) forecast but do not allow significant overprediction at
      #any trough
      if(
        is.na(beststart)
        && (length(trough_hit_i) > 0)
        && as.logical(trough_hit_i)
        && all(thisfc$pcterror < max_error)
      ){
        beststart <- thisdate
      }
      
    }else if(best_selection == "consecutive"){
      #Store data for results
      analysis_data <- c(analysis_data, list(thisfc))
      #Total number of troughs within error tolerance
      trough_hit_alt_i <- sum(thisfc$trough_hit)
      #Track how many troughs are hit in a row within acceptable error bounds
      seq_values <- rle(thisfc$trough_hit)
      #If the first value in seq_values is FALSE, it indicates the first value in
      #thisfc$trough_hit is FALSE so no consecutive troughs were hit OR thisfc has
      #no data (may occur is start date is on or beyond last trough)
      if( (nrow(thisfc) < 1) || (!seq_values$values[1]) ){
        trough_hit_i <- 0
      }else{
        trough_hit_i <- seq_values$lengths[1]
      }
      #Update iterators
      trough_hit <- c(trough_hit, trough_hit_i)
      trough_hit_alt <- c(trough_hit_alt, trough_hit_alt_i)
      trough_total <- c(trough_total, nrow(thisfc))
      
      #If the number of troughs hit in a row within tolerance is a new record,
      #record it!
      if(trough_hit_i > maxtroughs){
        maxtroughs <- trough_hit_i
        beststart <- thisdate
      }
    }
  }
  
  #Give lists proper names
  names(analysis_data) <- start_dates
  names(all_data) <- start_dates
  
  #If no good case study was found, simply use the date that hits the most troughs
  #(ignoring the need for consecutive hits)
  if(is.na(beststart) && any(trough_hit_alt > 0)){
    beststart <- start_dates[which.max(trough_hit_alt)]
  }
  
  if(is.na(beststart)){
    warning("No good case study date identified. Try again with less-strict minima criteria.")
    fc_plot <- NULL
    fc_all_plot <- NULL
    
  }else{
    #Plot the best start date, ending at the last trough+14 days or 90-days just for display
    plot_end <- max(90, 14 + (max(gage_yr$gage_data[gage_yr$gage_data$trough,gage_yr$date_col]) - beststart))
    fc_plot <- gage_yr$plot_baseflow_forecast(start_date = beststart,
                                              forecast_days = 0:plot_end,
                                              AGWRC = AGWRC)
    
    #Plot all attempts until beststart is reached on the same plot and use
    #dotted lines to show flow after best start. Red dots will indicate other
    #potential start dates or troughs
    fc_all_plot <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = gage_yr$gage_data[1:which(gage_yr$gage_data[,gage_yr$date_col] == start_dates[1]),],
        ggplot2::aes(x = !!ggplot2::sym(gage_yr$date_col),
                     y = !!ggplot2::sym(gage_yr$flow_col)),
        linetype = 1
      ) +
      ggplot2:: geom_line(
        data = gage_yr$gage_data[which(gage_yr$gage_data[,gage_yr$date_col] == start_dates[1]) : nrow(gage_yr$gage_data),],
        ggplot2::aes(x = !!ggplot2::sym(gage_yr$date_col),
                     y = !!ggplot2::sym(gage_yr$flow_col)),
        linetype = 2
      ) +
      ggplot2::geom_point(aes(x = start_dates, y = gage_df[gage_df$trough,gage_yr$flow_col]),col = "darkred") +
      ggplot2::scale_y_log10() +
      ggplot2::labs( y = "Flow (cfs)", x = NULL,
            title = paste0("USGS-",gage_obj$gage_id, " BF Forecast, ",yr)) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    #For each forecast using start_dates prior to best start, plot the data on
    #the plot as either an orange or blue line
    plot_data <- all_data#[1:which(names(all_data) == beststart)]
    for(i in 1:length(plot_data)){
      plot_datai <- plot_data[[i]]
      #If the data represents the beststart, plot as a solid blue line.
      #Otherwise, plot as a solid/dotted orange line
      if( plot_datai$Date[1] == (beststart + 1) ){
        fc_all_plot <- fc_all_plot +
          ggplot2:: geom_line(
            data = plot_datai[1:plot_end,],
            ggplot2::aes(x = .data$Date, y = .data$Forecast),
            col = "#1f77b4"
          )
      }else{
        if(plot_datai$Date[1] > (beststart + 1)){
          fc_all_plot <- fc_all_plot +
            ggplot2::geom_line(
              data = plot_datai[1 : (which(plot_datai$Date == (beststart + plot_end))),],
              ggplot2::aes(x = .data$Date, y = .data$Forecast) ,
              col = "#8c564b", linetype = 3
            )
        }else{
          fc_all_plot <- fc_all_plot +
            ggplot2::geom_line(
              data = plot_datai[1:which(plot_datai$Date == start_dates[i + 1]),],
              ggplot2::aes(x = .data$Date, y = .data$Forecast) ,
              col = "#ff7f0e"
            ) +
            ggplot2::geom_line(
              data = plot_datai[which(plot_datai$Date == start_dates[i + 1]) : (which(plot_datai$Date == (beststart + plot_end))),],
              ggplot2::aes(x = .data$Date, y = .data$Forecast) ,
              col = "#ff7f0e", linetype = 3
            )
        }
        
      }
      
    }
  }
  #Return a list of data with results and plot
  return(
    list(
      beststart = beststart,
      all_data = all_data,
      analysis_data = analysis_data,
      fc_plot = fc_plot,
      fc_all_plot = fc_all_plot
    )
  )
}


#'@name bf_forecast_start_date
#'@title bf_forecast_start_date
#'@description Find appropriate start dates for baseflow forecasting
#'@details This function assists users in finding an appropriate start date to
#'  conduct a baseflow forecast using the methods on \code{WaterGageDaily()}.
#'  Such forecasts are recommended to start from a low point in the season of
#'  interest where ET is constant and no recharge is occurring. This function
#'  allows users to enter a start date and then find an appropriate start in the
#'  prior x-days or between two dates by identifying valid minimum flow.
#'@param start_date A character or date vector of length 1 identifying the
#'  starting point of the user analysis that will be adjusted by
#'  adjust_start_date
#'@param adjust_start_date Either an integer vector of length 1 or a
#'  character/Date vector of length 2. If a integer is entered, an appropriate
#'  start date will be searched for in the prior adjust_start_date days before
#'  start_date. Otherwise, if a character/Date vector is input, an appropriate
#'  starting date will be searched between the input dates
#'@param gage_data A data frame containing flow and date field identified by
#'  flow_col and date_col, often derived via \code{WaterGageDaily()}
#'@param flow_col The field containing flow values that will be searched for
#'  minimum values in the user dates of interest
#'@param date_col The field containing date values that contain the date
#'  indexing
#'@return A Date representing the adjusted start date, which may be identical to
#'  start_date
#'@examples \dontrun{
#'gage_obj <- WaterGageDaily$new(
#'  gage_id = '02069700', ds_in = ds,
#'  start_date = "2002-01-01",end_date = "2002-12-31"
#')
#'bf_forecast_start_date(start_date = "2002-08-01",
#'                       adjust_start_date = 30,
#'                       gage_data = gage_obj$gage_data,
#'                       flow_col = gage_obj$flow_col,
#'                       date_col = gage_obj$date_col)
#'bf_forecast_start_date(start_date = "2002-08-01",
#'                       adjust_start_date = c("2002-05-01","2002-07-01"),
#'                       gage_data = gage_obj$gage_data,
#'                       flow_col = gage_obj$flow_col,
#'                       date_col = gage_obj$date_col)
#'}
#'@export bf_forecast_start_date
bf_forecast_start_date <- function(start_date,
                                   adjust_start_date,
                                   gage_data,
                                   flow_col = "value", date_col = "time"){
  #Default value is to keep user input start date
  new_start_date <- start_date
  adjust_error <- FALSE
  
  #adjust_start_date is either a single length numeric or a character/date
  #vector of length 2
  if(is.numeric(adjust_start_date) & length(adjust_start_date) == 1){
    #Identify the lowest flow within the past 30 rows of data and begin forecast
    #from that low point
    date_range <- seq.Date(as.Date(start_date) - adjust_start_date, start_date)
    gage_date_range <- gage_data[gage_data[,date_col] <= max(date_range) &
                                   gage_data[,date_col] >= min(date_range),]
    if(nrow(gage_date_range) > 0){
      #Minimum flow during period
      Q0 <- min(gage_date_range[,flow_col], na.rm = TRUE)
      #Adjusted start date
      new_start_date <- max(gage_date_range[gage_date_range[,flow_col] == Q0, date_col])
    }
    
    
  }else if( (lubridate::is.Date(adjust_start_date) | is.character(adjust_start_date)) &&
            length(adjust_start_date) == 2){
    #Find a start date in the range provided by the user assuming a date
    #vector provided
    date_range <- tryCatch(as.Date(adjust_start_date),error = function(e) NULL)
    if(is.null(date_range) || any(is.na(date_range))){
      adjust_error <- TRUE
    }else{
      #Identify the lowest flow within the provided range
      gage_date_range <- gage_data[gage_data[,date_col] <= max(date_range) &
                                     gage_data[,date_col] >= min(date_range),]
      if(nrow(gage_date_range) > 0){
        #Minimum flow during period
        Q0 <- min(gage_date_range[,flow_col], na.rm = TRUE)
        #Adjusted start date
        new_start_date <- max(gage_date_range[gage_date_range[,flow_col] == Q0, date_col], na.rm = TRUE)
      }
      
    }
    
  }else{
    #Non-standard inputs
    adjust_error <- TRUE
  }
  #If a non-standard adjustment was use, return a message warning user and
  #simply keep the start date
  if(adjust_error){
    message("Non-standard date adjustment. Start date was kept as ",
            start_date,". Please see help menu for more options")
  }
  
  return(new_start_date)
}
