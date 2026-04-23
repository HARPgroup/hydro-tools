# Facility Utilities

#' Display the monthly percentiles of flows from minimum to 50% and mean flows
#' of the given flow data
#' @description Using a data frame provided by user, summarize the q_col to
#'   create an output that shows the minimum, 5 - 50%, and mean of that column,
#'   grouped by mo_col. The most common use is to quickly see flow percentiles
#'   of model or gage data across months
#' @param df2sum A data frame that contains at least a column to be summarized
#'   (q_col, typically flow) and a grouping column (mo_col, typically month)
#' @param q_col The name of the column in df2sum to be summarized and grouped by mo_col
#' @param mo_col A grouping column used to derive the percentiles of q_col
#' @param rdigits The number of digits to report/round percentiles to
#' @return A data frame that has 8 columns: Month, Minimum, 5%, 10%, 25%, 30%,
#'   50%, Mean. These columns represent the correponsing percentile or statistic
#'   of q_col grouped by mo_col and rounded to rdigits. the names of the data
#'   frame correspond to these statistics. The data is organized by month.
#' @export om_flow_table
om_flow_table <- function(df2sum, q_col = "Qout", mo_col = "month", rdigits = 1) {
  # Expects a data frame, df2sum with Qout, month columns
  if (zoo::is.zoo(df2sum)) {
    df2sum <- as.data.frame(df2sum)
  }
  #Round the mean to one less digit, accounting for some uncertainty/sigfigs
  if (rdigits > 1) {
    mrdigits = rdigits - 1
  } else {
    mrdigits = rdigits
  }
  #Initialize a data frame of monthly summary metrics
  intake_summary_tbl <- data.frame(
    "Month" = month.abb,
    'Min' = numeric(12),
    fivep=  numeric(12),
    tenp = numeric(12),
    twofivep = numeric(12),
    thirtyp = numeric(12),
    fiftyp = numeric(12),
    'Mean' = numeric(12),
    stringsAsFactors = FALSE
  )
  #For each month, compute the mean, median, minimum flow and various other
  #quantile flows
  for (i in index(month.abb)) {
    moname <- month.abb[i]
    drows <- df2sum[df2sum[,mo_col] == i,]
    q_drows <- stats::quantile(drows[,q_col], probs=c(0,0.05,0.1,0.25, 0.3, 0.5), na.rm = TRUE)
    q_mean <- mean(drows[,q_col], na.rm = TRUE)
    intake_summary_tbl[i,'Min'] <- round(as.numeric(q_drows["0%"]),rdigits)
    intake_summary_tbl[i,"fivep"] <- round(as.numeric(q_drows["5%"]),rdigits)
    intake_summary_tbl[i,"tenp"] <- round(as.numeric(q_drows["10%"]),rdigits)
    intake_summary_tbl[i,"twofivep"] <- round(as.numeric(q_drows["25%"]),rdigits)
    intake_summary_tbl[i,"thirtyp"] <- round(as.numeric(q_drows["30%"]),rdigits)
    intake_summary_tbl[i,"fiftyp"] <- round(as.numeric(q_drows["50%"]),rdigits)
    intake_summary_tbl[i,'Mean'] <- round(q_mean,mrdigits)
  }
  #Rename entries in the tbale for display
  names(intake_summary_tbl) <- c('Month', 'Min', '5%', '10%', '25%', '30%', '50%', 'Mean')
  return(intake_summary_tbl)
}

#' Difference in monthly percentiles
#' @description Using a data frame provided by user, find the monthly
#'   percentiles of cu_pre_var and cu_post_var using om_flow_table. Report the
#'   differences in these monthly percentiles or the percentage change (post to
#'   pre)
#' @param pr_data A data frame that contains month and the user input
#'   cu_post_var and cu_pre_var columns. This data frame is likely made up of
#'   multiple model run files pulled via fn_get_runfile
#' @param cu_post_var The numeric column in pr_data that represents a "post"
#'   alteration scenario i.e. Qout on a river node from run 600 in the OM
#' @param cu_pre_var The numeric column in pr_data that represents a "pre"
#'   alteration scenario i.e. Qout on a river node from run 100 in the OM
#' @param output_style Should the differences in monthly percentiles be reported
#'   as percentage change ("percent") from cu_pre_var or as the numeric
#'   difference ("numeric")?
#' @param cu_decimals How many decimals should the output be rounded to?
#' @param cu_min_valid Minimum flow value to display, otherwise assumed below
#'   model valid range as output will be NA
#' @return A data frame that has either the percent or absolute difference in
#'   monthly percentile between cu_pre_var and cu_post_var depending on
#'   output_style. The month is included as the first column. Subsequent columns
#'   are those output by cu_flow_table i.e. minimum, 5%, 10%, 25%, 50%, and
#'   median
#' @export om_cu_table_data
om_cu_table_data <- function(pr_data,
                             cu_post_var, cu_pre_var,
                             output_style = "percent", cu_decimals, cu_min_valid = 0.0) {
  if ( (cu_post_var == "Qout" & cu_pre_var == "Qbaseline") & !("Qbaseline" %in% names(pr_data))) {
    # perform regular calculations if Qbaseline does not already exist
    pr_data$Qbaseline <- pr_data$Qout + (pr_data$wd_cumulative_mgd - pr_data$ps_cumulative_mgd) * 1.547
  }
  # first do these tables with effectively no rounding (10 decimal places)
  qi_table_noro = om_flow_table(pr_data, q_col = cu_pre_var, mo_col = 'month', rdigits = 10)
  qo_table_noro = om_flow_table(pr_data, cu_post_var, 'month', 10)
  
  # make a copy formatted with months and labels:
  cu_table = qi_table_noro 
  
  
  #Return either the raw difference (output_style = 'numeric') or the percent
  #change from cu_pre_var
  if(output_style == "percent"){
    message("Calculting difference between cu_pre_var and cu_post_var as a 
            percentage change from cu_pre_var. Use the output_style = 'numeric' 
            input to see raw differences.")
    # now, cu_table uses the rounded values for display, but the noro values for calculating 
    # the percent change
    cu_table[,2:ncol(cu_table)] <- round(
      100.0 * (qo_table_noro[,2:ncol(qo_table_noro)] - qi_table_noro[,2:ncol(qi_table_noro)]) 
      / qi_table_noro[,2:ncol(qi_table_noro)],
      cu_decimals
    )
  }else{
    message("Calculting difference between cu_pre_var and cu_post_var as a 
            raw numeric change from cu_pre_var. Use the output_style = 'percent' 
            input to see percent differences.")
    cu_table[,2:ncol(cu_table)] <- round(
      qo_table_noro[,2:ncol(qo_table_noro)] - qi_table_noro[,2:ncol(qi_table_noro)],
      cu_decimals
    )
  }
  
  cu_table <- replace(cu_table, is.na(cu_table), "n/a")
  cu_table <- replace(cu_table, (qi_table_noro < cu_min_valid), "n/a")
  
  
  return(cu_table)
}


#' Difference in monthly percentiles
#' @description Using a data frame and column list provided by user, calculate quantiles 
#' for desired variables.
#' @param rundata_df Model run timeseries dataframe.
#' @param metrics Which metrics to calculate quantiles for? These should be
#'   column names of rundata_df
#' @param quantiles Which quantiles to calculate?
#' @param rdigits Number of Digits to round column values.
#' @return A data frame that if has one row per variable with quantiles in columns
#' @export om_quantile_table
om_quantile_table <- function(
    rundata_df, 
    metrics = c("Qintake","Runit"), 
    quantiles = c(0,0.1,0.25,0.5,0.75,0.9,1.0), 
    rdigits = 2
  ) {
  quantile_df <- data.frame(matrix(ncol = length(quantiles), nrow = 0))
  colnames(quantile_df) <- paste0(quantiles*100, "%")
  for (i in 1:length(metrics)){
    if (length(grep(metrics[i],names(rundata_df),value=TRUE)) == 0){
      quantile <- data.frame(rep(NA,length(quantiles)))
      colnames(quantile) <- metrics[i]
      quantile <- t(quantile)
      colnames(quantile) <- paste0(quantiles*100, "%")
      quantile_df <- rbind(quantile_df, quantile)
    } else {
      quantile <- data.frame(quantile(rundata_df[,metrics[i]], probs=quantiles))
      colnames(quantile) <- metrics[i]
      quantile_df <- rbind(quantile_df, t(quantile))
    }
  }
  quantile_df <- round(quantile_df,rdigits)
  return(quantile_df)
}

#' Monthly boxplot with single year highlight
#' @description Create a boxplot of the monthly metric requested by user at
#'   this gage (or of a user input data frame) and then plot the target year
#'   as highlighted points on the plot. Often used in OWS to assess
#'   potential winter baseflow recharge when using gage data and evaluting
#'   between October - March (include_months = c(10,11,12,1,2,3)). 
#' @param targetYear What year should be highlighted on the plot? Defaults
#'   to this year. If left as NULL, no additional point data will be included
#' @param include_months What months should be included in plot/analysis?
#' @param AYS Which month begins the analysis year? The plot will start in
#'   this month and then proceed through the next 12 months or those
#'   included by user in include_months
#' @param dataframe_in A data frame to summarize and use for the plot. Must
#'   contain the fields input in value_col and date_col
#' @param value_col A field name in dataframe_in used to control which column is
#'   plotted from dataframe_in
#' @param date_col A field name in dataframe_in that must contain R Dates, to be
#'   used in developing the monthly metrics
#' @param gage_id A name to display in the plot title
#' @param metric A function to pass to \code{dplyr::summarize()} to control
#'   how data monthly data is aggregated
#' @param metric_name A name to use in plot title to represent the passed metric
#'   function
#' @param use_y_log Boolean. Should the y axis be on a log10 scale? Defaults
#'   to TRUE
#' @param ylab Character. The text to display on the y-axis of the plot.
#' @return A boxplot of monthly mean flows with highlighted points
#' @export plot_boxplot_context
plot_boxplot_context = function(targetYear = as.numeric(format(Sys.Date(),"%Y")),
                                include_months = 1:12,
                                AYS = 10,
                                dataframe_in = NULL,
                                value_col = "flow",
                                date_col = "time",
                                gage_id = NA,
                                metric = mean,
                                metric_name = "mean",
                                use_y_log = TRUE,
                                ylab = "Monthly Mean Flow (cfs)"){
  #metric name for plotting labels
  metric_name <- paste0(toupper(substring(metric_name, 1, 1)), substring(metric_name, 2))
  
  #Create columns for the month and year in the gage data
  plotData <- dataframe_in
  plotData$month <- as.numeric(format(plotData[,date_col], "%m"))
  plotData$year <- as.numeric(format(plotData[,date_col], "%Y"))
  #Increment year per user start month to reflect the order data should be
  #included in plot
  plotData$analysis_year <- plotData$year
  plotData$analysis_year[plotData$month >= AYS] <- plotData$analysis_year[plotData$month >= AYS] + 1
  #Group by month and year and find the metric of the value_col requested by
  #user in every month-year. Then filter out month-years with incomplete
  #data
  plotData <- plotData |> 
    dplyr::group_by(month,analysis_year) |> 
    dplyr::summarise(aggregateValue = metric(!!dplyr::sym(value_col), na.rm = TRUE),
                     ndays = dplyr::n()) |> 
    dplyr::mutate(daysInMonth = lubridate::days_in_month(paste0(analysis_year,"-",month,"-01"))) |> 
    dplyr::filter(daysInMonth == ndays) |> 
    #Only include the months requested by the user and relabel all months
    #using their abbreviations
    dplyr::filter(month %in% include_months) |> 
    dplyr::mutate(month = month.abb[month])
  #Store the months as a factor, with ordered levels based on user input
  plotData$month <- factor(plotData$month,levels = month.abb[include_months])
  
  #Create a boxplot of the month-year mean flows and include the targetData
  #as point data to highlight this year's trends
  p <- ggplot() + 
    geom_boxplot(data = plotData, aes(group = month, x = month, y = aggregateValue)) +
    ggplot2::labs(color = element_blank()) + 
    ggplot2::xlab(element_blank()) + 
    ggplot2::ylab(ylab) + 
    theme_minimal()
  #If the user wishes to use a log scale on the y-axis
  if(use_y_log){
    #Use a semi-log (y) plot
    p <- p + scale_y_log10()
  }
  
  if(!is.null(targetYear)){
    #Select only data from the user selected target year to additionally
    #include on the plot
    targetData <- plotData[plotData$analysis_year == targetYear,]
    
    p <- p + 
      geom_point(data = targetData, aes(x = month, y = aggregateValue,
                                        col = as.character(targetYear)),
                 pch = 12) +
      #Color and label the point values
      ggplot2::scale_color_manual(values = "blue") + 
      ggplot2::ggtitle(paste(metric_name,value_col,targetYear,"\nvs. Hist. Monthly",metric_name,"\nUSGS",gage_id)) + 
      theme(plot.title = element_text(hjust = 0.5)) 
    #Add the year to the plot object name
  }else{
    p <- p + 
      ggplot2::ggtitle(paste("Hist. Monthly",metric_name,"\nUSGS",gage_id)) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  return(p)
}
