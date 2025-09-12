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
  if (is.zoo(df2sum)) {
    df2sum <- as.data.frame(df2sum)
  }
  intake_summary_tbl = data.frame(
    "Month" = character(), 
    'Min' = numeric(),
    '5%' = numeric(),
    '10%' = numeric(),
    '25%' = numeric(), 
    '30%' = numeric(),
    '50%' = numeric(),
    'Mean' = numeric(),
    stringsAsFactors = FALSE) ;
  if (rdigits > 1) {
    mrdigits = rdigits - 1
  } else {
    mrdigits = rdigits
  }
  for (i in index(month.abb)) {
    moname <- month.abb[i]
    drows <- sqldf(paste("select * from df2sum where ", mo_col, " = ", i))
    q_drows <- quantile(drows[,q_col], probs=c(0,0.05,0.1,0.25, 0.3, 0.5), na.rm=TRUE)
    q_mean <- mean(drows[,q_col])
    newline = data.frame(
      "Month" = moname,
      'Min' = round(as.numeric(q_drows["0%"]),rdigits),
      '5%' = round(as.numeric(q_drows["5%"]),rdigits),
      '10%' = round(as.numeric(q_drows["10%"]),rdigits),
      '25%' = round(as.numeric(q_drows["25%"]),rdigits), 
      '30%' = round(as.numeric(q_drows["30%"]),rdigits),
      '50%' = round(as.numeric(q_drows["50%"]),rdigits),
      'Mean' = round(q_mean,mrdigits),
      stringsAsFactors = FALSE
    )
    intake_summary_tbl <- rbind(intake_summary_tbl, newline)
  }
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





om_quantile_table <- function(rundata_df, metrics = c("Qintake","Runit"), quantiles = c(0,0.1,0.25,0.5,0.75,0.9,1.0), rdigits = 2) {
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
