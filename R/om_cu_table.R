#' Generate a table of CIA model result summary statistsics for both Rseg and Facility models, comparing across scenarios
#'
#' @param fac_report_info facility json list
#' @param pr_data riverseg json list
#' @param cu_post_var list of runids of interest 
#' @param cu_pre_var list of facility metrics of interest
#' @param cu_threshold list of riverseg metrics of interest  
#' @param cu_decimals vhydro url
#' @return dataframe of summary stats
#' @seealso NA
#' @export om_cu_table
#' @examples NA
om_cu_table <- function(fac_report_info, pr_data, cu_post_var, cu_pre_var, cu_threshold, cu_decimals) {
  if ( (cu_post_var == "Qout" & cu_pre_var == "Qbaseline") ) {
    # regular calculations
    pr_data$Qbaseline <- pr_data$Qout + (pr_data$wd_cumulative_mgd - pr_data$ps_cumulative_mgd) * 1.547
  }
  pr_data$cu_daily <- 100.0 * (
    (pr_data[,cu_post_var] - pr_data[,cu_pre_var]) / pr_data[,cu_pre_var]
  )
  qi_table = om_flow_table(pr_data, cu_pre_var, 'month', cu_decimals)
  qo_table = om_flow_table(pr_data, cu_post_var, 'month', cu_decimals)
  cu_table = qi_table # make a copy formatted with months and labels
  cu_table[,2:ncol(cu_table)] <- round(
    100.0 * (qo_table[,2:ncol(qo_table)] - qi_table[,2:ncol(qi_table)]) 
    / qi_table[,2:ncol(qi_table)]
  )
  cu_table <- replace(cu_table, is.na(cu_table), "n/a")
  cu_table <- replace(cu_table, (qi_table < cu_min_valid), "n/a")
  
  qcu_table = qo_table
  qcu_colors = matrix(nrow = nrow(qo_table), ncol = ncol(qo_table))
  rn = 0
  for (r in rownames(qo_table)) {
    rn = rn + 1
    cn = 0
    for (c in colnames(qo_table[r,])) {
      cn = cn + 1
      qcu_colors[rn,cn] = "white"
      if (!is.na(as.numeric(cu_table[r,c]))) {
        if ( as.numeric(cu_table[r,c]) <= cu_threshold[1]) {
          qcu_colors[rn,cn] = "yellow"
        } 
        if ( as.numeric(cu_table[r,c]) <= cu_threshold[2]) {
          qcu_colors[rn,cn] = "orange"
        } 
        if ( as.numeric(cu_table[r,c]) <= cu_threshold[3]) {
          qcu_colors[rn,cn] = "red"
        } 
        # qcu_table[r,c] <- paste0( qo_table[r,c], " (", cu_table[r,c],"%)")
      }
      qcu_table[r,c] <- paste0( qo_table[r,c], "\n(", cu_table[r,c],"%)")
    }
  }
  
  fqcu_table <- flextable::flextable(qcu_table)
  fqcu_table <- bg(fqcu_table, bg = "#EFEFEF", part = "header")
  
  for (i in 1:nrow(qcu_colors)) {
    for (j in 1:ncol(qcu_colors)) {
      fqcu_table <- bg(fqcu_table, i, j, bg = qcu_colors[i,j])
    }
  }
  
  fqcu_caption <- paste(
    "Modeled monthly consumptive use statistics in the ", 
    rseg_feature$name, "in cubic feet per second (cfs). Columns show the modeled non-exceedance flow percentiles and the consumptive user % due to cumulative demands for", 
    run_info$reports$scenario_name$value, 
    ". Simulated demands include all up-stream demands and demands at ", 
    fac_feature$name, fac_report_info$intake_name$value, 
    ") and cumulative return flows.  ", 
    "Fields that are marked as 'n/a' indicate that the baseline flow for that time period/percentile was below the model accuracy threshold of ", cu_min_valid,"cfs."
  )
  fqcu_table$caption_text <- fqcu_caption
  #fqcu_table <- set_caption(
  #  fqcu_table,
  #  caption = fqcu_caption, # this doesn't format very nicely and doesn't auto-number
  #  autonum = NULL,
  #  word_stylename = "Table Caption",
  #  fp_p = NULL,
  #  align_with_table = TRUE,
  #  html_classes = NULL,
  #  html_escape = TRUE
  #)
  return(fqcu_table)
}
