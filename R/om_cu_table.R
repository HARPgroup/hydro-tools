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
  # first do these tables with effectively no rounding (10 decimal places)
  qi_table_noro = om_flow_table(pr_data, q_col = cu_pre_var, mo_col = 'month', rdigits = 10)
  qo_table_noro = om_flow_table(pr_data, cu_post_var, 'month', 10)
  # now re-do these tables with rounding for formatting purposes
  qi_table = om_flow_table(pr_data, q_col = cu_pre_var, mo_col = 'month', rdigits = cu_decimals)
  qo_table = om_flow_table(pr_data, cu_post_var, 'month', cu_decimals)
  cu_table = qi_table # make a copy formatted with months and labels
  # now, cu_table uses the rounded values for display, but the noro values for calculating 
  # the percent change
  cu_table[,2:ncol(cu_table)] <- round(
    100.0 * (qo_table_noro[,2:ncol(qo_table)] - qi_table_noro[,2:ncol(qi_table_noro)]) 
    / qi_table_noro[,2:ncol(qi_table_noro)]
  )
  cu_table <- replace(cu_table, is.na(cu_table), "n/a")
  cu_table <- replace(cu_table, (qi_table < cu_min_valid), "n/a")
  # for a multidtude of R named colors, see here: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
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
        # consider augmentation to be similar to depletion percentiles.
        if ( as.numeric(cu_table[r,c]) >= -1.0 * cu_threshold[2]) {
          qcu_colors[rn,cn] = "lightblue2"
        } 
        if ( as.numeric(cu_table[r,c]) >= -1.0 * cu_threshold[3]) {
          qcu_colors[rn,cn] = "lightblue3"
        } 
        # qcu_table[r,c] <- paste0( qo_table[r,c], " (", cu_table[r,c],"%)")
        cu_table[r,c] <- sprintf("%+.0f", as.numeric(cu_table[r,c]))
      }
      qcu_table[r,c] <- paste0( qo_table[r,c], "\n(", cu_table[r,c],"%)")
    }
  }
  
  fqcu_table <- flextable::flextable(qcu_table)
  fqcu_table <- flextable::bg(fqcu_table, bg = "#EFEFEF", part = "header")
  
  for (i in 1:nrow(qcu_colors)) {
    for (j in 1:ncol(qcu_colors)) {
      fqcu_table <- flextable::bg(fqcu_table, i, j, bg = qcu_colors[i,j])
    }
  }
  
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
