#' Generate a table of CIA model result summary statistics for a model, comparing across scenarios
#'
#' @param model_info model json list
#' @param runid.list list of runids of interest 
#' @param metric.list list of metrics of interest  
#' @param include.elfgen include elfgen metrics TRUE or FALSE
#' @param site vhydro url
#' @param site_base base vahydro url
#' @param rdigits Number of digits to round
#' @return stats.df dataframe of summary stats
#' @seealso NA
#' @export om_model_table
#' @examples NA
om_model_table <- function (
  model_info = FALSE,
  runid.list = c('runid_6011','runid_6012'),
  metric.list = c("Qout","Qbaseline","l30_Qout",
                       "l90_Qout","consumptive_use_frac","wd_cumulative_mgd","ps_cumulative_mgd"),
  include.elfgen = FALSE,
  site = "http://deq1.bse.vt.edu:81/d.dh",
  site_base = "http://deq1.bse.vt.edu:81",
  rdigits = 2
) {
  
  if (is.logical(model_info)){
    message("Must supply model_info")
  }
  
  
  ################################################################################################
  # FORMAT MODEL STATS
  ################################################################################################
  model_summary <- data.frame()
  scenario_short_name_list <- data.frame()
  scenario_short_name_list <- FALSE
  
  #i <- 1
  for (i in 1:length(runid.list)){
    runid.i <- runid.list[i]
    run.i <- sub("runid_", "", runid.i)
    
    # RETRIEVE SCENARIO "SHORT NAME"
    run_info.fac <- find_name(model_info,runid.i)
    if (is.null(run_info.fac$reports)) {
      scenario_short_name.i <- runid.i
    } else {
      ri <- run_info.fac$reports
      scenario_short_name.i <- as.character(ri$scenario_short_name$value)
    }
    if (is.logical(scenario_short_name_list)) {
      scenario_short_name_list <- data.frame(scenario = scenario_short_name.i)
    } else {
      scenario_short_name_list <- rbind(scenario_short_name_list, data.frame(scenario = scenario_short_name.i))
    }
    
    # RETRIEVE MODEL STATS
    default_info = list(
      remaining_days_p0 = list(
        "name" = "remaining_days_p0",
        "value" = "NA"
      ),
      remaining_days_p10 = list(
        "name" = "remaining_days_p10",
        "value"  = "NA"
      ),
      remaining_days_p50 = list(
        "name" = "remaining_days_p50",
        "value"  = "NA"
      )
    )

    run_info <- find_name(model_info,runid.i)
    run_info <- merge.list(run_info, default_info)
    summary.i <- list()
    for (j in 1:length(metric.list)) {
      varname <- metric.list[j]
      varvalue <- om.as.numeric(find_name(run_info,varname)$value)
      if (is.numeric(varvalue)) {
        varvalue <- format(round(varvalue,rdigits), big.mark=",")
      }
      summary.i[varname] <- varvalue
    }
    summary.i <- as.data.frame(summary.i)
    
    # ADD ELFGEN STATS TO TABLE
    if (isTRUE(include.elfgen)) {
      default_info.elfgen = list(
        richness_change_abs = list(
          "name" = "richness_change_abs",
          "value" = 'No elfgen Available'
        ),
        richness_change_pct = list(
          "name" = "richness_change_pct",
          "value" = 'No elfgen Available'
        )
      )
      elfgen.metric.list <- c('richness_change_abs','richness_change_pct')
      run_info.elfgen <- find_name(run_info,'elfgen_EDAS_huc8')
      run_info.elfgen <- merge.list(run_info.elfgen, default_info.elfgen)

      elfgen_summary.i <- list()
      for (j in 1:length(elfgen.metric.list)) {
        varname <- elfgen.metric.list[j]
        varvalue <- om.as.numeric(find_name(run_info.elfgen,varname)$value)
        if (is.numeric(varvalue)) {
          varvalue <- format(round(varvalue,rdigits), big.mark=",")
        }
        elfgen_summary.i[varname] <- varvalue
      }
      elfgen_summary.i <- as.data.frame(elfgen_summary.i)
      names(elfgen_summary.i) <- elfgen.metric.list
      summary.i <- cbind(summary.i,elfgen_summary.i)
      }
      
      
    if (nrow(summary.i) > 0) {
      summary.i <- cbind("runid" = run.i,"model" = model_info[[1]]$name,summary.i)
      model_summary <- rbind(model_summary,summary.i)
    }
  }
  
  #ADD "scenario_short_name" TO DATAFRAME
  model_summary <- cbind(scenario_short_name_list,model_summary)
  
  #TRANSPOSE DATAFRAME
  model_summary.T <- as.data.frame(t(model_summary[,-1]))
  colnames(model_summary.T) <- model_summary[,1]
  
  stats.df <- model_summary.T
  return(stats.df)
}

om.as.character <- function(x, default = NA) {
  if (length(x) == 0) {
    return(as.character(default))
  }
  return(as.character(x))
}

om.as.numeric <- function(x, default = NA) {
  if (length(x) == 0) {
    return(as.numeric(default))
  }
  return(as.numeric(x))
}
