#' Generate a table of model demand inputs, comparing across scenarios
#'
#' @param model_info model json list
#' @param runid.list list of runids of interest 
#' @return demands.df dataframe of scenario demands
#' @seealso NA
#' @export om_demand_table
#' @examples NA
om_demand_table <- function (
  model_info = FALSE,
  runid.list = c('runid_6011','runid_6012')
) {
  
  if (is.logical(model_info)){
    message("Must supply model_info")
  }
  
  ################################################################################################
  # FORMAT STATS
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

    run_info <- find_name(model_info,runid.i)
    summary.i <- list()
      
      if (startsWith(run.i,"2")){
        #CURRENT CONDITIONS RUNS, DONT DISPLAY VWP DEMANDS 
        summary.i["avg_day_mgd"] <- format(round((as.numeric(find_name(fac_model_info, "current_mgy")$equation$value)/365),2), nsmall = 2) 
        summary.i["max_day_limit_mgd"] <- "NA"
        summary.i["max_ann_limit_mgy"] <- "NA"
      } else if (startsWith(run.i,"4")) {
        #PERMIT TERM MAX RUNS
        summary.i["avg_day_mgd"] <- format(round((as.numeric(find_name(fac_model_info, "vwp_max_mgy")$equation$value)/365),2), nsmall = 2) 
        summary.i["max_day_limit_mgd"] <- format(round(as.numeric(find_name(fac_model_info, "vwp_max_mgd")$equation$value),2), nsmall = 2) 
        summary.i["max_ann_limit_mgy"] <- format(round(as.numeric(find_name(fac_model_info, "vwp_max_mgy")$equation$value),2), nsmall = 2) 
      } else if (startsWith(run.i,"6")) {
        #PROPOSED PERMIT TERM MAX RUNS
        if (find_name(fac_model_info, "vwp_prop_max_mgd")$equation$value == "vwp_max_mgd") {
          summary.i["max_day_limit_mgd"] <- format(round(as.numeric(find_name(fac_model_info, "vwp_max_mgd")$equation$value),2), nsmall = 2) 
        } else {
          summary.i["max_day_limit_mgd"] <- format(round(as.numeric(find_name(fac_model_info, "vwp_prop_max_mgd")$equation$value),2), nsmall = 2) 
        }
        if (find_name(fac_model_info, "vwp_prop_max_mgy")$equation$value == "vwp_max_mgy") {
          summary.i["max_ann_limit_mgy"] <- format(round(as.numeric(find_name(fac_model_info, "vwp_max_mgy")$equation$value),2), nsmall = 2) 
          summary.i["avg_day_mgd"] <- format(round((as.numeric(find_name(fac_model_info, "vwp_max_mgy")$equation$value)/365),2), nsmall = 2) 
        } else {
          summary.i["max_ann_limit_mgy"] <- format(round(as.numeric(find_name(fac_model_info, "vwp_prop_max_mgy")$equation$value),2), nsmall = 2) 
          summary.i["avg_day_mgd"] <- format(round((as.numeric(find_name(fac_model_info, "vwp_prop_max_mgy")$equation$value)/365),2), nsmall = 2) 
        }
      } else {
        #FUTURE: NEED TO MAP IN ADDITIONAL RUNIDS
        summary.i["avg_day_mgd"] <- "Need to add runid mapping to om_demand_table()"
        summary.i["max_day_limit_mgd"] <- "Need to add runid mapping to om_demand_table()"
        summary.i["max_ann_limit_mgy"] <- "Need to add runid mapping to om_demand_table()"
      } 
        
    summary.i <- as.data.frame(summary.i)
      
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
  
  demands.df <- model_summary.T
  return(demands.df)
}