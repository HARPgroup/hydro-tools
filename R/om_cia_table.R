#' Generate a table of CIA model result summary statistsics for both Rseg and Facility models, comparing across scenarios
#'
#' @param fac_model_info facility json list
#' @param rseg_model_info riverseg json list
#' @param runid.list list of runids of interest 
#' @param fac.metric.list list of facility metrics of interest
#' @param rseg.metric.list list of riverseg metrics of interest  
#' @param site vhydro url
#' @param site_base base vahydro url
#' @return stats.df dataframe of summary stats
#' @seealso NA
#' @export om_cia_table
#' @examples NA
om_cia_table <- function (
  fac_model_info = FALSE,
  rseg_model_info = FALSE,
  runid.list = c('runid_6011','runid_6012'),
  fac.metric.list = c('wd_mgd','ps_mgd','unmet30_mgd'),
  rseg.metric.list = c("Qout","Qbaseline","l30_Qout",
                       "l90_Qout","consumptive_use_frac","wd_cumulative_mgd","ps_cumulative_mgd"),
  site = "http://deq1.bse.vt.edu:81/d.dh",
  site_base = "http://deq1.bse.vt.edu:81"
) {
  
  if (is.logical(fac_model_info)){
    message("Must supply fac_model_info")
  }
  
  if (is.logical(rseg_model_info)){
    message("Must supply rseg_model_info")
  }
  
  ################################################################################################
  # FORMAT FAC & RSEG MODEL STATS
  ################################################################################################
  fac_summary <- data.frame()
  rseg_summary <- data.frame()
  scenario_short_name_list <- data.frame()
  scenario_short_name_list <- FALSE
  
  #i <- 1
  for (i in 1:length(runid.list)){
    runid.i <- runid.list[i]
    run.i <- sub("runid_", "", runid.i)
    
    # RETRIEVE SCENARIO "SHORT NAME"
    run_info.fac <- find_name(fac_model_info,runid.i)
    if (is.null(run_info.fac$reports)) {
      scenario_short_name.i <- runid.i
    } else {
      ri <- run_info.fac$reports
      scenario_short_name.i <- ri$scenario_short_name$value
    }
    if (is.logical(scenario_short_name_list)) {
      scenario_short_name_list <- data.frame(scenario = scenario_short_name.i)
    } else {
      scenario_short_name_list <- rbind(scenario_short_name_list, data.frame(scenario = scenario_short_name.i))
    }
    
    # RETRIEVE FAC MODEL STATS 
    fac_summary.i <- list()
    for (j in 1:length(fac.metric.list)) {
      varname <- fac.metric.list[j]
      varvalue <- find_name(run_info.fac,varname)$value
      fac_summary.i[varname] <- as.numeric(varvalue)
    }
    fac_summary.i <- as.data.frame(fac_summary.i)
    fac_summary.i <- round(fac_summary.i,2)

    if (nrow(fac_summary.i) > 0) {
      fac_summary.i <- cbind("runid" = run.i,"fac_model" = fac_model_info[[1]]$name,fac_summary.i)
      fac_summary <- rbind(fac_summary,fac_summary.i)
    }
    
    # RETRIEVE RSEG MODEL STATS
    default_info.rseg = list(
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

    run_info.rseg <- find_name(rseg_model_info,runid.i)
    run_info.rseg <- merge.list(run_info.rseg, default_info.rseg)
    rseg_summary.i <- list()
    for (j in 1:length(rseg.metric.list)) {
      varname <- rseg.metric.list[j]
      varvalue <- find_name(run_info.rseg,varname)$value
      rseg_summary.i[varname] <- as.numeric(varvalue)
    }
    rseg_summary.i <- as.data.frame(rseg_summary.i)
    rseg_summary.i <- round(rseg_summary.i,2)
    
    # ADD ELFGEN STATS TO TABLE
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
    run_info.elfgen <- merge.list(run_info.elfgen, default_info.elfgen)
    
    elfgen_summary.i <- list()
    for (j in 1:length(elfgen.metric.list)) {
      varname <- elfgen.metric.list[j]
      varvalue <- find_name(run_info.elfgen,varname)$value
      elfgen_summary.i[varname] <- varvalue
      if (is.numeric(elfgen_summary.i[varname])) {
        elfgen_summary.i[varname] <- round(elfgen_summary.i[varname],2)
      }
    }
    elfgen_summary.i <- as.data.frame(elfgen_summary.i)
    
    names(elfgen_summary.i) <- elfgen.metric.list
    
    rseg_summary.i <- cbind(rseg_summary.i,elfgen_summary.i)

    if (nrow(rseg_summary.i) > 0) {
      #rseg_summary.i <- cbind("runid" = run.i,"run_date" = rseg.info.i$run_date,"starttime" = str_remove(rseg.info.i$starttime," 00:00:00"),"endtime" = str_remove(rseg.info.i$endtime," 00:00:00"),rseg_summary.i)
      rseg_summary.i <- cbind("runid" = run.i,"rseg_model" = rseg_model_info[[1]]$name,rseg_summary.i)
      rseg_summary <- rbind(rseg_summary,rseg_summary.i)
    }
  }
  
  ################################################################################################
  # JOIN FAC AND RSEG MODEL STATS INTO SINGLE TABLE
  ################################################################################################
  rseg.met.list <- paste(rseg.metric.list, collapse = ",")
  fac.met.list <- paste(fac.metric.list, collapse = ",")
  # fac_rseg_stats <- sqldf(
  #   paste(
  #     "SELECT a.runid,a.run_date, a.starttime, a.endtime, a.riverseg,' ' AS Rseg_Stats,", rseg.met.list,
  #     ", a.richness_change_abs, a.richness_change_pct, ' ' AS Facility_Stats,",fac.met.list," 
  #     FROM rseg_summary AS a     
  #     LEFT OUTER JOIN fac_summary AS b     
  #     ON a.runid = b.runid")
  # )
  fac_rseg_stats <- sqldf(
    paste(
      "SELECT a.runid,' ' AS Rseg_Stats,a.rseg_model,", rseg.met.list,
      ", a.richness_change_abs, a.richness_change_pct, ' ' AS Facility_Stats,b.fac_model,",fac.met.list," 
      FROM rseg_summary AS a     
      LEFT OUTER JOIN fac_summary AS b     
      ON a.runid = b.runid")
  )
  
  #ADD "scenario_short_name" TO DATAFRAME
  fac_rseg_stats <- cbind(scenario_short_name_list,fac_rseg_stats)
  
  #TRANSPOSE DATAFRAME
  fac_rseg_stats.T <- as.data.frame(t(fac_rseg_stats[,-1]))
  colnames(fac_rseg_stats.T) <- fac_rseg_stats[,1]
  #View(fac_rseg_stats.T)
  #pandoc.table(fac_rseg_stats.T, style = "rmarkdown", split.table = 120)
  
  stats.df <- fac_rseg_stats.T
  return(stats.df)
}