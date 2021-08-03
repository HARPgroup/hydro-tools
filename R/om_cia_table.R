#' Generate a table of CIA model result summary statistsics for both Rseg and Facility models, comparing across scenarios
#'
#' @param rseg.hydroid riverseg dh_feature hydroid
#' @param fac.hydroid facility dh_feature hydroid
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
  #rseg.hydroid = 462757,
  #fac.hydroid = 71977,
  fac_model_info = FALSE,
  rseg_model_info = FALSE,
  runid.list = c('runid_401','runid_601'),
  fac.metric.list = c('wd_mgd','ps_mgd','unmet1_mgd','unmet7_mgd','unmet30_mgd','unmet90_mgd'),
  rseg.metric.list = c('Qout','Qbaseline','remaining_days_p0','remaining_days_p10','remaining_days_p50','l30_Qout',
                        'l90_Qout','consumptive_use_frac','wd_cumulative_mgd','ps_cumulative_mgd')#,
  #site = "https://deq1.bse.vt.edu/d.dh",
  #site_base = "https://deq1.bse.vt.edu"
) {
  
  if (is.logical(fac_model_info)){
    message("Must supply fac_model_info")
  }
  
  if (is.logical(rseg_model_info)){
    message("Must supply rseg_model_info")
  }
  
  ################################################################################################
  # RETRIEVE FAC & RSEG MODEL STATS
  ################################################################################################
  # rseg.model <- om_get_model(site, rseg.hydroid)
  # rseg.elid <- om_get_prop(site, rseg.model$pid, entity_type = 'dh_properties','om_element_connection')$propvalue
  
  # fac.model <- om_get_model(site, fac.hydroid, model_varkey = 'om_water_system_element')
  # fac_obj_url <- paste(json_obj_url, fac.model$pid, sep="/")
  # fac_model_info <- om_auth_read(fac_obj_url, token,  "text/json", "")
  # fac_model_info <- fromJSON(fac_model_info)
  
  fac_summary <- data.frame()
  rseg_summary <- data.frame()
  scenario_short_name_list <- data.frame()
  scenario_short_name_list <- FALSE
  
  #i <- 2
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
    fac_summary.i <- as.data.frame(run_info.fac[fac.metric.list])[paste0(fac.metric.list,".value")]
    names(fac_summary.i) <- fac.metric.list
      
    if (nrow(fac_summary.i) > 0) {
      fac_summary <- rbind(fac_summary,fac_summary.i)
    }
    
    #----------------------------------------------------------------------------------------------------------------
    
    default_info.rseg = list(
      remaining_days_p0 = list(
        "name" = "remaining_days_p0",
        "value" = NA
      ),
      remaining_days_p10 = list(
        "name" = "remaining_days_p10",
        "value"  = NA
      ),
      remaining_days_p50 = list(
        "name" = "remaining_days_p50",
        "value"  = NA
      )
    )
   
    # RETRIEVE RSEG MODEL STATS
    run_info.rseg <- find_name(rseg_model_info,runid.i)
    run_info.rseg <- merge.list(run_info.rseg, default_info.rseg)
    rseg_summary.i <- as.data.frame(run_info.rseg[rseg.metric.list])[paste0(rseg.metric.list,".value")]
    names(rseg_summary.i) <- rseg.metric.list
 
    
    # ADD ELFGEN STATS TO TABLE -------------------------------------------------------------------------------------
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
    
    run_info.elfgen <- find_name(run_info.rseg,'elfgen_EDAS_huc8')
    run_info.elfgen <- merge.list(run_info.elfgen, default_info.elfgen)
    elfgen_summary.i <- as.data.frame(run_info.elfgen[elfgen.metric.list])[paste0(elfgen.metric.list,".value")]
    names(elfgen_summary.i) <- elfgen.metric.list
    
    rseg_summary.i <- cbind(rseg_summary.i,elfgen_summary.i)
    #----------------------------------------------------------------------------------------------------------------
    
    
    #rseg.info.i <- fn_get_runfile_info(rseg.elid,run.i,site=site_base)
    if (nrow(rseg_summary.i) > 0) {
      #rseg_summary.i <- cbind("runid" = run.i,"run_date" = rseg.info.i$run_date,"starttime" = str_remove(rseg.info.i$starttime," 00:00:00"),"endtime" = str_remove(rseg.info.i$endtime," 00:00:00"),rseg_summary.i)
      rseg_summary <- rbind(rseg_summary,rseg_summary.i)
    }
  }
  
  #RENAME COLUMN IN scenario_short_name_list
  #colnames(scenario_short_name_list)<-c("Scenario")
  
  ################################################################################################
  # JOIN FAC AND RSEG MODEL STATS INTO SINGLE TABLE
  ################################################################################################
  #dplyr method of rounding only those columns that are numeric (facilitates elfgen message)
  fac_summary <- fac_summary %>% mutate_if(is.numeric, round, digits=2)
  rseg_summary <- rseg_summary %>% mutate_if(is.numeric, round, digits=2)
  
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
      "SELECT a.runid,a.run_date, a.starttime, a.endtime, a.riverseg,' ' AS Rseg_Stats,", rseg.met.list,
      ", a.richness_change_abs, a.richness_change_pct, ' ' AS Facility_Stats,",fac.met.list," 
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
