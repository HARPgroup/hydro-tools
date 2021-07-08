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
  rseg.hydroid = 462757,
  fac.hydroid = 72672,
  runid.list = c('runid_201','runid_401'),
  fac.metric.list = c('wd_mgd','ps_mgd','unmet1_mgd','unmet7_mgd','unmet30_mgd','unmet90_mgd'),
  rseg.metric.list = c('Qout','Qbaseline','remaining_days_p0','remaining_days_p10','remaining_days_p50','l30_Qout',
                        'l90_Qout','consumptive_use_frac','wd_cumulative_mgd','ps_cumulative_mgd'),
  site = "https://deq1.bse.vt.edu/d.dh",
  site_base = "https://deq1.bse.vt.edu"
) {
  ################################################################################################
  # RETRIEVE FAC & RSEG MODEL STATS
  ################################################################################################
  rseg.model <- om_get_model(site, rseg.hydroid)
  rseg.elid <- om_get_prop(site, rseg.model$pid, entity_type = 'dh_properties','om_element_connection')$propvalue
  
  fac_summary <- data.frame()
  rseg_summary <- data.frame()
  #i <- 1
  for (i in 1:length(runid.list)){
    runid.i <- runid.list[i]
    run.i <- sub("runid_", "", runid.i)
    
    # RETRIEVE FAC MODEL STATS 
    fac.metrics.i <- data.frame('model_version' = c('vahydro-1.0'),'runid' = c(runid.i),'runlabel' = fac.metric.list,'metric' = fac.metric.list)
    fac_summary.i <- om_vahydro_metric_grid(metric, 'bundle' = 'facility','ftype' = 'all',fac.metrics.i,base_url = paste(site,"/entity-model-prop-level-export",sep=""))
    fac_summary.i <- sqldf(paste("SELECT '",run.i,"' AS runid, * FROM 'fac_summary.i' WHERE featureid = ",fac.hydroid,sep=""))
    if (nrow(fac_summary.i) > 0) {
      fac_summary <- rbind(fac_summary,fac_summary.i)
    }
    
    # RETRIEVE RSEG MODEL STATS
    rseg.info.i <- fn_get_runfile_info(rseg.elid,run.i,site=site_base)
    rseg.metrics.i <- data.frame('model_version' = c('vahydro-1.0'),'runid' = c(runid.i),'runlabel' = rseg.metric.list,'metric' = rseg.metric.list)
    rseg_summary.i <- om_vahydro_metric_grid(metric, rseg.metrics.i,base_url = paste(site,"/entity-model-prop-level-export",sep=""))
    rseg_summary.i <- sqldf(paste("SELECT * FROM 'rseg_summary.i' WHERE featureid = ",rseg.hydroid,sep=""))
    
    # ADD SCENARIO TEXT DESCRIPTIONS
    ## NEW METHOD (SLOW)
    # scenario <- withCallingHandlers(find_name(run_text(runid.i,site), runid.i))
    # scenario <- scenario$reports$cia$scenario_name$value
    # rseg_summary.i <- cbind(scenario,rseg_summary.i)
    
    ## CONVENTIONAL METHOD (FASTER)
    ds <- RomDataSource$new(site)
    scen_var <- ds$get_vardef('om_scenario')
    scen_config <- om_get_prop(site, scen_var$varid, entity_type = 'dh_variabledefinition',propname = 'variants')
    scen.i <- om_get_prop(site, scen_config$pid, entity_type = 'dh_properties',propname = runid.i)
    reports.i <- om_get_prop(site, scen.i$pid, entity_type = 'dh_properties',propname = 'reports')
    cia.i <- om_get_prop(site, reports.i$pid, entity_type = 'dh_properties',propname = 'cia')
    scenario_name.i <- om_get_prop(site, cia.i$pid, entity_type = 'dh_properties',propname = 'scenario_name')
    scenario <- scenario_name.i$propcode
    rseg_summary.i <- cbind(scenario,rseg_summary.i)
    
    
    # ADD ELFGEN STATS TO TABLE -------------------------------------------------------------------------------------
      runid_i_pid <- om_get_prop(site, rseg.model$pid, entity_type = 'dh_properties',propname = runid.i)$pid
      elfgen_EDAS_huc8_i <- om_get_prop(site, runid_i_pid, entity_type = 'dh_properties',propname = 'elfgen_EDAS_huc8')
    
      if (!is.logical(elfgen_EDAS_huc8_i)) {
        richness_change_abs <- om_get_prop(site, elfgen_EDAS_huc8_i$pid, entity_type = 'dh_properties',propname = 'richness_change_abs')$propvalue
        richness_change_pct <- om_get_prop(site, elfgen_EDAS_huc8_i$pid, entity_type = 'dh_properties',propname = 'richness_change_pct')$propvalue
      } else {
        richness_change_abs <- 'No elfgen Available'
        richness_change_pct <- 'No elfgen Available'
      }
      
      rseg_summary.i <- cbind(rseg_summary.i,richness_change_abs)
      rseg_summary.i <- cbind(rseg_summary.i,richness_change_pct)
    #----------------------------------------------------------------------------------------------------------------
   
    if (nrow(rseg_summary.i) > 0) {
      rseg_summary.i <- cbind("runid" = run.i,"run_date" = rseg.info.i$run_date,"starttime" = str_remove(rseg.info.i$starttime," 00:00:00"),"endtime" = str_remove(rseg.info.i$endtime," 00:00:00"),rseg_summary.i)
      rseg_summary <- rbind(rseg_summary,rseg_summary.i)
    }
  }
  
  ################################################################################################
  # JOIN FAC AND RSEG MODEL STATS INTO SINGLE TABLE
  ################################################################################################
  # ROUND DATA TO 2 PLACES
  #fac_summary[,-(1:6)] <- round(fac_summary[,-(1:6)],2)
  #rseg_summary[,-(1:10)] <- round(rseg_summary[,-(1:10)],2)

  #dplyr method of rounding only those columns that are numeric (facilitates elfgen message)
  fac_summary <- fac_summary %>% mutate_if(is.numeric, round, digits=2)
  rseg_summary <- rseg_summary %>% mutate_if(is.numeric, round, digits=2)
  
  rseg.met.list <- paste(rseg.metric.list, collapse = ",")
  fac.met.list <- paste(fac.metric.list, collapse = ",")
  fac_rseg_stats <- sqldf(
    paste(
      "SELECT a.runid, a.scenario ,a.run_date, a.starttime, a.endtime, a.riverseg,' ' AS Rseg_Stats,", rseg.met.list,
      ",' ' AS Facility_Stats,",fac.met.list,", a.richness_change_abs, a.richness_change_pct 
      FROM rseg_summary AS a     LEFT OUTER JOIN fac_summary AS b     ON a.runid = b.runid")
  )
  
  # fac_rseg_stats <- sqldf(
  #   paste(
  #     "SELECT a.runid ,a.run_date, a.starttime, a.endtime, a.riverseg,' ' AS Rseg_Stats,", rseg.met.list,
  #     ",' ' AS Facility_Stats,",fac.met.list,", a.richness_change_abs, a.richness_change_pct
  #     FROM rseg_summary AS a     LEFT OUTER JOIN fac_summary AS b     ON a.runid = b.runid")
  # )
  
  #TRANSPOSE DATAFRAME, IF DESIRED
  fac_rseg_stats.T <- as.data.frame(t(fac_rseg_stats[,-1]))
  colnames(fac_rseg_stats.T) <- fac_rseg_stats[,1]
  #View(fac_rseg_stats.T)
  #pandoc.table(fac_rseg_stats.T, style = "rmarkdown", split.table = 120)
  
  stats.df <- fac_rseg_stats.T
  return(stats.df)
}
