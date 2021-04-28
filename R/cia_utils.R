# cia_utils.R
# Supporting functions for the CIA static and Shiny dashboards.


# CIA_data Function: Grabs data from vahydro and returns data frame 
#data frame contains flow and percent change data for 2 runids of all upstream and downstream river segments
CIA_data <- function(riv_seg, runid1, runid2, flow_metric, AllSegList){
  downstream <- data.frame(fn_ALL.downstream(riv_seg, AllSegList))
  names(downstream)[names(downstream) == colnames(downstream)[1]] <- "riv_seg"
  riv_seg <- as.data.frame(riv_seg)
  
  # Calculates Upstream River Segments
  upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
  names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
  
  if(upstream == 'NA'){
    river <- rbind(riv_seg, downstream)
  }else {
    river <- rbind(upstream,riv_seg, downstream)
  }
  
  #setting up dataframe for om_vahydro_metric_grid
  df <- data.frame(
    'model_version' = c('vahydro-1.0'),
    'runid' = c(paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), paste0('runid_', runid1), paste0('runid_', runid2), '0.%20River%20Channel', 'local_channel'),
    'runlabel' = c('Qbaseline_1', 'Qbaseline_2', 'L90_1', 'L90_2', 'WD_1', 'WD_2', 'PS_1', 'PS_2', 'PSNX_1', 'PSNX_2', 'Qout_1', 'Qout_2', 'Metric_1', 'Metric_2', 'length', 'sub_length'),
    'metric' = c('Qbaseline', 'Qbaseline','l90_Qout','l90_Qout','wd_cumulative_mgd','wd_cumulative_mgd','ps_cumulative_mgd','ps_cumulative_mgd','ps_nextdown_mgd','ps_nextdown_mgd', 'Qout', 'Qout', flow_metric, flow_metric, 'length', 'length')
  )
  
  #importing dataframe of river segment metrics
  wshed_data <- om_vahydro_metric_grid(metric, df)
  
  #triming dataframe to just river segments on river of interest
  cia_data <- sqldf("SELECT * FROM river join wshed_data
                    WHERE riverseg like riv_seg")
  
  #pull the values that exist from length and subcomp_da (One of the two will be NA for each segment)
  da_data <- sqldf("SELECT pid, length, sub_length,
                    CASE
                    WHEN length is null then sub_length
                    ELSE length
                    END as da
                    from cia_data")
  
  #selecting the values that exist and adding them to cia_data (convert to miles)
  cia_data$length <- da_data$da/5280
  
  #deleting sub_length column bc all values are not in length
  cia_data$sub_length <- NULL
  #deleting riv_seg column bc it is a duplicate
  cia_data$riv_seg <- NULL
  
  #creating percent change columns
  #calculating Percent change values for mean annual flow and inputed metric flow
  cia_data$Qout_pc <- ((abs(cia_data$Qout_2 - cia_data$Qout_1))/cia_data$Qout_1)*100
  cia_data$metric_pc <- ((abs(cia_data$Metric_2 - cia_data$Metric_1))/cia_data$Metric_1)*100
  
  #creating column describing - vs + change
  if(cia_data$Qout_1 > cia_data$Qout_2){
    cia_data$Qout_change <- '-'
  }else{
    cia_data$Qout_change <- '+'
  }
  
  if(cia_data$Metric_1 > cia_data$Metric_2){
    cia_data$Metric_change <- '-'
  }else{
    cia_data$Metric_change <- '+'
  }
  
  return(cia_data)
}

