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
  
  # #Adding length segments together to form river mile (distance from headwater) column
  # i <- 1
  # while (i <= nrow(cia_data)) {
  #   
  #   river_length <- c()
  #   
  #   # Loop creates vector of current segment and upstream segment lengths
  #   for (n in 1:i) {
  #     n_length <- as.numeric(cia_data$length[n])
  #     river_length <- c(river_length, n_length)
  #   }
  #   # Makes length column to total length to segment from start of river
  #   cia_data$mile[i] <- sum(river_length)
  #   
  #   i <- i + 1
  # }
  # 
  # # Creating a river mile column
  # for (i in 1:(length(cia_data$mile))){
  #   if(i == 1){
  #     cia_data$rmile[i] <- cia_data$mile[length(cia_data$mile)]
  #   }
  #   else{
  #     cia_data$rmile[i] <- cia_data$mile[length(cia_data$mile)] - cia_data$mile[i-1]
  #   }
  # }
  # 
  # # Calculating Percent change values for mean annual flow and inputed metric flow
  # cia_data$Qout_pc <- ((cia_data$Qout_2 - cia_data$Qout_1)/cia_data$Qout_1)*100
  # cia_data$metric_pc <- ((cia_data$Metric_2 - cia_data$Metric_1)/cia_data$Metric_1)*100
  # 
  # # Must make seg list numbered for x axis on graphs with bars, could be used in table companion
  # cia_data$seglist = as.numeric(c(1:nrow(cia_data)))
  # 
  # # Now lets graph using mean baseflow
  # # Pre setting y axes max
  # if (max(cia_data$Qout_1 >= cia_data$Qout_2)) {
  #   y_prim <- c(0,max(cia_data$Qout_1) + 100)
  # } else {
  #   y_prim <- c(0, max(cia_data$Qout_2) + 100)
  # }
  # y_sec <-  c(min(cia_data$Qout_pc) - 2, max(cia_data$Qout_pc) + 2)
  # 
  # coeff <- max(y_sec) / max(y_prim)
  # cia_data$Qout_pc_graph <- cia_data$Qout_pc/coeff
  # 
  # # Pre setting y axes max (Do Not Need If No More ggplot)
  # if (max(cia_data$Metric_1 >= cia_data$Metric_2)) {
  #   y_prim2 <- c(0,max(cia_data$Metric_1) + 100)
  # } else {
  #   y_prim2 <- c(0, max(cia_data$Metric_2) + 100)
  # }
  # y_sec2 <-  c(min(cia_data$metric_pc) - 2, max(cia_data$metric_pc) + 2)
  # 
  # coeff2 <- max(y_sec2) / max(y_prim2)
  # cia_data$metric_pc_graph <- cia_data$metric_pc/coeff2
  
  
  
  #return(list(cia_data,
  #           y_prim,
  #          y_prim2,
  #         coeff,
  #        coeff2))
  
  return(cia_data)
}
