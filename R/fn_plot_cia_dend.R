# fn_plot_cia_dend.R
# Main Plot for the CIA static and Shiny dashboards.
# fn_plot_cia_dend Function: Takes CIA_data out put and returns plots
#As of latest update to code 
#we did not figure out a way to separte this into multiple functions because of the way the graphing is done
fn_plot_cia_dend <- function(cia_data_frame){
  
  # Calculates Upstream River Segments
  upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
  names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
  
  # While loop that runs the function for every upstream segment
  a <- 1
  cia_data <- data.frame()
  p <- ggplot(NULL)
  p1 <- ggplot(NULL)
  while(a <= nrow(upstream)){
    if(upstream == 'NA'){
      riv_seg <- riv_seg
    }else{
      riv_seg <- upstream[a,]
    }
    #only runs code if river segment is headwater
    if(fn_ALL.upstream(riv_seg,AllSegList) == 'NA'){
      #determines all downstream segments
      downstream <- data.frame(fn_ALL.downstream(riv_seg, AllSegList))
      names(downstream)[names(downstream) == colnames(downstream)[1]] <- "riv_seg"
      riv_seg <- as.data.frame(riv_seg)
      #creates dataframe of river segment and all downstream segments
      river <- rbind(riv_seg, downstream)
      names(river)[names(river) == colnames(river)[1]] <- "rivseg"
      
      #pulls river data from river segments that match headwater and its downstream segs
      cia_data_loop <- sqldf("SELECT * FROM river join cia_data_frame
                        WHERE rivseg like riverseg")
      
      #Adding length segments together to form river mile (distance from headwater) column
      i <- 1
      while (i <= nrow(cia_data_loop)) {
        
        river_length <- c()
        
        # Loop creates vector of current segment and upstream segment lengths
        for (n in 1:i) {
          n_length <- as.numeric(cia_data_loop$length[n])
          river_length <- c(river_length, n_length)
        }
        # Makes length column to total length to segment from start of river
        cia_data_loop$mile[i] <- sum(river_length)
        
        i <- i + 1
      }
      
      # Creating a river mile column
      for (i in 1:(length(cia_data_loop$mile))){
        if(i == 1){
          cia_data_loop$rmile[i] <- cia_data_loop$mile[length(cia_data_loop$mile)]
        }
        else{
          cia_data_loop$rmile[i] <- cia_data_loop$mile[length(cia_data_loop$mile)] - cia_data_loop$mile[i-1]
        }
      }
      
      # # Calculating Percent change values for mean annual flow and inputed metric flow
      # cia_data_loop$Qout_pc <- ((cia_data_loop$Qout_2 - cia_data_loop$Qout_1)/cia_data_loop$Qout_1)*100
      # cia_data_loop$metric_pc <- ((cia_data_loop$Metric_2 - cia_data_loop$Metric_1)/cia_data_loop$Metric_1)*100
      # 
      # # Must make seg list numbered for x axis on graphs with bars, could be used in table companion
      # cia_data_loop$seglist = as.numeric(c(1:nrow(cia_data_loop)))
      # 
      # # Now lets graph using mean baseflow
      # # Pre setting y axes max
      # if (max(cia_data_loop$Qout_1 >= cia_data_loop$Qout_2)) {
      #   y_prim <- c(0,max(cia_data_loop$Qout_1) + 100)
      # } else {
      #   y_prim <- c(0, max(cia_data_loop$Qout_2) + 100)
      # }
      # y_sec <-  c(min(cia_data_loop$Qout_pc) - 2, max(cia_data_loop$Qout_pc) + 2)
      # 
      # coeff <- max(y_sec) / max(y_prim)
      # cia_data_loop$Qout_pc_graph <- cia_data_loop$Qout_pc/coeff
      # 
      # # Pre setting y axes max (Do Not Need If No More ggplot)
      # if (max(cia_data_loop$Metric_1 >= cia_data_loop$Metric_2)) {
      #   y_prim2 <- c(0,max(cia_data_loop$Metric_1) + 100)
      # } else {
      #   y_prim2 <- c(0, max(cia_data_loop$Metric_2) + 100)
      # }
      # y_sec2 <-  c(min(cia_data_loop$metric_pc) - 2, max(cia_data_loop$metric_pc) + 2)
      # 
      # coeff2 <- max(y_sec2) / max(y_prim2)
      # cia_data_loop$metric_pc_graph <- cia_data_loop$metric_pc/coeff2
      # 
      #combine current data frame with new data frame
      cia_data <- rbind(cia_data_loop, cia_data)
      
      #plot graph
      p <- p +
        geom_line(data = cia_data_loop, aes(x = rmile, y = Qout_1, colour = Qout_change, size = Qout_pc))
      #geom_line(data = cia_data_loop, aes(x = rmile, y = Qout_2)) + 
      #scale_color_gradient2(low = "Brown", high = "Blue", mid = "Chocolate", midpoint = 0, name = "Percent Change") +
      #labs(size = 'Percent Change') +
      #geom_vline(data = cia_data_loop, (aes(xintercept = rmile)),linetype=8, colour = "grey") +
      #geom_text(data = cia_data_loop, aes(x = rmile, label = paste(propname),
      #                                    y=(max(Qout_1)/2)), colour="grey", angle=90,
      #          vjust=-0.4, size=3)
      
      p1 <- p1 +
        geom_line(data = cia_data_loop, aes(x = rmile, y = Metric_1, colour = Metric_change, size = metric_pc))
      #geom_line(data = cia_data_loop, aes(x = rmile, y = Qout_2)) + 
      #scale_color_gradient2(low = "Red", high = "Blue", mid = "Green", midpoint = 0, name = "Percent Change") +
      
    }
    
    a <- a + 1
  }
  
  # Reversing scale for correct river mile orientation
  p <- p + scale_x_reverse()
  p1 <- p1 + scale_x_reverse()
  
  # Creating data frame with segment ID numbers
  cia_data <- cia_data[!duplicated(cia_data$riv_seg),]
  # Makes numbers ordered by river mile (is this whats best? should we make it based on tributary?)
  cia_data <- cia_data[order(cia_data$rmile, decreasing = TRUE),]
  cia_data$seglist <- 1:nrow(cia_data)
  
  p <- p +
    geom_point(data = cia_data, aes(x = rmile, y = Qout_1)) +
    geom_text(data = cia_data, aes(x = rmile, y = Qout_1, label = seglist, vjust = 1.0)) + 
    scale_size_continuous(range = c(0.2, 3), name = "Percent Change") +
    scale_colour_manual(values = c("blue", "brown"), name = "Percent Change") +
    theme_bw() +
    ggtitle(paste0("Percent Change in Mean Annual Flow between runid", runid1, " and runid", runid2)) +
    xlab('River Mile [Mi]') +
    ylab('Flow [cfs]')
  
  p1 <- p1 +
    geom_point(data = cia_data, aes(x = rmile, y = Metric_1)) +
    geom_text(data = cia_data, aes(x = rmile, y = Metric_1, label = seglist, vjust = 1.0)) + 
    scale_size_continuous(range = c(0.2, 3), name = "Percent Change") +
    scale_colour_manual(values = c("blue", "brown"), name = "Percent Change") +
    theme_bw() +
    ggtitle(paste0("Percent Change in ", flow_metric, " Flow between runid", runid1, " and runid", runid2)) +
    xlab('River Mile [Mi]') +
    ylab('Flow [cfs]')
  
  dend_plot <- list(p, p1)
  
  return(dend_plot)
}
