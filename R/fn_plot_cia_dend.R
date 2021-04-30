# fn_plot_cia_dend.R
# Main Plot for the CIA static and Shiny dashboards.

# fn_plot_cia_dend Function: Takes CIA_data out put and returns plots
#' Dendritic Ploting Function
#' @description Helps locate all upstream segments from the working segment
#' @param riv_seg Downstream river segment that you wish to have no more tributaries below segment - cannot be any more downstream then segment entered into function
#' @param AllSegList Data frame of cumulative impact data
#' @param runid1 Runid that corresponds to first metric in data frame
#' @param runid2 Runid that corresponds to second metric in data frame
#' @param flow_metric Flow metric that corresponds to data in data frame
#' @param cia_data_frame Data frame of cumulative impact data
#' @return A dendritic plot of segments from data frame
#' @import sqldf
#' @import ggplot2
#' @export fn_plot_cia_dend
fn_plot_cia_dend <- function(riv_seg, AllSegList, runid1, runid2, flow_metric, cia_data_frame){
  
  #Declaring initial inputed river segment for graphing dot
  riv_seg_i <- riv_seg
  
  #Calculates Upstream River Segments
  upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
  names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
  
  #While loop that runs the function for every upstream segment
  a <- 1
  p <- ggplot(NULL)
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
      names(river)[names(river) == colnames(river)[1]] <- "riv_seg"
      
      #pulls river data from river segments that match headwater and its downstream segs
      cia_data_loop <- sqldf("SELECT * FROM river join cia_data_frame
                        WHERE riv_seg like riverseg")
      
      #plot graph
      p <- p +
        geom_line(data = cia_data_loop, aes(x = rmile, y = Metric_1, colour = Metric_change, size = metric_pc))
      
    }
    
    a <- a + 1
  }
  
  # Reversing scale for correct river mile orientation
  p <- p + scale_x_reverse()
  
  
  #Creating data frame with just original inputed river segment to graph point
  riv_seg_og <- cia_data_frame[cia_data_frame$riverseg == riv_seg_i,]
  
  p <- p +
    geom_point(data = riv_seg_og, aes(x = rmile, y = Metric_1)) +
    geom_text(data = cia_data_frame, aes(x = rmile, y = Metric_1, label = seglist, vjust = 1.0)) + 
    scale_size_continuous(range = c(0.2, 3), name = "Percent Change") +
    scale_colour_manual(values = c("blue", "grey", "brown"), name = "Percent Change") +
    theme_bw() +
    ggtitle(paste0("Percent Change in ", flow_metric, " Flow between runid", runid1, " and runid", runid2)) +
    xlab('River Mile [Mi]') +
    ylab('Flow [cfs]')
  
  return(p)
}