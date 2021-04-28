#extracts only the upstream functions of intended intended outlet
#' Upstream Segments Function
#' @description Helps locate all upstream segments from the working segment
#' @param riv.seg River segment of interest
#' @param AllSegList A list of all segments
#' @return All upstream segments
#' @import rapportools
#' @export basin_data

fn_extract_basin <- function(cia_data_frame, end_seg){
  #calculating upstream segments
  upstream <- data.frame((fn_ALL.upstream(outlet_point, AllSegList)))
  names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
  
  if(upstream == 'NA'){
    river <- outlet_point
  }else {
    river <- rbind(upstream,outlet_point)
  }
  basin_data <- sqldf("SELECT * FROM river join cia_data_frame
                    WHERE riv_seg like riverseg")
  basin_data$riv_seg <- NULL
  
  return(basin_data)
}
