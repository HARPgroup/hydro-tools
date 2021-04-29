#extracts only the upstream functions of intended intended outlet
#' Extracting Basin Function
#' @description Trims data frame to include only segments upstream of end segment
#' @param cia_data_frame Data frame of cumulative impact data
#' @param end_seg Desired end river segment - river segment that is most downstream
#' @return Trimmed cumulative impact data frame
#' @import sqldf
#' @export basin_data

fn_extract_basin <- function(cia_data_frame, end_seg){
  #calculating upstream segments
  upstream <- data.frame((fn_ALL.upstream(end_seg, AllSegList)))
  names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
  
  if(upstream == 'NA'){
    river <- end_seg
  }else {
    river <- rbind(upstream,end_seg)
  }
  basin_data <- sqldf("SELECT * FROM river join cia_data_frame
                    WHERE riv_seg like riverseg")
  basin_data$riv_seg <- NULL
  
  return(basin_data)
}
