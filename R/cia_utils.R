# cia_utils.R
# Supporting functions for the CIA static and Shiny dashboards.


# CIA_data Function: Grabs data from vahydro and returns data frame 
#data frame contains flow and percent change data for 2 runids of all upstream and downstream river segments
#' Cumulative Impact Analysis Data Function
#' @description Extracts metric data from all upstream river segments and downstream segments from inputed segment
#' @param riv_seg Desired river segment
#' @param runid1 First runid of interest
#' @param runid2 Second runid of interest
#' @param flow_metric Desired flow metric
#' @param AllSegList A list of all river segments
#' @return data frame of river segments downstream and upstream of inputed segment
#' @import sqldf
#' @export CIA_data
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
  cia_data <- sqldf("select *, CASE
                     WHEN Metric_1 > Metric_2 
                     THEN -1 
                     WHEN Metric_2 < Metric_1
                     THEN 1
                     ELSE 1 
                     END as Metric_change 
                     FROM cia_data")
  
  return(cia_data)
}


#' fn_river_network()
#' @description Calculates ands adds river mile column to data frame of river segments
#' @param riv_seg Desired river segment
#' @param AllSegList List of all vahydro river segments
#' @param cia_data_frame CIA data frame with specific columns
#' @return data frame of river segments, and associated river miles
#' @import sqldf
#' @export fn_river_network
fn_river_network <- function(riv_seg, AllSegList, cia_data_frame){
  
  #Calculates Upstream River Segments
  upstream <- data.frame((fn_ALL.upstream(riv_seg, AllSegList)))
  names(upstream)[names(upstream) == colnames(upstream)[1]] <- "riv_seg"
  
  #While loop that runs the function for every upstream segment
  a <- 1
  cia_data <- data.frame()
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
      
      #Adding length segments together to form river mile (distance from headwater) column
      i <- 1
      while (i <= nrow(cia_data_loop)) {
        
        river_length <- c()
        
        #Loop creates vector of current segment and upstream segment lengths
        for (n in 1:i) {
          n_length <- as.numeric(cia_data_loop$length[n])
          river_length <- c(river_length, n_length)
        }
        #Makes length column to total length to segment from start of river
        cia_data_loop$mile[i] <- sum(river_length)
        
        i <- i + 1
      }
      
      #Creating a river mile column
      for (i in 1:(length(cia_data_loop$mile))){
        if(i == 1){
          cia_data_loop$rmile[i] <- cia_data_loop$mile[length(cia_data_loop$mile)]
        }
        else{
          cia_data_loop$rmile[i] <- cia_data_loop$mile[length(cia_data_loop$mile)] - cia_data_loop$mile[i-1]
        }
      }
      
      #combine current data frame with new data frame
      cia_data <- rbind(cia_data_loop, cia_data)
      
    }
    
    a <- a + 1
  }
  #Creating data frame with segment ID numbers
  cia_data <- cia_data[!duplicated(cia_data$riv_seg),]
  #Makes numbers ordered by river mile (is this whats best? should we make it based on tributary?)
  cia_data <- cia_data[order(cia_data$rmile, decreasing = TRUE),]
  cia_data$seglist <- 1:nrow(cia_data)
  #Triming to only solumns needed in fn_plot_cia_dend
  cia_data <- sqldf("SELECT seglist, riverseg, propname, rmile, Metric_1,
                    Metric_2, metric_pc, Metric_change
                    FROM cia_data")
  return(cia_data)
}


#extracts only the upstream functions of intended intended outlet
#' Extracting Basin Function
#' @description Trims data frame to include only segments upstream of end segment
#' @param cia_data_frame Data frame of cumulative impact data
#' @param end_seg Desired end river segment - river segment that is most downstream
#' @return Trimmed cumulative impact data frame
#' @import sqldf
#' @export fn_extract_basin
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


#############################################################################
## Example Code to get AllSegList
# Download and Import csv with all river segments (copy commented lines below)
# localpath <- tempdir()
# filename <- paste("vahydro_riversegs_export.csv",sep="")
# destfile <- paste(localpath,filename,sep="\\")
# download.file(paste(site,"/vahydro_riversegs_export",sep=""), destfile = destfile, method = "libcurl")
# RSeg.csv <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
# AllSegList <- substring(RSeg.csv$hydrocode, 17)
############################################################################

#' Find Upstream Segments
#' @description Trims data frame to include only segments upstream of end segment
#' @param riv.seg string ID of segment to fine
#' @param AllSegList AllSegList data frame of all segments
#' @return string next upstream segment ID
#' @import stringr rapportools
#' @export fn_upstream
#fn_upstream() copy from cbp6_functions
fn_upstream <- function(riv.seg, AllSegList) {
  # Create dataframe for upstream and downstream segments based on code in string
  ModelSegments <- data.frame(matrix(nrow = length(AllSegList), ncol = 6))
  colnames(ModelSegments)<- c('RiverSeg', 'Middle', 'Last', 'AdditionalName', 'Downstream', 'Upstream')
  ModelSegments$RiverSeg <- AllSegList
  
  # Pull out 4 digit codes in middle and end for upstream/downstream segments
  i <- 1
  for (i in 1:nrow(ModelSegments)){
    
    ModelSegments[i,2]<- str_sub(ModelSegments[i,1], start=5L, end=8L)
    ModelSegments[i,3]<- str_sub(ModelSegments[i,1], start=10L, end=13L)
    ModelSegments[i,4]<- str_sub(ModelSegments[i,1], start=15L, end=-1L)
    i <- i + 1
  }
  
  # Determine Downstream Segment ----------
  j <- 1
  for (j in 1:nrow(ModelSegments)){
    if (ModelSegments[j,4] != ""){
      Downstream <- which((ModelSegments$Middle==ModelSegments$Middle[j]) & (ModelSegments$Last==ModelSegments$Last[j]) & (ModelSegments$AdditionalName==""))
      if (length(Downstream)==0){
        ModelSegments[j,5]  <- 'NA'
      }else if (length(Downstream)==1){
        ModelSegments[j,5] <- as.character(ModelSegments[Downstream,1])
      }
    }else if (ModelSegments[j,4]==""){
      Downstream <- which((ModelSegments$Middle==ModelSegments$Last[j]) & (ModelSegments$AdditionalName==""))
      if (length(Downstream)==0){
        ModelSegments[j,5]  <- 'NA'
      }else if (length(Downstream)==1){
        ModelSegments[j,5] <- as.character(ModelSegments[Downstream,1])
      }else if (length(Downstream)>1){
        ModelSegments[j,5] <- 'NA'
      }
    }
    j<-j+1
  }
  # Determine Upstream Segment ----------
  k<-1
  for (k in 1:nrow(ModelSegments)){
    Upstream <- which(as.character(ModelSegments$Downstream)==as.character(ModelSegments$RiverSeg[k]))
    NumUp <- ModelSegments$RiverSeg[Upstream]
    ModelSegments[k,6]<- paste(NumUp, collapse = '+')
    if (is.empty(ModelSegments[k,6])==TRUE){
      ModelSegments[k,6]<- 'NA'
    } 
    k<-k+1
  }
  SegUpstream <- as.numeric(which(as.character(ModelSegments$RiverSeg)==as.character(riv.seg)))
  SegUpstream <- ModelSegments$Upstream[SegUpstream]
  SegUpstream <- strsplit(as.character(SegUpstream), "\\+")
  SegUpstream <- try(SegUpstream[[1]], silent = TRUE)
  if (class(SegUpstream)=='try-error') {
    SegUpstream <- NA
  }
  return(SegUpstream)
}


#fn_ALL.upstream() copy from cbp6_functions
fn_ALL.upstream <- function(riv.seg, AllSegList) {
  UpstreamSeg <- fn_upstream(riv.seg, AllSegList)
  AllUpstream <- character(0)
  BranchedSegs <- character(0)
  while (is.na(UpstreamSeg[1])==FALSE || is.empty(BranchedSegs) == FALSE) {
    while (is.na(UpstreamSeg[1])==FALSE) {
      num.segs <- as.numeric(length(UpstreamSeg))
      if (num.segs > 1) {
        BranchedSegs[(length(BranchedSegs)+1):(length(BranchedSegs)+num.segs-1)] <- UpstreamSeg[2:num.segs]
        UpstreamSeg <- UpstreamSeg[1]
      }
      AllUpstream[length(AllUpstream)+1] <- UpstreamSeg
      UpstreamSeg <- fn_upstream(UpstreamSeg, AllSegList)
    }
    num.branched <- as.numeric(length(BranchedSegs))
    UpstreamSeg <- BranchedSegs[1]
    BranchedSegs <- BranchedSegs[-1]
  }
  AllUpstream <- AllUpstream[which(AllUpstream != 'NA')]
  if (is.empty(AllUpstream[1])==TRUE) {
    AllUpstream <- 'NA'
  }
  return(AllUpstream)
}


#' Find Downstream Segment
#' @description Trims data frame to include only segments upstream of end segment
#' @param riv.seg string ID of segment to fine
#' @param AllSegList AllSegList data frame of all segments
#' @return string next upstream segment ID
#' @import stringr rapportools
#' @export fn_downstream
#fn_downstream() copy from cbp6_functions
fn_downstream <- function(riv.seg, AllSegList) {
  # Create dataframe for upstream and downstream segments based on code in string
  ModelSegments <- data.frame(matrix(nrow = length(AllSegList), ncol = 6))
  colnames(ModelSegments)<- c('RiverSeg', 'Middle', 'Last', 'AdditionalName', 'Downstream', 'Upstream')
  ModelSegments$RiverSeg <- AllSegList
  
  # Pull out 4 digit codes in middle and end for upstream/downstream segments
  i <- 1
  for (i in 1:nrow(ModelSegments)){
    
    ModelSegments[i,2]<- str_sub(ModelSegments[i,1], start=5L, end=8L)
    ModelSegments[i,3]<- str_sub(ModelSegments[i,1], start=10L, end=13L)
    ModelSegments[i,4]<- str_sub(ModelSegments[i,1], start=15L, end=-1L)
    i <- i + 1
  }
  
  # Determine Downstream Segment ----------
  j <- 1
  for (j in 1:nrow(ModelSegments)){
    if (ModelSegments[j,4] != ""){
      Downstream <- which((ModelSegments$Middle==ModelSegments$Middle[j]) & (ModelSegments$Last==ModelSegments$Last[j]) & (ModelSegments$AdditionalName==""))
      if (length(Downstream)==0){
        ModelSegments[j,5]  <- 'NA'
      }else if (length(Downstream)==1){
        ModelSegments[j,5] <- as.character(ModelSegments[Downstream,1])
      }
    }else if (ModelSegments[j,4]==""){
      Downstream <- which((ModelSegments$Middle==ModelSegments$Last[j]) & (ModelSegments$AdditionalName==""))
      if (length(Downstream)==0){
        ModelSegments[j,5]  <- 'NA'
      }else if (length(Downstream)==1){
        ModelSegments[j,5] <- as.character(ModelSegments[Downstream,1])
      }else if (length(Downstream)>1){
        ModelSegments[j,5] <- 'NA'
      }
    }
    j<-j+1
  }
  # Determine Upstream Segment ----------
  k<-1
  for (k in 1:nrow(ModelSegments)){
    Upstream <- which(as.character(ModelSegments$Downstream)==as.character(ModelSegments$RiverSeg[k]))
    NumUp <- ModelSegments$RiverSeg[Upstream]
    ModelSegments[k,6]<- paste(NumUp, collapse = '+')
    if (is.empty(ModelSegments[k,6])==TRUE){
      ModelSegments[k,6]<- 'NA'
    } 
    k<-k+1
  }
  SegDownstream <- as.numeric(which(as.character(ModelSegments$RiverSeg)==as.character(riv.seg)))
  SegDownstream <- ModelSegments$Downstream[SegDownstream]
  SegDownstream <- strsplit(as.character(SegDownstream), "\\+")
  SegDownstream <- try(SegDownstream[[1]], silent = TRUE)
  if (class(SegDownstream)=='try-error') {
    SegDownstream <- NA
  }
  return(SegDownstream)
}


#fn_ALL.downstream() copy form cbp6_functions
fn_ALL.downstream <- function(riv.seg, AllSegList) {
  downstreamSeg <- fn_downstream(riv.seg, AllSegList)
  Alldownstream <- character(0)
  BranchedSegs <- character(0)
  while (is.na(downstreamSeg[1])==FALSE || is.empty(BranchedSegs) == FALSE) {
    while (is.na(downstreamSeg[1])==FALSE) {
      num.segs <- as.numeric(length(downstreamSeg))
      if (num.segs > 1) {
        BranchedSegs[(length(BranchedSegs)+1):(length(BranchedSegs)+num.segs-1)] <- downstreamSeg[2:num.segs]
        downstreamSeg <- downstreamSeg[1]
      }
      Alldownstream[length(Alldownstream)+1] <- downstreamSeg
      downstreamSeg <- fn_downstream(downstreamSeg, AllSegList)
    }
    num.branched <- as.numeric(length(BranchedSegs))
    downstreamSeg <- BranchedSegs[1]
    BranchedSegs <- BranchedSegs[-1]
  }
  Alldownstream <- Alldownstream[which(Alldownstream != 'NA')]
  if (is.empty(Alldownstream[1])==TRUE) {
    Alldownstream <- 'NA'
  }
  return(Alldownstream)
}


# diffs 2 timeseries for he same modle different scenario
# use like:
#  om_ts_diff(datsr400, datsr600, "Qout", "Qout", "<>")
#  om_ts_diff(datjrva400, datjrva600, "Qout", "Qout", "> 10 + ")
#  om_ts_diff(datjrva600, datjrva400, "wd_cumulative_mgd", "wd_cumulative_mgd", "> 1.1 * ")
#  use "all" to just join on date and return all records
#  om_ts_diff(datjrh400, datjrh600, "Qout", "Qout", "all")
om_ts_diff <- function(df1, df2, col1, col2, op = "<>") {
  # use "all" for op if just want the df
  df1 <- as.data.frame(df1)
  df2 <- as.data.frame(df2)
  dsql <- paste0(
    "select a.year,a.month,a.day, ",
    "a.", col1, " as v1, b.", col2, " as v2",
    " from df1 as a " ,
    "left outer join df2 as b ",
    " on ( ",
    "a.year = b.year
   and a.month = b.month 
   and a.day = b.day
  ) "
  )
  if (!(op == "all")) {
    dsql <- paste0(
      dsql, " where a.",col1," ", op, " b.", col2
    )
  }
  message(dsql)
  rets <- sqldf(
    dsql
  )
  return(rets)
}
