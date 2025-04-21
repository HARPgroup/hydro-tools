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
    if(is.null(upstream)){
      riv_seg <- riv_seg
    }else{
      riv_seg <- upstream[a,]
    }
    #only runs code if river segment is headwater
    if(is.null(fn_ALL.upstream(riv_seg,AllSegList))){
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
#' @description Trims data frame to include only segments upstream of end
#'   segment. It is inclusive of the end segment and will thus include the
#'   outlet and all upstream segments
#' @param cia_data_frame Data frame of cumulative impact data. This must contain
#'   a column with the name "riverseg" that contains all river segments of
#'   interest. This function can ONLY find upstream segments if they are present
#'   in this riverseg column!
#' @param end_seg Desired end river segment - river segment that is most downstream
#' @return Trimmed cumulative impact data frame containing the outlet and all upstream segments
#' @export fn_extract_basin
fn_extract_basin <- function(cia_data_frame, end_seg){
  #Get all segments upstream of end_seg. Note that fn_ALL.upstream() may
  #overwrite some data if cia_data_frame has columns used in fn_ALL.upstream()
  upstream <- fn_ALL.upstream(riv.seg = end_seg, 
                              AllSegList = cia_data_frame[!is.na(cia_data_frame$riverseg),],
                              getAllSegmentsOnly = TRUE)
  
  #upstream will feture a list for each segment in end_seg. Unlist for
  #convenience of use. This will combine all upstream segments of all segments
  #in end_seg into a single character vector
  upstream <- unlist(upstream)
  
  #Get all data from the user input data frame based on the upstream segments
  basin_data <- cia_data_frame[cia_data_frame$riverseg %in% upstream,]
  
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
#Newer, auth method:
# ds <- RomDataSource$new(site, rest_uname)
# ds$get_token(rest_pw)
# rsegs <- ds$auth_read(uri = paste0(site,"/vahydro_riversegs_export"))
# AllSegList <- data.frame(riverseg = gsub("vahydrosw_wshed_","",rsegs),
#                     blankColumn = NA)

############################################################################
#' Find Upstream Segments: A basic function that finds the segments immediately
#' upstream of a given segment(s). riv.seg may be a character vector of segments.
#' @description Returns a vector of the segments immediately upstream of riv.seg
#'   based on the list in AllSegList
#' @param riv.seg string ID of segment from which to find upstream segments
#' @param AllSegList AllSegList vector of all model segments. These should be
#'   river segments. This function can only find segments that are in this vector
#'   so the user should use a comprehensive list of segments as demonstrated in
#'   the hydrotools package in cia_utils.R
#' @param allSegToSeg Primarily for use when fn_upstream is called repeatedly.
#'   By providing the vector of segments that each segment in AllSegList flow
#'   into, we can improve the performance of fn_upstream
#' @param allSegThisSeg Primarily for use when fn_upstream is called repeatedly.
#'   By providing the vector of segments IDs for each segment in AllSegList,
#'   we can improve the performance of fn_upstream
#' @param allSegOldshed Primarily for use when fn_upstream is called repeatedly.
#'   By providing the vector of segments in AllSegList flow that have the old
#'   watershed notation (e.g. JL8_2090_8021_Big_Cerry), we can improve the
#'   performance of fn_upstream
#' @return string next upstream segment ID
#' @export fn_upstream
fn_upstream <- function(riv.seg,AllSegList,
                        allSegToSeg = NULL,allSegThisSeg = NULL,
                        allSegOldshed = NULL){
  #Default output is NULL
  out <- NULL
  #First, unlist riv.seg to make it easier to work with
  thisSeg <- unlist(riv.seg)
  
  #If a segment is an old sub watershed, remove from the analysis as we
  #should not guess which segments are upstream of it
  thisSeg <- thisSeg[grepl(".+_[0-9]{4}_[0-9]{4}$",
                           thisSeg)]
  
  #Get the individual segment IDs of each segment in riv.seg (i.e. excluding
  #the segment it flows to)
  thisSeg <- gsub(".+_([0-9]{4})_([0-9]{4}).*","\\1",thisSeg)
  
  if(any(mapply(is.null,list(allSegToSeg,allSegThisSeg,allSegOldshed)))){
    #Find the individual id of the river segment each segment flows to
    allSegToSeg <- gsub(".+_[0-9]{4}_([0-9]{4}).*","\\1",
                  AllSegList)
    
    #Find the individual id of each river segment
    allSegThisSeg <- gsub(".+_([0-9]{4})_[0-9]{4}.*","\\1",
                       AllSegList)
    
    #Identify subwatersheds that follow the old notation of
    #TU3_8880_9230_sf_big_cherry (which is a subwatershed of TU3_8880_9230 and
    #must be captured as upstream of TU3_8880_9230)
    allSegOldshed <- grepl(".+_[0-9]{4}_[0-9]{4}_+.*",
                        AllSegList)
  }
  
  #If the end segment is defined (e.g. not NA and has length greater than
  #zero), then get all segments upstream of all segments in riv.seg. This does
  #NOT apply to old subwatersheds, which should not return upstream segs
  if(all(!is.na(thisSeg)) && length(thisSeg) > 0){
    #Combine the segments of thisSeg (which come from riv.seg) into a single
    #or statement in regex form
    thisSeg <- paste0("(",unlist(thisSeg),")",collapse="|")
    #Return all river segments that have a "toSeg" column matching the ID in
    #thisSeg. In other words, find all segments that flow into the segments in
    #riv.seg.
    upstreamSegs <- AllSegList[grepl(thisSeg,allSegToSeg)
                                        & !allSegOldshed]
    
    #Also capture any upstream watersheds that use the previous notation. For
    #instance, the segment TU3_8880_9230 has several watersheds that flow into
    #it. These include TU3_8881_8880 - TU3_8884_8880 as well as
    #TU3_8880_9230_sf_big_cherry and TU3_8880_9230_sf_below_big_cherry. These
    #latter two segments need to be captured
    oldSubsheds <- AllSegList[grepl(thisSeg,allSegThisSeg) &
                                         allSegOldshed]
    
    out <- c(oldSubsheds,upstreamSegs)
  }
  #If no segments match a valid riv.seg input, there are no upstream segments.
  #Return NULL instead of the empty character vector that is otherwise
  #returned by the grepl call above. This is done for consistency of output so
  #that any segment with no upstream segments return NULL.
  if(length(out) == 0){
    out <- NULL
  }
  #Return out from this function
  return(out)
}


#' Find Upstream Segments: A basic function that finds the segments immediately
#' upstream of a given segment(s). riv.seg may be a character vector of segments.
#' @description Returns a vector of the segments immediately upstream of riv.seg
#'   based on the list in AllSegList
#' @param AllSegList Data frame that must contain riverseg and all river
#segments in state. All columns are maintained in final This function can only
#find segments that are in this list so the user should use a comprehensive
#list of segments as demonstrated in the hydrotools package in cia_utils.R
#' @param riv.seg If user wishes to only return certain segments, they can
#specify these in riv.seg to get a data frame showing that segment, other data
#in AllSegList, and all upstream segments. May be left as NULL to instead find
#upstream segments of all unique segments in AllSegList
#' @param getAllSegmentsOnly If the user only wishes to get the ids of upstream
#segments and the outlet i.e. this is inclusive of riv.seg. This may only be
#used if riv.seg is specified. Defaults to TRUE.
#' @return string next upstream segment ID
#' @export fn_ALL.upstream
fn_ALL.upstream <- function(
    AllSegList,
    riv.seg = NULL,
    getAllSegmentsOnly = TRUE
){
  #AllSegList should be a data frame and must contain a river segment column:
  if(class(AllSegList) != "data.frame" || 
     length(AllSegList$riverseg) == 0){
    stop("The input DATA.FRAME for AllSegList must contain a column
    with the name of \"riverseg\". This column MUST contain all river segments
    in the area of interest (if unsure, use all river segments in the state).
    This function can only find upstream segments that are contain in the
    riverseg column!")
  }
  
  #Find the individual id of the river segment each segment flows to
  allSegToSeg <- gsub(".+_[0-9]{4}_([0-9]{4}).*","\\1",
                      AllSegList$riverseg)
  
  #Find the individual id of each river segment
  allSegThisSeg <- gsub(".+_([0-9]{4})_[0-9]{4}.*","\\1",
                        AllSegList$riverseg)
  
  #Identify subwatersheds that follow the old notation of
  #TU3_8880_9230_sf_big_cherry (which is a subwatershed of TU3_8880_9230 and
  #must be captured as upstream of TU3_8880_9230)
  allSegOldshed <- grepl(".+_[0-9]{4}_[0-9]{4}_+.*",
                         AllSegList$riverseg)
  
  #Create a copy of AllSegList to alter and return to user
  segDataFrame <- AllSegList
  
  #If the user has entered any values for riv.seg, set segDataFrame to only
  #contain those segments. Maintain AllSegList for reference to use for
  #fn_upstream()
  if(!is.null(riv.seg) &&
     length(riv.seg) > 0 && 
     class(riv.seg) == 'character'
  ){
    #Get only river segments specified by user
    segDataFrame <- segDataFrame[segDataFrame$riverseg %in% riv.seg,]
  }
  
  #Add the next upstream segment to the data frame by calling fn_upstream() and
  #using the riverseg column as the first input. For each river segment, this
  #will return a character vector of all upstream segments. By providing
  #fn_upstream with additional details of allSegList, we can speed up this
  #function
  segDataFrame$upstreamSegs <- mapply(fn_upstream,riv.seg = segDataFrame$riverseg,
                                     MoreArgs = list(
                                       AllSegList = AllSegList$riverseg,
                                       allSegToSeg = allSegToSeg,
                                       allSegThisSeg = allSegThisSeg,
                                       allSegOldshed = allSegOldshed
                                       ),
                                     SIMPLIFY = FALSE,USE.NAMES = FALSE)
  
  #Now, we need to find all segments upstream of the upstream segments. These
  #may branch into different tributaries so we will need to run fn_upstream
  #repeatedly. Lets first create an object to store the results in. We will want
  #to store the final list of segments in AllSegList so lets add that as a
  #column, allUpstreamSegs
  segDataFrame$allUpstreamSegs <- segDataFrame$upstreamSegs
  #Store upstream segments as their own vector. This will be updated in each
  #loop iteration
  upstreamSegs <- segDataFrame$allUpstreamSegs
  #Define a variable that shows when there are no more upstream segments for a
  #given river segment
  allAreNull <- FALSE
  #While there are values in upstreamSegs, find all river segments upstream of
  #the values in upstreamSegs and add to allUpstreamSegs
  while(!allAreNull) {
    #Now get all segments upstream of the upstream segments e.g. the next
    #segment upstream of upstreamSegs. if upstreamSegs is NULL, then there are
    #no more upstream segments of a given river segment and fn_upstream will
    #automatically return NULL
    upstreamSegs <- mapply(fn_upstream,riv.seg = upstreamSegs,
                           MoreArgs = list(
                             AllSegList = AllSegList$riverseg,
                             allSegToSeg = allSegToSeg,
                             allSegThisSeg = allSegThisSeg,
                             allSegOldshed = allSegOldshed),
                           SIMPLIFY = FALSE,USE.NAMES = FALSE)
    
    #Combine new list of next upstream segments with existing list and unlist
    #for convenience. This gives us a vector for each river segment in
    #AllSegList that has allUpstreamSegs so far. upstreamSegs may still have
    #picked up on additional segments that will be searched for in the next
    #loop. Otherwise, upstreamSegs is now NULL and will be skipped.
    allUpstreamSegs <- mapply(list,segDataFrame$allUpstreamSegs,upstreamSegs,
                              SIMPLIFY = FALSE,USE.NAMES = FALSE)
    allUpstreamSegs <- mapply(unlist,allUpstreamSegs,
                              SIMPLIFY = FALSE,USE.NAMES = FALSE)
    
    # #Find which lists added additional segments e.g. may have more additional
    # #upstream segments and need to be checked again
    # checkOut <- unlist(
    #   mapply(identical,AllSegList$allUpstreamSegs,allUpstreamSegs,
    #        SIMPLIFY = FALSE,USE.NAMES = FALSE)
    # )
    # 
    #Store the new list of upstream river segments in allUpstreamSegs
    segDataFrame$allUpstreamSegs <- allUpstreamSegs
    
    #Check if all upstream segments are NULL. This will occur when all branches
    #of all river segments have been sorted through as a NULL is only returned
    #when no upstream segments remain. Otherwise, upstreamSegs will have segment
    #IDs and will need to be searched for the segments upstream of those
    allAreNull <- mapply(is.null,upstreamSegs)
    allAreNull <- all(allAreNull)
  }
  
  #For comparison with fn_extract_basin(), add original segment into list of
  #upstream segments and add as new column into AllSegList
  allSegments <- mapply(list,segDataFrame$allUpstreamSegs,
                        segDataFrame$riverseg,
                        SIMPLIFY = FALSE,USE.NAMES = FALSE)
  allSegments <- mapply(unlist,allSegments,
                        SIMPLIFY = FALSE,USE.NAMES = FALSE)
  segDataFrame$allSegments <- allSegments
  
  #Return all of segDataFrame from this function unless the user has specified
  #riv.seg and that they wish to only get the watershed river segment ids
  out <- segDataFrame
  #If the user wishes to only get the watershed river segments, return only the
  #last column
  if(getAllSegmentsOnly){
    if(length(riv.seg) == 0){
      stop("riv.seg must contain at least 1 river segment id to use this
      feature. When TRUE, getALLSegmentsOnly will return a list of upstream
      river segments for only those river segments specified in riv.seg.
      Please correct inputs.")
    }
    out <- segDataFrame$allSegments
    #In the case that riv.seg is length one, convert the output list to a
    #character vector for convenience (and to match older hydrotools function
    #i.e. hydrotools:::fn_ALL.Upstream() and fn_upstream(), which this function
    #is intended to replace)
    if(length(riv.seg) == 1){
      out <- unlist(out)
    }
  }
  
  return(out)
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


# fn_iha_7q10 
#'
#' @name fn_iha_7q10
#' @title fn_iha_7q10
#' @description provide the 7q10 from a given flow timeseries
#' @param zoots a timeseries flormatted in zoo (required by IHA)
#' @return singel numeric value for 7q10
#' @import PearsonDS
#' @export fn_iha_7q10
#' @examples NA
#' @seealso NA
fn_iha_7q10 <- function(zoots) {
  g2 <- group2(zoots) 
  #print("Group 2, 7-day low flow results ")
  #print(g2["7 Day Min"])
  x <- as.vector(as.matrix(g2["7 Day Min"]))
  # fudge 0 values
  # correct for zeroes?? If so, use this loop:
  # This is not an "approved" method - we need to see how the GS/other authorities handles this
  for (k in 1:length(x)) {
    if (x[k] <= 0) {
      x[k] <- 0.00000001
      print (paste("Found 0.0 average in year", g2["year"], sep = " "))
    }
  }
  x <- log(x)
  if (length(x) <= 1) {
    return(exp(x[1]))
  } else {
    pars <- PearsonDS:::pearsonIIIfitML(x)
    x7q10 <- exp(qpearsonIII(0.1, params = pars$par))
    return(x7q10);
  }
}


# fn_iha_mlf 
#'
#' @name fn_iha_mlf
#' @title fn_iha_mlf
#' @description provide the quantile of minimum observed monthly flow for the month over a period of years
#' @param zoots a timeseries flormatted in zoo (required by IHA)
#' @param targetmo month in title case string
#' @param q numeric what quantile to return (default median/0.5)
#' @return singel numeric value for 7q10
#' @export fn_iha_mlf
fn_iha_mlf <- function(zoots, targetmo, q=0.5) {
  modat <- group1(zoots,'water','min')  # IHA function that calculates minimum monthly statistics for our data by water year	 
  message(paste("Grabbing ", targetmo, " values ", sep=''))
  g1vec <- as.vector(as.matrix(modat[,targetmo]))  # gives only August statistics
  
  # calculates the 50th percentile - this is the August Low Flow
  # August Low Flow = median flow of the annual minimum flows in August for a chosen time period
  message("Performing quantile analysis")
  x <- quantile(g1vec, q, na.rm = TRUE);
  return(as.numeric(x));
}

# fn_iha_flow_extreme 
#'
#' @name fn_iha_flow_extreme
#' @title fn_iha_flow_extreme
#' @description provide the quantile of annual period 
#' @param flows a timeseries flormatted in zoo (required by IHA)
#' @param metric the flow period and type (min/max)
#' @param stat default=min, options: max, median
#' @param wyear_type water year default=calendar, options: water
#' @return singel numeric value for the selected index/stat
#' @export fn_iha_flow_extreme
fn_iha_flow_extreme <- function(flows, metric, stat='min', wyear_type='calendar') {
  g2flows <- group2(flows, yearType = wyear_type);
  metric_flows <- g2flows[metric];
  if (stat == 'min') {
    ndx = which.min(as.numeric(metric_flows[,metric]));
  } else if (stat == 'max') {
    ndx = which.max(as.numeric(metric_flows[,metric]));
  } else if (stat == 'median') {
    ndx = which(as.numeric(metric_flows[,metric]) == median(as.numeric(metric_flows[,metric])))
  }
  
  metric_flows_Qout = round(g2flows[ndx,metric],6);
  extreme_year = g2flows[ndx,]$"year";
  
  if (is.na(metric_flows_Qout)) {
    metric_flows_Qout = 0.0
    extreme_year = 0
  }
  return(c(metric_flows_Qout, extreme_year))
}
