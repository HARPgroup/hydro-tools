# cia_utils.R
# Supporting functions for the CIA static and Shiny dashboards.

#' Model element finder
#' @description Finds the model, and its parent feature or prop given an OM elementid
#' @param elid Desired elementid
#' @param ds RomDataSource
#' @return data frame of info about the target elementid
#' @import sqldf
#' @export om_find_dh_elid
om_find_dh_elid <- function(elid, ds) {
  
  model_search_elid <- sqldf(
    paste0(
    "select a.name, a.hydrocode, 
   CASE 
     WHEN a.hydroid IS NULL THEN 'model' 
     ELSE 'feature'
   END as parent_type, 
   CASE 
     WHEN a.hydroid IS NULL THEN p.pid 
     ELSE a.hydroid
   END as parent_id,
   b.propname, b.pid, e.propvalue as elid
   from dh_properties as e 
   left outer join dh_properties as b
   on (
     e.featureid = b.pid
   )
   left outer join dh_feature as a
   on (
     a.hydroid = b.featureid
   )
   left outer join dh_properties as p
   on (
     p.pid = b.featureid
     and b.entity_type = 'dh_properties'
   )
   where e.propname = 'om_element_connection'
   and e.propvalue = ",elid),
    connection = ds$connection
  )
  if (nrow(model_search_elid) == 0) {
    model_search_elid = FALSE
  }
  return(model_search_elid)
} 

om_model_run_monitor <- function(ds_model, minspast=60, elids = FALSE, limit = 100) {
  status_sql = "select a.elementid, a.elemname, b.status_mesg, b.runid, b.host, b.last_updated, 
       CASE 
         WHEN report is NULL THEN  
           replace(remote_url,'runlog'||b.runid||'.'||c.elementid, 'report'||c.elementid||'-'||b.runid) 
         ELSE report 
       END as report 
    from scen_model_element as a, system_status as b 
    left outer join scen_model_run_elements as c 
       on( b.element_key = c.elementid and c.runid = b.runid ) 
    where a.elementid = b.element_key 
    "
  if (minspast > 0) {
    status_sql = fn$paste(status_sql, "and b.last_updated >= (now() - interval '$minspast minutes')")
  }
  if (!is.logical(elids)) {
    elist = paste0("(", paste(elids, collapse = ","), ")")
    status_sql = fn$paste(status_sql, "and a.elementid in $elist")
  }
  
  status_sql = fn$paste(status_sql, "order by last_updated DESC LIMIT $limit")
  message(status_sql)
  status_recs = sqldf::sqldf(
    status_sql,
    connection = ds_model$connection
  )
  return(status_recs)
}

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


#' @name fn_ALL.upstream
#' @title fn_ALL.upstream
#' @description Returns a vector of the segments upstream of riv.seg based on
#'   the list in AllSegList
#' @details Find Upstream Segments: A basic function that finds the segments 
#' upstream of a given segment(s). riv.seg may be a character vector of segments.
#' and AllSegList must be a dataframe. This function returns a vector of
#' upstream segments
#' @param AllSegList Data frame that must contain riverseg and all river
#' segments in state. All columns are maintained in final This function can only
#' find segments that are in this list so the user should use a comprehensive
#' list of segments as demonstrated in the hydrotools package in cia_utils.R
#' @param riv.seg If user wishes to only return certain segments, they can
#' specify these in riv.seg to get a data frame showing that segment, other data
#' in AllSegList, and all upstream segments. May be left as NULL to instead find
#' upstream segments of all unique segments in AllSegList
#' @param getAllSegmentsOnly If the user only wishes to get the ids of upstream
#'   segments and the outlet i.e. this is inclusive of riv.seg. This may only be
#'   used if riv.seg is specified. Defaults to TRUE.
#' @return string next upstream segment ID
#' @export fn_ALL.upstream
fn_ALL.upstream <- function(
    AllSegList,
    riv.seg = NULL,
    getAllSegmentsOnly = TRUE
){
  #AllSegList should be a data frame and must contain a river segment column:
  if(!inherits(AllSegList, "data.frame") || 
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
     inherits(riv.seg, 'character')
  ){
    #Get only river segments specified by user
    segDataFrame <- segDataFrame[segDataFrame$riverseg %in% riv.seg,]
    #If user has only provided a single column dataframe and a single riv.seg,
    #this may return a vector. In these cases, convert to data.frame for
    #fn_upstream()
    if(is.character(segDataFrame) && length(segDataFrame) == 1){
      segDataFrame <- data.frame(riverseg = segDataFrame)
    }
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
#' @import stringr
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
    if (hydrotools:::is.empty(ModelSegments[k,6])==TRUE){
      ModelSegments[k,6]<- 'NA'
    } 
    k<-k+1
  }
  SegDownstream <- as.numeric(which(as.character(ModelSegments$RiverSeg)==as.character(riv.seg)))
  SegDownstream <- ModelSegments$Downstream[SegDownstream]
  SegDownstream <- strsplit(as.character(SegDownstream), "\\+")
  SegDownstream <- try(SegDownstream[[1]], silent = TRUE)
  if (inherits(SegDownstream, 'try-error')) {
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
#' @name fn_iha_7q10
#' @title fn_iha_7q10
#' @description Calculate the 7Q10 from a flow timeseries
#' @details
#' This function was originally inspired by the Nature Conservancy's IHA package
#' and relied on \code{group2()}. However, now this function is just a simple
#' wrapper of \code{xQy()} to ensure consistency with the rest of DEQ. It takes
#' in a data frame or zoo timeseries and outputs the 7Q10 as a single value
#' numeric.
#' @param zoots a timeseries formatted in zoo or a data frame that contains a
#'   date and flow column, to be specified by user
#' @param flowColumnIn If a dataframe is provided to the function, this is the
#'   name of the column that contains the flow data for analysis. Ignored if a
#'   zoo timeseries was provided.
#' @param dateColumnIn If a dataframe is provided to the function, this is the
#'   name of the column that contains the dates. Ignored if a zoo timeseries was
#'   provided
#' @return single numeric value for 7Q10
#' @export fn_iha_7q10
#' @examples
#' flows <- dataRetrieval::readNWISdv("01631000","00060")
#' flows <- dataRetrieval::renameNWISColumns(flows)
#' #Convert flows to zoo
#' flows_zoo <- zoo::as.zoo(x = flows$Flow)
#' zoo::index(flows_zoo) <- flows$Date
#' fn_iha_7q10(flows_zoo)
#' @seealso xQy
fn_iha_7q10 <- function(zoots, flowColumnIn = "Flow", dateColumnIn = "Date") {
  #Calculate critical low flows from the zoo time series
  low_flows <- xQy(gageDataIn = zoots, flowColumn = flowColumnIn, 
                   dateColumn = dateColumnIn,
                   AYS = "04-01", AYE = "03-31",
                   startYear = NULL, endYear = NULL,
                   x = 7, y = 10,
                   IncludeSummerFlow = FALSE)
  #Return the 7Q10
  out_7Q10 <- low_flows$Flows$n7Q10
  return(out_7Q10)
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
  x <- stats::quantile(g1vec, q, na.rm = TRUE);
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
    ndx = which(as.numeric(metric_flows[,metric]) == stats::median(as.numeric(metric_flows[,metric])))
  }
  
  metric_flows_Qout = round(g2flows[ndx,metric],6);
  extreme_year = g2flows[ndx,]$"year";
  
  if (is.na(metric_flows_Qout)) {
    metric_flows_Qout = 0.0
    extreme_year = 0
  }
  return(c(metric_flows_Qout, extreme_year))
}


# is.empty 
#'
#' @name is.empty
#' @title is.empty
#' @description Checks common NULL, NA, length cases for empty values
#' @details
#' Imported from rapportools and should be used sparingly as explicit length,
#' NA, and NULL checks are better for ensuring consistent data strucutre. From
#' rapportools: "Rails-inspired helper that checks if vector values are "empty",
#' i.e. if it's: NULL, zero-length, NA, NaN, FALSE, an empty string or 0. Note
#' that unlike its native R is.<something> sibling functions, is.empty is
#' vectorised (hence the "values")."
#' @param x an object to check its emptiness. 
#' @param ... additional arguments for sapply
is.empty <- function (x, ...) 
{
  if (length(x) <= 1) {
    if (is.null(x)) 
      return(TRUE)
    if (length(x) == 0) 
      return(TRUE)
    if (is.na(x) || is.nan(x)) 
      return(TRUE)
    if (is.character(x) && nchar(x) == 0) 
      return(TRUE)
    if (is.logical(x) && !isTRUE(x)) 
      return(TRUE)
    if (is.numeric(x) && x == 0) 
      return(TRUE)
    return(FALSE)
  }
  else sapply(x, is.empty, trim = trim, ...)
}


#'@name simple_elfgen
#'@title Simple Hydro Elfgen Wrapper
#'@description A wrapper that makes it easier to access VA Hydro's imported EDAS
#'  dataset and provides simple arguments to eflgen package.
#'@details This function provides the necessary inputs to call the
#'  elfgen::elfgen() function. Simply, this wrapper function uses easily
#'  accessible input to find the necessary NHD Plus watersheds and fish taxa
#'  data to create the \code{watershed.df} input in elfgen() and returns its
#'  output. The function will use the largest NHDPlus feature of the input code
#'  that is contained within the user input hydro feature based on the provided
#'  hydroid
#'@param ds A datasource provided by the user, usally the RomDataSource instance
#'  create in DEQ config.R files, querying drupal.dh03
#'@param ws_code The code of the NHDPlus HUC of interest, which may be derived
#'  from a VA Hydro feature using \code{hydrotools::simple_nhdPlusFlows()} OR
#'  the hydroid of the VA Hydro feature with bundle watershed.
#'@param huc_level Which NHD waterhsed level is represented by nhd_code? "huc8"
#'  is the default
#'@param dataset Should \code{elfgen()} use USGS Icthys data or DEQ EDAS data
#'  (via Hydro, which may be outdated but is state-specific data). Options are
#'  "IchthyMaps" for USGS or "VAHydro-EDAS" and "VAHydro-EDAS-ds" for DEQ data,
#'  with the former leveraging \code{nhdPlusTools::get_nhdplus()} to derive flows
#'  and the latter using imported flows from the provided datasource
#'@param axisNames Either a logical boolean (default TRUE) or character vector
#'  to be used to derive the x-axis names for the plotted parameters. If TRUE,
#'  then axis names will be guessed based on known VA Hydro varkey conversions.
#'  If a character vector is provided of the same length as ws_varkey, these
#'  will be used as plot titles. Otherwise, the ws_varkey will serve as the
#'  label
#'@param ws_varkey Which NHDPlus flow for the \code{huc_level} should be used to
#'  generate the \code{elfgen()} ecologic limit function? The options are mean
#'  flow for any month but MUST be in the form "erom_q0001e_aug". Use
#'  "erom_q0001e_mean" to use mean annual flow or "All" to get all flows and all
#'  elfgen regressions
#'@param quantile A specified value for the quantile of interest - 0.95 equals
#'  the 95th percentile. This is used in the eflgen ecologic limit function
#'  analysis
#'@param breakpt A breakpoint - either user-supplied fixed value (single or a
#'  numeric of equal length to ws_varkey) or may be derived using elfgen
#'  breakpoint functions \code{bkpt_pwit()} or \code{bkpt_ymax()} by inputting
#'  the character value "ymax" or "pwit" respectively. If using "pwit", it is
#'  recommended the user provide a \code{blo} and \code{bhi} value as documented
#'  in the elfgen pacakge. \code{blo} will default to 0 and \code{bhi} to the
#'  \code{yaxis_thresh} value input, which itself defaults to the maximum number
#'  of taxa. "pwit" will default to ymax if none can be identified. "pwit" and
#'  "ymax" will be determine breakpoints separately for each ws_varkey
#'@param yaxis_thresh The maximum value to be used in the plot. If this value is
#'  left NULL by user, it will be set as the maximum value of the dataset. If
#'  using "pwit" as the breakpt, this will also serve as the \code{bhi} value if
#'  not otherwise specified by user.
#'@param blo When using the "pwit" breakpt analysis, this is the "bound low"
#'  value, or the lower bound of the piecewise range and defaults to 0. See
#'  \code{elfgen::bkpt_pwit()} for additional details.
#'@param bhi When using the "pwit" breakpt analysis, this is the "bound high"
#'  value, or the upper bound of the piecewise range and defaults to
#'  \code{yaxis_thresh}. See \code{elfgen::bkpt_pwit()} for additional details.
#'@return A list with elfgen as the list with plot image and dataframe of ELF statistics (see
#'  \code{elfgen::elfgen()} for more details), the flow and fish data used in
#'  the ELF derived from dataset, and the threshold/breakpoint that is set or
#'  calculated
#'@examples \dontrun{simple_elfgen( ds = ds, ws_code = 68069, huc_level = "huc8",
#'  dataset = 'VAHydro-EDAS', ws_varkey = c('erom_q0001e_aug','erom_q0001e_mar'), quantile = 0.8,
#'  breakpt = "ymax", yaxis_thresh = NULL, blo = 0, bhi = yaxis_thresh, axisNames = TRUE)}
#'@export
simple_elfgen <- function(
    ds, ws_code, huc_level = "huc8", dataset = 'VAHydro-EDAS', ws_varkey = 'All',
    quantile = 0.8, breakpt = "ymax", yaxis_thresh = NULL, axisNames = TRUE,
    blo = 0, bhi = yaxis_thresh){
  if(any(tolower(ws_varkey) == "all")){
    #All nhd flow varkeys
    ws_varkey <- c('erom_q0001e_mean', 
                   'erom_q0001e_jan', 'erom_q0001e_feb', 'erom_q0001e_mar',
                   'erom_q0001e_apr', 'erom_q0001e_may', 'erom_q0001e_june',
                   'erom_q0001e_july', 'erom_q0001e_aug', 'erom_q0001e_sept',
                   'erom_q0001e_oct', 'erom_q0001e_nov', 'erom_q0001e_dec')
  }
  #Find the NHD feature based on the HUC level and code
  watershed.code <- as.character(ws_code)
  watershed.bundle <- 'watershed'
  watershed.ftype <- paste("nhd_", huc_level, sep = "")
  watershed_feature = RomFeature$new(ds, list(ftype = watershed.ftype,
                                              bundle = watershed.bundle,
                                              hydrocode = watershed.code), TRUE)
  #If no NHD watershed was found, try searching for this watershed using hydroid
  if(is.na(watershed_feature$hydroid)){
    watershed_feature = RomFeature$new(ds, list(bundle = watershed.bundle,
                                                hydroid = watershed.code,
                                                ftype = "vahydro"), TRUE)
    
    #If no feature was found, return FALSE and warn user to check inputs
    if(is.na(watershed_feature$hydroid)){
      message("No matching watershed with this NHD hydrocode nor any watershed with
             this hydroid found. Please check inputs. ")
      return(FALSE)
    }
  }

  #Based on user input, use either Icthy data from USGS or DEQ monitoring data
  if (dataset == 'IchthyMaps'){
    #if loop below works only for huc:6,8,10 due to naming convention in containing_watershed
    if(huc_level == 'huc8'){
      watershed.code <- stringr::str_sub(watershed.code, -8,-1)
    }
    watershed.df <- elfgen::elfdata(watershed.code)
  }else if (dataset == "VAHydro-EDAS" | dataset == "VAHydro-EDAS-ds"){
    x_metrix_sql <- ''
    flow_type_sql <- ''
    flow_join_sql <- ''
    if(dataset == "VAHydro-EDAS-ds"){
      #If the user wishes to get flows directly from the datasource, join these
      #in using the requested varkeys
      x_metrix_sql <- "dap.propvalue AS x_metric,"
      flow_type_sql <- "dav.varkey as flow_type,"
      flow_join_sql <- 
        "LEFT JOIN dh_variabledefinition AS dav 
         ON dav.varkey IN ([ws_varkey])
         LEFT JOIN dh_properties AS dap 
         ON (
          dap.featureid = ws.hydroid
          AND dap.entity_type = 'dh_feature'
          AND dav.hydroid = dap.varid
         )"
    }
     
    sql <- paste(
    "SELECT event.tid, 
	    to_timestamp(event.tstime),
	    pv.varkey,
	    biodat.propcode,
	    biodat.propvalue AS y_metric,",
      x_metrix_sql, flow_type_sql,
	    "CASE 
        WHEN ws.ftype = 'nhd_huc8' THEN REPLACE(ws.hydrocode,'nhd_huc8_','') 
        WHEN ws.ftype = 'nhd_huc12' THEN REPLACE(ws.hydrocode,'huc12_', '') 
        WHEN ws.ftype = 'vahydro' THEN REPLACE(ws.hydrocode,'vahydrosw_wshed_','') 
        ELSE ws.hydrocode
      END AS hydrocode,
      st.hydroid AS station_hydroid
	
    FROM dh_feature_fielded AS cov
  
    LEFT JOIN dh_feature_fielded AS ws
    ON ST_Contains(cov.dh_geofield_geom, ws.dh_geofield_geom)
  
    LEFT JOIN dh_feature_fielded AS st
    ON ST_Contains(ws.dh_geofield_geom, st.dh_geofield_geom)
  
    LEFT JOIN dh_variabledefinition AS tsv 
    ON (tsv.varkey = 'aqbio_sample_event')
  
    LEFT JOIN dh_timeseries AS event
    ON (
      event.varid = tsv.hydroid
      AND event.featureid = st.hydroid
    ) 
    LEFT JOIN dh_properties AS biodat
    ON (
      biodat.featureid = event.tid
      AND biodat.entity_type = 'dh_timeseries'
    ) 
    LEFT JOIN dh_variabledefinition AS pv 
    ON (
      pv.varkey = '[bio_varkey]'
      AND pv.hydroid = biodat.varid
    )",flow_join_sql,
    "LEFT JOIN dh_variabledefinition AS srv 
    ON srv.varkey = 'sampres'
    LEFT JOIN dh_properties AS sr 
    ON (
      sr.featureid = event.tid
      AND sr.entity_type = 'dh_timeseries'
      AND srv.hydroid = sr.varid
    )
    WHERE cov.hydroid = [covid] 
	    AND pv.hydroid IS NOT NULL
	    AND ws.ftype = '[ws_ftype]'
	    AND ws.bundle='watershed'
	    AND sr.propcode = '[sampres]'")
    
    config <- list(
      covid = watershed_feature$hydroid,
      ws_ftype = 'nhdplus',
      ws_varkey = ws_varkey,
      bio_varkey = 'aqbio_nt_total',
      sampres = 'species'
    )
    sql <- stringr::str_replace_all(sql, '\\[covid\\]', as.character(config$covid))
    sql <- stringr::str_replace_all(sql, '\\[ws_ftype\\]', as.character(config$ws_ftype))
    sql <- stringr::str_replace_all(sql, '\\[ws_varkey\\]', paste0("'",as.character(config$ws_varkey),"'",collapse = ","))
    sql <- stringr::str_replace_all(sql, '\\[bio_varkey\\]', as.character(config$bio_varkey))
    sql <- stringr::str_replace_all(sql, '\\[sampres\\]', as.character(config$sampres))
    message(paste("querying for samples contained by", watershed_feature$ftype, watershed_feature$hydrocode))
    watershed_data <- sqldf::sqldf(sql, connection = ds$connection)
    #If the user uses flows directly from the datasource, then we can just
    #output the returned query. Otherwise, we need to add in the data by first
    #finding the flows from nhdPlusTools
    if(dataset == "VAHydro-EDAS-ds"){
      watershed.df <- watershed_data[!duplicated(watershed_data[,c("tid")]),
                                     !(names(watershed_data) %in% c("x_metric","flow_type"))]
      #For each monthly flow requested by the user, add as column to pivot the
      #data frame wider:
      for(i in config$ws_varkey){
        flowData <- watershed_data[watershed_data$flow_type == i,]
        watershed.df[,i] <- flowData$x_metric[match(flowData$tid,watershed.df$tid)]
      }
      
    }else{
      #Get the flows of all requested nhd watersheds
      nhdFlows <- nhdplusTools::get_nhdplus(comid = unique(watershed_data$hydrocode))
      #Rename the columns using VA Hydro varkeys and remove geometry column
      nhdFlows <- renameNHD(nhdFlows, returnPlotName = FALSE)
      nhdFlows_noSF <- nhdFlows[,names(nhdFlows) != "geometry"]
      #Join requested flows into the watershed_data data frame
      watershed.df <- sqldf::sqldf(
        paste("SELECT wsdf.*,",paste0("nhddf.",ws_varkey,collapse = ","),
              "FROM watershed_data as wsdf
              LEFT JOIN nhdFlows_noSF as nhddf
              ON wsdf.hydrocode = nhddf.comid")
      )
    }
  }else{
    message("dataset not yet implemented. Please use 'VAHydro-EDAS-ds',
            'VAHydro-EDAS', or 'IchthyMaps'")
    return(FALSE)
  }
  
  #If the user has not provided the maximum y-axis value for plotting, assume
  #the full range is applicable
  yaxis_threshi <- yaxis_thresh
  if(is.null(yaxis_threshi)){
    yaxis_threshi <- max(watershed.df$y_metric)
  }

  
  out <- list()
  bkpt_list <- character()
  #For each monthly flow requested by the user, attempt to apply elfgen:
  for(i in config$ws_varkey){
    #Get the column with the flow of interest and change the name to x_metric as
    #expected in elfgen
    target_wsdf <- watershed.df[, c(i, 'y_metric', 'hydrocode')]
    names(target_wsdf)[names(target_wsdf) == i] <- 'x_metric'
    
    #Find the breakpoint for this dataset, dependent on user selection
    if(is.numeric(breakpt) && length(breakpt) == 1){
      breakpti <- breakpt
    }else if(is.numeric(breakpt) && all(!is.na(breakpt)) && length(breakpt) == length(config$ws_varkey)){
      breakpti <- breakpt[which(config$ws_varkey == i)]
    }else if(!is.numeric(breakpt) && tolower(breakpt) == 'ymax'){
      #If the user chooses to set the break point using the ymax value, call
      #elfgen to find that breakpoint
      breakpti <- elfgen::bkpt_ymax(target_wsdf)
    }else if(!is.numeric(breakpt) && tolower(breakpt) == 'pwit'){
    #If the user has elected to use a piecewise breakpoint, find it via elfgen
      #If user wants to use the piecewise function but has failed to provide the
      #low and hi thresholds, specify these as the full data range
      bhii <- bhi
      bloi <- blo
      if(is.null(bhii)){
        bhii <- yaxis_threshi
      }
      if(is.null(bloi)){
        bloi <- 0
      }
      tryPWIT <- tryCatch({
        elfgen::bkpt_pwit(target_wsdf, quantile, blo = bloi, bhi = bhii)
      },error = function(e) {NULL})
        
      if(!is.null(tryPWIT)){
        breakpti <- tryPWIT
      }else{
        message(paste("No piecewise breakpoint identified for",i,"with current
                      blo and bhi breakpoints. Defaulted to ymax."))
        #Default to the ymax for this ws_varkey if no PWIT could be found
        breakpti <- elfgen::bkpt_ymax(target_wsdf)
      }
    }else{
      message("Breakpoint is incorrect. Ensure that it is either set to
      'pwit', 'ymax', a single numeric, or a numeric of equal length to
      ws_varkey (13 for all)")
      return(FALSE)
    }
    
    #Set axis name for elfgen based on ws_varkey for more readible plot labels
    if(is.logical(axisNames) & axisNames){
      axisName <- renameNHD(i, returnPlotName = TRUE)
    }else if(is.logical(axisNames) & !axisNames){
      axisName <- i
    }else if((is.character(axisNames) & length(axisNames == config$ws_varkey))){
      axisName <- axisNames[i]
    }
    
    #Call elfgen, providing the ichtys or queried EDAS data and the user supplied
    #breakpoint/maximum value
    elf <- elfgen::elfgen(
      "watershed.df" = target_wsdf,
      "quantile" = quantile,
      "breakpt" = breakpti,
      "yaxis_thresh" = yaxis_threshi,
      "xlabel" = axisName,
      "ylabel" = "Fish Species Richness"
    )
    bkpt_list <- c(bkpt_list, breakpti)
    #Either add results to list or return a list of length 1 depending on how
    #many flows were requested by user
    if(length(config$ws_varkey) == 1){
      out <- elf
    }else{
      outi <- list(elf)
      names(outi) <- i
      out <- c(out,outi)
    }
  }
  
  return(
    list(
      elfgen = out,
      watershed_data = watershed.df,
      breakpt = bkpt_list,
      yaxis_thresh = yaxis_threshi
    )
  )
}

#'@name simple_nhdPlusFlows
#'@title Get NHDPlus HUC Monthly Flows
#'@description A wrapper function that makes it easy to obtain HUC flows from a
#'  hydro feature
#'@details This function provides the necessary inputs to call nhdPlusTools
#'  functions to extract the relevant HUC flows from an area of interest. The
#'  function will use the largest NHDPlus feature of the input code that is
#'  contained within the user input hydro feature based on the provided hydroid.
#'  Output flow columns will be renamed using VA Hydro varkeys.
#'@param ds A datasource provided by the user, usally the RomDataSource instance
#'  create in DEQ config.R files, querying drupal.dh03
#'@param hydroid The hydroid of a feature of interest. This is used to find
#'  intersecting NHD Plus catchments and to ultimately identify the NHD feature
#'  of interest based on the huc_level input
#'@param huc_level Which NHD waterhsed level should be used to generate the
#'  \code{elfgen()} relationships? Defaults to "huc8", representing HUC8
#'@return A list that contains the HUC code of interest and data on the nhdPlus
#'  segment
#'@examples \dontrun{simple_nhdPlusFlows(ds = ds, hydroid = 68069, huc_level = "huc8")}
#'@export
simple_nhdPlusFlows <- function(ds, hydroid, huc_level = "huc8"){
  #Find the riversegment feature based on user input hydroid
  riverseg_feature <- RomFeature$new(ds, list(hydroid=as.integer(hydroid)), TRUE)
  #Find the NHDPlus watersheds contained within this watershed featuer
  contained_df <- riverseg_feature$find_spatial_relations(
    target_entity = 'dh_feature', 
    inputs = list(
      bundle = 'watershed',
      ftype = 'nhdplus'
    ),
    operator = 'st_contains',
    return_geoms = FALSE,
    query_remote = TRUE
  )
  #Use nhdplusTools to get relevant data regarding these NHDPlus segments
  #contained in the user input river segment
  nhdplus_df <- as.data.frame(nhdplusTools::get_nhdplus(comid= contained_df$hydrocode))
  message(paste("length(nhdplus_df): ", length(nhdplus_df[,1])))
  #Get the flow of interest along with the comid, name, code, and total drainage
  #area
  nhdplus_df <- nhdplus_df[,c("comid", "gnis_name", "reachcode", "totdasqkm",
                              grep("qa_(([0-9]{2})|(ma))",names(nhdplus_df),value = TRUE))]
  nhdplus_df <- renameNHD(nhdplus_df, returnPlotName = FALSE)
  
  #Find the outlet based on the maximum total drainage area
  outlet_nhdplus_segment <- nhdplus_df[which.max(nhdplus_df$totdasqkm),][1,]
  #Get the first X digits of the code based on the user input
  nhd_code <- substr(outlet_nhdplus_segment$reachcode, 1, as.integer(stringr::str_remove(huc_level, "huc")))
  
  return(
    list(
      nhd_code = nhd_code,
      nhdplus_segment = outlet_nhdplus_segment
    )
  )
}

#'@name renameNHD
#'@title Use VA Hydro Varkeys on NHDPlus
#'@description A function to rename the flows from NHDPlus to VAHydro varkeys
#'@details This function takes a data.frame or tibble output from 
#'\code{nhdPlusTools::get_nhdplus()} and renames the relevant flow columns using
#'VA Hydro varkeys from dh_variabledefinition to allow for consistent comparison
#'to data from VA Hydro databases. Alternatively, it takes in a vector of flow
#'names from \code{nhdPlusTools::get_nhdplus()} and returns a vector of readible
#'names (when returnPlotName is TRUE).
#'@param get_nhdplus_df A data frame output from
#'  \code{nhdPlusTools::get_nhdplus()} or the \code{simple_nhdplus()} wrapper.
#'  Alternatively, a vector of nhd flow names
#'@param returnPlotName Logical. Defaults to FALSE. If TRUE, returns a character
#'  vector of names that describe the flow column
#'@return An identical data frame with flow columns renamed using appropriate
#'  varkeys if returnPlotName is FALSE or a character vector of readible names
#'@examples \dontrun{
#'out <- simple_nhdPlusFlows(ds = ds, hydroid = 68069, huc_level = "huc8")
#'out <- renameNHD(out)
#'names(out)
#'}
#'@export
renameNHD <- function(get_nhdplus_df, returnPlotName = FALSE){
  nhd_map <- data.frame(
    newName = c('erom_q0001e_jan', 'erom_q0001e_feb', 'erom_q0001e_mar',
                'erom_q0001e_apr', 'erom_q0001e_may', 'erom_q0001e_june',
                'erom_q0001e_july', 'erom_q0001e_aug', 'erom_q0001e_sept',
                'erom_q0001e_oct', 'erom_q0001e_nov', 'erom_q0001e_dec', 
                'erom_q0001e_mean', "comid", "gnis_name", "reachcode", "totdasqkm"),
    nhdName = c('qa_01', 'qa_02', 'qa_03', 'qa_04', 'qa_05', 'qa_06',
                'qa_07', 'qa_08', 'qa_09', 'qa_10', 'qa_11', 'qa_12',
                'qa_ma', "comid", "gnis_name", "reachcode", "totdasqkm"),
    plotName = c('Mean January Flow (cfs)', 'Mean February Flow (cfs)',
                 'Mean March Flow (cfs)','Mean April Flow (cfs)','Mean May Flow (cfs)',
                 'Mean June Flow (cfs)','Mean July Flow (cfs)',
                 'Mean August Flow (cfs)','Mean September Flow (cfs)',
                 'Mean October Flow (cfs)','Mean November Flow (cfs)',
                 'Mean December Flow (cfs)','Mean Annual Flow (cfs)',
                 "comid", "gnis_name", "reachcode", "totdasqkm")
  )
  if(returnPlotName){
    #If the user has provided a character of names and requested that the plot
    #name be returned, find the new plot name only if all provided names have
    #matches in the key table nhd_map
    if(is.character(get_nhdplus_df) & all(get_nhdplus_df %in% nhd_map$newName)){
      namesForReturn <- nhd_map$plotName[match(get_nhdplus_df, nhd_map$newName)]
    }else{
      message("Some names in get_nhdplus_df could not be found in mapping or a 
              non-character vector provided. Names returned unchanged.")
      namesForReturn <- get_nhdplus_df
    }
    
    return(namesForReturn)
  }else{
    #Find the index of each nhdName (where it is present in the names of
    #get_nhdplus_df) within the names of get_nhdplus_df
    matchIndex <- match(nhd_map$nhdName[nhd_map$nhdName %in% names(get_nhdplus_df)],
                        names(get_nhdplus_df))
    names(get_nhdplus_df)[matchIndex] <- nhd_map$newName
    return(as.data.frame(get_nhdplus_df))
  }
}

# fn_handletimestamp 
#'
#' @name fn_handletimestamp
#' @title Handle Any Time/Date to Timestamp
#' @description Try to bring any date or timestamp into a common format (Unix epoch, TBD)
#' @param ts some type of date or timestamp
#' @return single numeric value for UNix epoch
#' @export fn_handletimestamp
fn_handletimestamp <- function(ts) {
  # don't do date_received as this is a field and is handled there
  if ( !is.null(ts) && (ts != '')) {
    orig <- ts
    # if a valid unix epoch style timestamp has been submitted 
    # this next will try to convert a string
    if (is.na(as.numeric(ts))) {
      # must be a formatted date, not a timestamp integer/float
      # not a valid unix timestamp, so try to convert from some date format
      if (!is.na(lubridate::ymd(ts))) {
        ts <- lubridate::ymd(ts)
      } else if (!is.na(lubridate::mdy(ts))) {
        ts <- lubridate::mdy(ts)
      }
      message(paste("Converted orig to Epoch:", ts))
    }
  }
  return(ts)
}
