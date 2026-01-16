#'@name usgs_bankfull_properties
#'@title usgs_bankfull_properties
#'@description Uses USGS bankfull regional curves for each VA geomorphic
#'  province to calculate bankfull channel stage, width, full width, and side
#'  slope
#'@details See USGS bankfull regression reports for additional information. The
#'  empirical bankfull equations are used to calculate channel properties based
#'  on which province the channel is in and the drainage area at that point.
#'  This data is often used in OM to set local_channel properties. Returns a list 
#'  with bank full stage "max height before floodplain", bank full width "max width before floodplain",
#'  base width "width at lowest stage", side slope of channel, and mannings Roughness
#'@param prov Integer 1 - 4. Geomorphic province 1 = appalachian plateau, 2 =
#'  valley and ridge, 3 = Piedmont, 4 = coastal plain
#'@param da Numeric. The drainage area of your channel.
#'@return A list with the bank full stage (h), bank full width (bf), base width
#'  (b), side slope (z), and mannings roughness (n)
#'@examples 
#'usgs_bankfull_properties(prov = 1,da = 10)
#'@export
usgs_bankfull_properties <- function(prov, da) {
  #Provincial Channel Geometry
  if (prov == 1){
    #Appalachian Plateau
    hc = 2.030 # "c" = coefficient for regional regression eqn
    he = 0.2310 # "e" = exponent for regional regression eqn
    bfc = 12.175
    bfe = 0.4711
    bc = 5.389
    be = 0.5349
    n = 0.036 # Manning's n
    nf = 0.055 # Floodplain n
  } else if (prov == 2){
    #Valley and Ridge
    hc = 1.435
    he = 0.2830
    bfc = 13.216
    bfe = 0.4532
    bc = 4.667
    be = 0.5489
    n = 0.038
    nf = 0.048
  }else if (prov == 3){
    #Piedmont
    hc = 2.137
    he = 0.2561
    bfc = 14.135
    bfe = 0.4111
    bc = 6.393
    be = 0.4604
    n = 0.095
    nf = 0.063
  }else if (prov == 4){
    #Coastal Plain
    hc = 2.820
    he = 0.2000
    bfc = 15.791
    bfe = 0.3758
    bc = 6.440
    be = 0.4442
    n = 0.040
    nf = 0.06
  }
  
  # Regional Regression Eqn's:
  #bank full stage "max height before floodplain":
  h = hc * (da**he)
  #bank full width "max width before floodplain":
  bf = bfc * (da**bfe)
  #base width "width @ lowest stage":
  b = bc * (da**be)
  #side slope of channel:
  z = 0.5 * (bf - b ) / h
  
  return(
    list(
      h = h,
      bf = bf,
      b = b,
      Z = z,
      n = n
    )
  )
}




#'@name simple_wshed_map
#'@title simple_wshed_map
#'@description A function that can create a simple GIS map using the maptiles
#'  library and watershed hydroids input by user
#'@details This function takes in a data source and a watershed hydroid. It then
#'  optionally queries the datasource to identify upstream watersheds. It
#'  creates a simple map of the watershed with a basemap populated by the
#'  maptiles R package following the \code{nhdplusTools:::set_zoom()} function.
#'  A series of point labels can be included to provide more context to the map
#'  An outlet point must be provided to ensure proper map tiles are gotten from
#'  basemap providers. This function creates a plot that is placed in the file
#'  path provided by the user
#'  
#'  Understanding the config input is important to styling the map as needed.
#'  The config should be provided with a list and may include any of the
#'  following elements:
#'  \describe{
#'     \item{zoomNHDPlusTools}{If provided, will set the zoom used to get the
#'     basemap tiles. If not, \code{nhdplusTools:::set_zoom()} will be used}
#'     \item{tileProvider}{The basemap of choice. See , otherwise defaults to
#'     "Esri.NatGeoWorldMap". See \code{?maptiles::get_tiles} for more options}
#'     \item{pointBuffer}{A vector of length 1 or 2 OR a data frame with a
#'     number of rows equal to the number of points to plot in pointSF input and
#'     two columns, latbuffer and lngbuffer. These values will be used to adjust
#'     the location of point labels on the map, allowing for fine tuning of map
#'     labels. See example.}
#'     \item{wshdBuffer}{A vector of length 1 or 2 OR a data frame with a
#'     number of rows equal to the number of watersheds in the plot input and
#'     two columns, latbuffer and lngbuffer. These values will be used to adjust
#'     the location of watershed labels on the map, allowing for fine tuning of map
#'     labels. See example.}
#'     \item{textSize}{Label font size using R base::plot units. Defaults to 0.4}
#'     \item{pointpch}{Numeric. The pch to use to display the points on the map
#'     e.g. the point type. See base R pch details for more information}
#'     \item{width}{numeric. The width of the image. If specified, height and
#'     units must be provided. res can be provided but will default to 300 ppi.
#'     Defaults to 5 inches}
#'     \item{height}{numeric. The height of the image. If specified, width and
#'     units must be provided. res can be provided but will default to 300 ppi.
#'     Defaults to 4 inches.}
#'     \item{units}{numeric. The units of the height and width inputs. Defaults 
#'     to 'in' but see \code{?plot()} for more options. If specified, width and
#'     height must be provided. res can be provided but will default to 300 ppi}
#'     \item{res}{numeric. The resolution of the image. If specified, height, width,
#'     and units must be provided. res defaults to 300 ppi}
#'  }
#'@param ds A datasource provided by the user, usally the RomDataSource instance
#'  create in DEQ config.R files, querying drupal.dh03
#'@param wshdHydroid A character vector of watershed hydroid(s) to include on
#'  the plot. If more than one is provided, the findUpstream input is disabled.
#'@param findUpstream Logical (TRUE or FALSE). Should the function find and plot
#'  all watersheds upstream of the user provided watershed? This cannot be used
#'  if the user has provided more than one watershed in wshdHydroid. Defaults to
#'  FALSE
#'@param pointSF An SF data frame that has three columns: lat, long, and label.
#'  The lat and long coordinates should be cooridinates in some epsg specified
#'  in the SF object. The label will be used to label the point on the plot. One
#'  of these points must be labeled "outlet" and should represent the most
#'  downstream point in the watershed
#'@param outFile A path to which the plot will be written using PNG. Defaults to
#'  working directory.
#'@param config A list with many optional style elements. See details for more
#'  information
#'  
#'@return A list with the bank full stage (h), bank full width (bf), base width
#'  (b), side slope (z), and mannings roughness (n)
#'@examples 
#'#wshdHydroid <- 67704
#'#pointSF <- data.frame(
#'#  lat = c(37.771666667,37.76781),
#'#  long = c(-79.465555556,-79.38390),
#'#  label = c("Lexington Golf Club","Outlet")
#'#)
#'#pointSF <- sf::st_as_sf(pointSF,coords = c('long','lat'),
#'#                        crs = 4326)
#'#outFile <- 'lgMap.PNG'
#'#
#'#
#'#simple_wshed_map(ds, wshdHydroid, findUpstream = TRUE,
#'#       pointSF,
#'#       outFile,config = list(
#'#         zoomNHDPlusTools = 10,
#'#         textSize = 0.4,
#'#         wshdBuffer = data.frame(
#'#           latbuffer = c(-0.03,-0.06,0),
#'#           lngbuffer = c(-0.03,-0.07,-0.02)
#'#         ),
#'#         pointBuffer = c(0.24,0),
#'#         tileProvider = "Esri.WorldTopoMap"
#'#       )
#'#)
#'@export
simple_wshed_map <- function(ds,wshdHydroid, findUpstream = FALSE,
                   #May include a label and buffer column
                   pointSF = NULL,
                   outFile = paste0(getwd(),"/simple_wshed_map.png"),
                   config = NULL) {
  #First, get all the requested hydroids:
  wshds <- DBI::dbGetQuery(ds$connection,paste0(
    "SELECT * FROM dh_feature_fielded WHERE hydroid IN (",
    paste0(wshdHydroid,collapse = ","),")"
  ))
  
  #If the user has elected to find upstream segments, only do so if only one
  #watershed was provided
  if(findUpstream && length(wshdHydroid) == 1){
    allRsegs <- DBI::dbGetQuery(ds$connection,
                           "SELECT hydroid,hydrocode as riverseg FROM dh_feature 
                           WHERE ftype = 'vahydro' 
                           and bundle = 'watershed'")
    #Remove non-standard river segments:
    allRsegs <- allRsegs[grepl("^vahydrosw_wshed_",allRsegs$riverseg),]
    
    #FInd upstream segments:
    upstreamSegs <- fn_ALL.upstream(allRsegs,wshds$hydrocode)
    
    #Get all the requested watersheds:
    wshds <- DBI::dbGetQuery(ds$connection,paste0(
      "SELECT * FROM dh_feature_fielded WHERE hydrocode IN (",
      paste0("'",upstreamSegs,"'",collapse = ","),")"
    ))
    
    
  }else if(findUpstream && length(wshdHydroid) > 1){
    message("Cannot find upstream segments if more than one hydroid was provided")
  }
  #Convert to spatial sf object:
  wshds <- sf::st_as_sf(wshds,wkt = 'dh_geofield', crs = 4326)
  
  #Check that points are provided appropriately
  if(!all(names(pointSF) %in% c("label","geometry")) ||
     !all(class(pointSF) %in% c('sf','data.frame'))
  ){
    warning("PointSFC is missing critical fields. Must have label and 
            geometry and must be class sf and data.frame")
  }
  
  
  #Get the watershed outlet to pass to plot_nhdplus
  outlet <- pointSF[pointSF$label == "Outlet",]
  if(nrow(outlet) == 0){
    warning("PointSFC must contain a row with Label of 'Outlet' that serves as basis for plot_nhdplus")
  }
  #Get NHD Info for point
  nhd_out <- nhdplusTools::get_nhdplus(outlet$geometry)
  #Plot the basins
  if(is.null(config$zoomNHDPlusTools)){
    mapNHD <- nhdplusTools::plot_nhdplus(outlets = list(nhd_out$comid),
                                         actually_plot  = FALSE)
    zoom <- set_zoom(sf::st_bbox(wshds))
  }else{
    mapNHD <- nhdplusTools::plot_nhdplus(outlets = list(nhd_out$comid),
                                         zoom = config$zoomNHDPlusTools,
                                         actually_plot = FALSE)
    zoom <- config$zoomNHDPlusTools
  }
  if(is.null(config$tileProvider)){
    tileProvider <- "Esri.NatGeoWorldMap"
  }else{
    tileProvider <- config$tileProvider
  }
  tiles <- maptiles::get_tiles(sf::st_bbox(wshds), zoom = zoom, 
                               crop = FALSE, verbose = FALSE, 
                               provider = tileProvider)
  
  #Rearrange watersheds in DA order
  wshds$comArea <- sf::st_area(sf::st_make_valid(wshds$dh_geofield))
  wshds <- wshds[order(wshds$comArea,decreasing = TRUE),]
  
  #Get locations for labels, adjusting for buffer:
  pointPlot <- pointSF[pointSF$label != 'Outlet',]
  pointLabelLocations <- sf::st_coordinates(pointPlot$geometry)
  wshdLabelLocations <- sf::st_coordinates(
    sf::st_centroid(
      sf::st_make_valid(
        wshds$dh_geofield
      )))
  #point buffer, which may have x,y buffers
  if(!is.null(config$pointBuffer)){
    if(inherits(config$pointBuffer, "numeric") & length(config$pointBuffer) <= 2){
      pointLabelLocations <- sweep(pointLabelLocations,2,config$pointBuffer)
    }else if(
      !is.null(config$pointBuffer) &&
      (all(dim(config$pointBuffer) == dim(pointLabelLocations)) |
       length(config$pointBuffer) == length(pointLabelLocations))
    )
      pointLabelLocations <- pointLabelLocations + config$pointBuffer
  }
  #watershed buffer, which may be specific to each watershed or a generic buffer
  #vector
  if(inherits(config$wshdBuffer, "numeric") & length(config$wshdBuffer) <= 2){
    wshdLabelLocations <- sweep(wshdLabelLocations,2,config$wshdBuffer)
  }else if(
    !is.null(config$wshdBuffer) &&
    (all(dim(config$wshdBuffer) == dim(wshdLabelLocations)) |
     length(config$wshdBuffer) == length(wshdLabelLocations))
  ){
    wshdLabelLocations <- wshdLabelLocations + config$wshdBuffer
  }
  
  
  #Check to see if user has specified any height, width for the image. Needs
  #units and res if so
  plotWidth <- 5
  plotHeight <- 4
  plotUnits <- "in"
  plotRes <- 300
  if( all(c(!is.null(config$width), !is.null(config$height), !is.null(config$units))) &&
      all(c(is.numeric(config$width), is.numeric(config$height), is.character(config$units)))
      ){
    #User has provided necessary inputs, use in plot. First check if resolution
    #was provided
    if(!is.null(config$res) && is.numeric(config$res)){
      plotRes <- config$res
    }
    plotWidth <- config$width
    plotHeight <- config$height
    plotUnits <- config$units
  }else if(any(c(!is.null(config$width), !is.null(config$height), !is.null(config$units)))){
    message("Config width/height/units ignored as all must be provided. res will only be used if these inputs are provided.")
  }
  #Check if pointpch was provided, otherwise default to 15.
  if(!is.null(config$pointpch) && is.numeric(config$pointpch)){
    pointpch <- config$pointpch
  }else{
    pointpch <- 13
  }
  #Check if textSize was provided, otherwise default to 0.4
  if(!is.null(config$textSize) && is.numeric(config$textSize)){
    textSize <- config$textSize
  }else{
    textSize <- 0.4
  }
  
  
  #Plot map:
  grDevices::png(outFile,width = plotWidth, height = plotHeight,
      units = plotUnits, res = plotRes)
  graphics::par(mar = c(2,1,1,1))
  terra::plotRGB(tiles, axes = TRUE,mar = c(2,1,1,1))
  # plot(mapNHD$flowline$geometry, add = TRUE, col = 'darkblue',lwd = 2)
  plot(add = TRUE,pointPlot$geometry,pch = pointpch)
  plot(add = TRUE,wshds$dh_geofield)
  graphics::text(pointLabelLocations[,1],pointLabelLocations[,2],
       labels = pointPlot$label,
       cex = textSize,pos = 4)
  graphics::text(wshdLabelLocations[,1],wshdLabelLocations[,2],
       labels = gsub("vahydrosw_wshed_","",wshds$hydrocode),
       cex = textSize)
  grDevices::dev.off()
}


#'@name set_zoom
#'@title set_zoom
#'@description Determine general zoom level for maptiles
#'@details This function was taken from \code{nhdplusTools:::set_zoom} verbatim
#'  and is used to set default zoom levels to grab maptiles background maps
#'@param pb a bounding box vector often from \code{sf::st_bbox()}
set_zoom <- function(pb){
  {
    bb <- sf::st_bbox(pb)
    dim <- max(abs(bb[3] - bb[1]), abs(bb[4] - bb[2]))
    if (dim < 1e+05 & dim >= 60000) {
      10
    }
    else if (dim < 60000 & dim >= 20000) {
      11
    }
    else if (dim < 20000) {
      12
    }
    else if (dim < 150000 & dim >= 80000) {
      9
    }
    else if (dim < 2e+05 & dim >= 150000) {
      8
    }
    else {
      7
    }
  }
  
}


#'@name usgs_nearest_gage
#'@title usgs_nearest_gage
#'@description Find the closest gage for a watershed feature and model.
#'@details This function finds full drainage watersheds that contain a feature, and make an area match.
#'@param watershed_feature a valid RomFeature for the watershed of interest
#'@param watershed_json a valid json model for the watershed of interest
usgs_nearest_gage <- function(watershed_feature, watershed_json) {
  # get gages
  gages <- watershed_feature$find_spatial_relations(
    inputs=list(bundle='watershed', ftype='usgs_full_drainage'), 
    operator='st_centroid_within', 
    TRUE
  )
  # get DA
  gages = sqldf(
    paste0(
      "select *, ST_Area(dh_geofield_geom::geography) / 1609.34^2 as area_sqmi
       from dh_feature_fielded as g ",
      "where g.hydroid in",
      paste0("(", paste(gages$hydroid, collapse=", "),")" )
    ),
    connection=ds$connection
  )
  gages$gageid <- stringr::str_replace_all(gages$hydrocode, "usgs_ws_",'')
  # choose gage based on DA
  gages$distance <- gages$area_sqmi - watershed_model_da(watershed_json)
  gage <- gages[abs(gages$distance) == min(abs(gages$distance)),]
  return(gage)
}


#'@name watershed_model_da
#'@title watershed_model_da
#'@description Find the drainage area from a json model collection.
#'@details This function handles the varying area data formats that these models have historically used.
#'@param watershed_json a valid json model for the watershed of interest
watershed_model_da <- function(watershed_json) {
  
  if ("0. River Channel" %in% names(watershed_json)) {
    drainage_area <- watershed_json$`0. River Channel`$drainage_area$value
  } else {
    drainage_area <- watershed_json$local_channel$drainage_area$value
  }
  return(drainage_area)
}


#'@name usgs_calib_rarray
#'@title usgs_calib_rarray
#'@description Create a best guess calibration render script param set.
#'@details Prepares a params list for the gage_vs_model.Rmd in hydro-tools/USGS.
#'@param watershed_feature a valid RomFeature for the watershed of interest
#'@param gage_info a dataframe returned from dataRetrieval::readNWISsite(gageid)
#'@param model_runid a model run id/scenario
usgs_calib_rarray <- function (riverseg_json, gage_info, model_runid) {
  # set up render array
  da <- watershed_model_da(riverseg_json)
  params = list(
    doc_title = "USGS Gage vs VAHydro Model",
    model_output_file=FALSE,
    runid = model_runid,
    gageid = gage_info$site_no,
    model_da = da,
    elid = riverseg_json$om_element_connection$value
  )
  return(params)
}