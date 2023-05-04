library(dataRetrieval)
library(nhdplusTools)
library(sf)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggspatial)
library(data.table)
library(sp)
source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/R/om_vahydro_metric_grid.R")

basepath='/var/www/R'
source('/var/www/R/config.R')
ds <- RomDataSource$new("http://deq1.bse.vt.edu/d.dh", rest_uname)
ds$get_token(rest_pw)

# function to generate map gg object (should replace hydro-tools/GIS_functions/mapgen.R)
mapgen <- function(start_point = data.frame(lat = 37.2863888889, lon = -80.0758333333, label = "Intake"),
                   points = data.frame(lat=double(),lon=double(),label=character()),
                   segswhere = "hydrocode NOT LIKE '%0000_0000'") {

  ######################################################################
  # process points layer
  point_layer =  st_point(c(points$lon[1], points$lat[1]))
  if (length(points[,1]) > 1) {
    for (i in 2:length(points[,1])) {
      point <- points[i,]
      point = st_point(c(point$lon[1], point$lat[1]))
      point_layer = c(point_layer, point)
    }
  }
  point_layer = st_sfc(point_layer)
  st_crs(point_layer) <- 4326
  points_labels <- as.data.frame(sf::st_coordinates(point_layer))
  points_labels$NAME <- points$label
  
  ######################################################################
  # process intake point
  start_point_layer <- st_sf(id = 1, geom = st_sfc(st_point(c(start_point$lon, start_point$lat)), crs = 4326))
  start_point_labels <- as.data.frame(sf::st_coordinates(start_point_layer))
  start_point_labels$NAME <- start_point$label
  
  sf_use_s2(FALSE) # switch off Spherical geometry (s2) 
  domain <- st_buffer(st_as_sfc(st_bbox(start_point_layer)), .2)
  nhd  <- plot_nhdplus(bbox = st_bbox(domain), actually_plot = FALSE)
  
  sf_bbox <- st_bbox(nhd$flowline)
  ggmap_bbox <- setNames(sf_bbox, c("left", "bottom", "right", "top"))
  basemap_toner <- get_map(source = "stamen", maptype = "toner", location = ggmap_bbox, zoom = 12)
  toner_map <- ggmap(basemap_toner)
  
  ######################################################################
  # process rseg layer
  scenario <- c("vahydro-1.0","runid_11")
  rsegs_sp <- model_geoprocessor(ds,scenario,segswhere)
  rsegs_sf <- st_as_sf(rsegs_sp)
  st_crs(rsegs_sf) <- 4326 
  
  rsegs_centroids <- rgeos::gCentroid(rsegs_sp,byid=TRUE)
  rsegs_labels <- as.data.frame(sf::st_coordinates(st_as_sf(rsegs_centroids)))
  rsegs_labels$NAME <- rsegs_sf$riverseg
  
  ######################################################################
  # generate map gg object
  map_gg <- toner_map + 
    geom_sf(data = rsegs_sf, inherit.aes = FALSE, color = "darkgoldenrod4", fill = NA, size = 2) +
    geom_sf(data = nhd$flowline,inherit.aes = FALSE,color = "blue", fill = NA, size = 0.5) +
    geom_sf(data = nhd$network_wtbd,inherit.aes = FALSE,color = "blue", fill = NA, size = 1) +
    geom_sf(data = nhd$off_network_wtbd,inherit.aes = FALSE,color = "blue", fill = NA, size = 1) +
    # geom_sf(data = nhd$catchment,inherit.aes = FALSE,color = "blue", fill = NA, size = 1) +
    geom_sf(data = start_point_layer, inherit.aes = FALSE, color = "black", size = 10, pch =18) +
    geom_sf(data = point_layer, inherit.aes = FALSE, color = "white", fill = "black", size = 10, pch = 21) +
    theme(text = element_text(size = 30),axis.title.x=element_blank(),axis.title.y=element_blank()) +
    
    # plot labels
    geom_text(data = rsegs_labels, aes(X, Y, label = NAME), colour = "darkgoldenrod4", size = 8) +
    geom_label(data = start_point_labels, aes(X, Y, label = NAME), colour = "black", size = 10, nudge_x = -0.019, nudge_y = 0.006) +
    geom_label(data = points_labels, aes(X, Y, label = NAME), colour = "black", size = 10, nudge_x = -0.033, nudge_y = 0.005) +
    
    # scalebar
    ggsn::scalebar(nhd$flowline, location = 'bottomleft', dist = 2, dist_unit = 'mi',transform = TRUE, model = 'WGS84',st.bottom=FALSE, st.size=12) +
    
    # north arrow
    ggspatial::annotation_north_arrow(which_north = "grid", location = "tr",height = unit(4, "cm"),width = unit(3, "cm"), style = north_arrow_orienteering(text_size = 20))
  
  return(map_gg)
}

# function to retrieve & format model segment metric & geometry data
model_geoprocessor <- function(ds,scenario_info,segswhere) {
  
  model_version <- scenario_info[1]
  runid <- scenario_info[2]
  
  # retrieve segments & metric data
  model_data <- data.frame(
    'model_version' = c(model_version),
    'runid' = c(runid),
    'metric' = c('Qout'),
    'runlabel' = c('Qout')
  )
  model_data <- om_vahydro_metric_grid(metric, model_data, ds = ds)
  
  watersheds <- sqldf(paste0("
  SELECT *
  FROM model_data
  WHERE ",segswhere,";
  "))
  #####################################################################
  # retrieve & format geometry data
  watershed_feature <- RomFeature$new(ds, list(hydroid = watersheds$featureid[1]), TRUE)
  watershed_wkt <- watershed_feature$geom
  polygons_sp <- sp::SpatialPolygonsDataFrame(readWKT(watershed_wkt), data=data.frame(hydrocode=watersheds$hydrocode[1],riverseg=watersheds$riverseg[1]))
  
  if (length(watersheds[,1]) > 1){
    #i<-1
    for (i in 2:length(watersheds[,1])){
      print(paste(i," in ",length(watersheds[,1]),sep=""))
      featureid <- watersheds$featureid[i]
      hydrocode <- watersheds$hydrocode[i]
      riverseg <- watersheds$riverseg[i]
      
      watershed_feature <- RomFeature$new(ds, list(hydroid = featureid), TRUE)
      watershed_poly <- sp::SpatialPolygonsDataFrame(readWKT(watershed_feature$geom), data.frame(watershed_feature$hydrocode) )
      watershed_wkt <- watershed_feature$geom
      polygons_sp <- rbind(polygons_sp, sp::SpatialPolygonsDataFrame(readWKT(watershed_wkt), data.frame(hydrocode=hydrocode,riverseg=riverseg)))
    }
  }
  return(polygons_sp)
} 

################################################################################
# Example Generating A Map Using mapgen():

# set up your dataframe of points you want displayed on the map (can be gages, intakes, facilities, anything!)
gage_02054530 <- dataRetrieval::readNWISsite("02054530")
gage_02055000 <- dataRetrieval::readNWISsite("02055000")
points = data.frame(lat=c(37.234062, 37.4144, gage_02054530$dec_lat_va, gage_02055000$dec_lat_va),
                    lon=c(-80.178434,-79.9338, gage_02054530$dec_long_va, gage_02055000$dec_long_va),
                    label=c("SH Roanoke River Intake","Tinker Creek Intake", paste("USGS",gage_02054530$site_no), paste("USGS",gage_02055000$site_no))
)

# execute mapgen() function by supplying a starting_point (i.e. intake location) and your points dataframe
map_gg <- mapgen(start_point = data.frame(lat = 37.286, lon = -80.076, label = "Salem WTP\nRoanoke River Intake"),
                 points = points, 
                 segswhere <- "hydrocode LIKE 'vahydrosw_wshed_OR%'")

# save the map image as png
fpath = "C:/Users/nrf46657/Desktop/tmp_maps/"
fname = paste(fpath,"fig.location_map_test.png",sep="")
ggsave(
  filename = fname,
  plot = map_gg,
  width = 20,
  height = 20
)
