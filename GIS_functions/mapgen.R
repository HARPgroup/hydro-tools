library(dataRetrieval)
library(nhdplusTools)
library(sf)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggspatial)
source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/GIS_functions/model_geoprocessor.R")

# function to generate map gg object
mapgen <- function(start_point,points,gageid,segswhere) {
  # process usgs gage layer
  gageinfo <- dataRetrieval::readNWISsite(gageid)
  points <- rbind(data.frame(lat = gageinfo$dec_lat_va, lon = gageinfo$dec_long_va, label = paste("USGS",gageinfo$site_no)),points)
  point_layer =  st_point(c(points$lon[1], points$lat[1]))
  
  ######################################################################
  # process points layer
  for (i in 2:length(points[,1])) {
    point <- points[i,]
    point = st_point(c(point$lon[1], point$lat[1]))
    point_layer = c(point_layer, point)
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
  # basemap_terrain <- get_map(source = "stamen", maptype = "terrain-lines", location = ggmap_bbox, zoom = 11)
  # terrain_map <- ggmap(basemap_terrain)
  
  ######################################################################
  # process rseg layer
  scenario <- c("vahydro-1.0","runid_11")
  rsegs_sp <- model_geoprocessor(scenario,segswhere)
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
    geom_label(data = start_point_labels, aes(X, Y, label = NAME), colour = "black", size = 10, nudge_x = -0.019, nudge_y = 0.006) +
    geom_label(data = points_labels, aes(X, Y, label = NAME), colour = "black", size = 10, nudge_x = -0.033, nudge_y = 0.005) +
    geom_text(data = rsegs_labels, aes(X, Y, label = NAME), colour = "darkgoldenrod4", size = 8) +
    
    # scalebar
    ggsn::scalebar(nhd$flowline, location = 'bottomleft', dist = 2, dist_unit = 'mi',transform = TRUE, model = 'WGS84',st.bottom=FALSE, st.size=12) +
    
    # north arrow
    ggspatial::annotation_north_arrow(which_north = "grid", location = "tr",height = unit(4, "cm"),width = unit(3, "cm"), style = north_arrow_orienteering(text_size = 20))
  
  return(map_gg)
}

# ggrepel label version: 
# library(ggrepel)
# geom_label_repel(data = start_point_coords, aes(X, Y, group = 1, label = NAME), size = 10, fill="white", box.padding =1) +
# geom_label_repel(data = irr_pond_12mg_coords, aes(X, Y, group = 1, label = NAME), size = 10, fill="white", box.padding =1) +
# geom_label_repel(data = irr_pond_07mg_coords, aes(X, Y, group = 1, label = NAME), size = 10, fill="white", box.padding =1) +
# geom_label_repel(data = gage_coords, aes(x = X, y = Y, group = 1, label = NAME), size = 10, fill="white", box.padding =1)