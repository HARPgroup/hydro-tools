library(ggplot2)
library(ggsn)
library(ggmap) #used for get_stamenmap, get_map
library(ggspatial) #annotation_north_arrow()
library(arcpullr)

#FUNCTION DEFAULTS TO STATEWIDE EXTENTS
base.map <- function(baselayers.gg,extent=data.frame(x = c(-84, -75),y = c(35.25, 40.6)),
                     plot_margin = c(-0.5,0.2,-0.5,0.1),
                     plot_zoom = 9,
                     scale_bar = TRUE
){
  
  # LOAD gg-ready MAP LAYERS FROM THE baselayers.gg LIST 
  bb.gg <- baselayers.gg[[which(names(baselayers.gg) == "bb.gg")]]
  states.gg <- baselayers.gg[[which(names(baselayers.gg) == "states.gg")]]
  rivs.gg <- baselayers.gg[[which(names(baselayers.gg) == "rivs.gg")]]
  reservoirs.gg <- baselayers.gg[[which(names(baselayers.gg) == "reservoirs.gg")]]
  
  # tile_layer <- get_map(
  #   location = c(left = extent$x[1],
  #                bottom = extent$y[1],
  #                right = extent$x[2],
  #                top = extent$y[2]),
  #   source = "stamen", zoom = plot_zoom, maptype = "terrain" 
  # ) #BB Changed source to stamen and maptype to terrain in response to OSM API changes
  # base_layer <- ggmap(tile_layer)
  
  ### ADDED IN HARP CODE ###
  
  bbox_points <- data.frame(x = c(-83.38,-75.24), y = c(36.54,39.46))
  map_server <- "https://gismaps.vdem.virginia.gov/arcgis/rest/services"
  # VA LandCover - very sparse, 
  map_layer <- "Download/LandCover_Downloads/MapServer/0"
  
  sf_use_s2(TRUE) ### Had to add this line to stop an error at st_crop using planar coordinates
  
  map_url <- paste(map_server,map_layer,sep ="/")
  mapdata <- get_spatial_layer(map_url)
  mapdata <- st_crop(mapdata, c(xmin= min(bbox_points$x), ymin = min(bbox_points$y), 
                                xmax = max(bbox_points$x), ymax = max(bbox_points$y))) #crop to our extent 
  base_layer <- ggplot() + 
                  geom_sf(data = mapdata)
  
  ### END HARP CODE ###
  
  map <- base_layer +
    #ADD STATE BORDER LAYER
    geom_path(data = states.gg,aes(x = long, y = lat, group = group),lwd=0.5,na.rm=TRUE) +
    
    #ADD RIVERS LAYER
    geom_path(data = rivs.gg, aes(x = long, y = lat, group = group), color="dodgerblue3",lwd=0.4,na.rm=TRUE) +
    # ADD WATERBODIES ###############################################################
  # geom_point(data = WBDF, aes(x = long, y = lat), color="dodgerblue3", size=0.09)+
  # geom_path(data = reservoirs.gg, aes(x = long, y = lat, group = group), color="dodgerblue3",lwd=0.4) +
  # geom_polygon(data = reservoirs.gg, aes(x = long, y = lat, group = group), color="dodgerblue3",lwd=0.4) +
  ###geom_point(data = reservoirs.gg, aes(x = long, y = lat), color="dodgerblue3", size=0.09)+
  #################################################################################
  
    #ADD BORDER LAYER
    geom_polygon(data = bb.gg,aes(x = long, y = lat, group = group), color="black", fill = NA,lwd=0.5,na.rm=TRUE) +
    
    coord_sf(xlim = extent$x, ylim = extent$y, expand = F) +
    # #ADD SCALE BAR
    # ggsn::scalebar(bb.gg, location = 'bottomleft', dist = 100, dist_unit = 'mi',
    #       
    #                transform = TRUE, model = 'WGS84',st.bottom=FALSE,
    #                st.size = 3.5, st.dist = 0.0285,
    #                anchor = c(
    #                  x = (((extent$x[2] - extent$x[1])/2)+extent$x[1])-1.8,
    #                  y = extent$y[1]+(extent$y[1])*0.001
    #                ))+
    
  #ADD NORTH BAR
  annotation_north_arrow(which_north = "grid", location = "tr",
                         height = unit(1, "cm"),
                         width = unit(1, "cm")
  )+
    
    #CUSTOMIZE THEME
    # theme(legend.justification=c(0,1), 
    #       legend.position="none",
    theme(plot.margin = unit(plot_margin, "cm"),
          plot.title = element_text(size=12),
          plot.subtitle = element_text(size=10),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank()) 
  
  if (isTRUE(scale_bar)){
    
    map <- map +
      
      #ADD SCALE BAR
      ggsn::scalebar(bb.gg, location = 'bottomleft', dist = 100, dist_unit = 'mi',
                     
                     transform = TRUE, model = 'WGS84',st.bottom=FALSE,
                     st.size = 3.5, st.dist = 0.0285,
                     anchor = c(
                       x = (((extent$x[2] - extent$x[1])/2)+extent$x[1])-1.8,
                       y = extent$y[1]+(extent$y[1])*0.001
                     ))
  }
  
  ## Trying to limit the frame to only include the bounding box in states.gg
  # map + coord_sf(xlim = c(-83.65,-75.25), ylim = c(35.45,40.4))
  # map + xlim = c(-84,-75)+ylim = c(35.25,40.6)
  
  return(map)
}
