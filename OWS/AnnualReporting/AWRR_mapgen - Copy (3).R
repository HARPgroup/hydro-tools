###################################################################################################### 
# LOAD FILES
######################################################################################################
site <- "http://deq2.bse.vt.edu/d.dh/"
  
basepath <- "/var/www/R/"
source(paste(basepath,"config.local.private",sep = '/'))
source(paste(hydro_tools,"GIS_functions/base.layers.R",sep = '/'))
source(paste(hydro_tools,"GIS_functions/base.map.R",sep = '/'))
if(!exists("baselayers")) {baselayers <- load_MapLayers(site = site)} #Load map layers if they're not already 
###################################################################################################### 
# GENERATE MAP
######################################################################################################

# BASEMAP EXAMPLE ##################################################################
# DEFAULT MAP EXTENT IS STATEWIDE
baselayers.gg <- base.layers(baselayers)

source(paste(hydro_tools,"GIS_functions/base.map.R",sep = '/'))
basemap.obj <- base.map(baselayers.gg)
#ggsave(plot = basemap.obj, file = paste0(export_path, "tables_maps/Xfigures/","basemap.png",sep = ""), width=6.5, height=4.95)
####################################################################################

# # LAYER OVERLAY EXAMPLE ############################################################
# mb.gg <- baselayers.gg[[which(names(baselayers.gg) == "mb.gg")]]
# minorbasin_layer <- geom_polygon(data = mb.gg,aes(x = long, y = lat, group = group),color="black", fill = NA,lwd=0.8)
# minorbasin_map <- basemap.obj + minorbasin_layer
# 
# deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
# minorbasin_map <- ggdraw(minorbasin_map)+deqlogo
# ggsave(plot = minorbasin_map, file = paste0(export_path, "tables_maps/Xfigures/","minorbasin_map.png",sep = ""), width=6.5, height=4.95)
######################################################################################################
######################################################################################################

######################################################################################################
######################################################################################################
# MONITORING STATIONS MAP

sw_features <- read.csv(paste(site,"monitoring-stations-sw-export",sep=""))
gw_features <- read.csv(paste(site,"monitoring-stations-gw-export",sep=""))

#sw_intake_layer <- geom_point(data = sw_features,aes(x = longitude, y = latitude, color="Streamflow Gage", shape="Streamflow Gage"),size=2, show.legend = TRUE)
#gw_well_layer <- geom_point(data = gw_features,aes(x = longitude, y = latitude, color="Observation Well", shape="Observation Well"),size=2, show.legend = TRUE)

sw_intake_layer <- geom_point(data = sw_features,aes(x = longitude, y = latitude, color="aliceblue"),size=2, show.legend = TRUE)
gw_well_layer <- geom_point(data = gw_features,aes(x = longitude, y = latitude, color="antiquewhite"),size=2, show.legend = TRUE)


monitoring_map <- basemap.obj + sw_intake_layer + gw_well_layer + 
                  theme(legend.position = c(0.8, 0.2)) +
  #                 scale_color_manual(values = c("brown4","blue")) +
  #                 scale_shape_manual(values=c(19, 17), guide=FALSE) +
  # guides(shape = guide_legend(override.aes = list(shape = c(19, 17)) ),
  #        fill = guide_legend(override.aes = list(fill = c("brown4","blue")) ) )

  
  scale_color_manual(
    "Legend",
    values=c("blue","brown4"),
    labels=c("Streamflow Gage","Observation Well")
  ) +
  
  guides(colour = guide_legend(
    override.aes = list(
      size = c(1, 1),
      linetype = c(0, 1),
      shape = c(17, 19)
    )
  )
  )
  
  
# scale_alpha_manual(name = NULL,
#                    values = c(1, 1),
#                    #breaks = c("Observed", "Fitted"),
#                    guide = guide_legend(override.aes = list(linetype = c(0, 1),
#                                                             shape = c(16, NA),
#                                                             color = "black") )) 


  # annotate("rect", xmin = -80, 
  #                  xmax = -82,
  #                  ymin = 36, 
  #                  ymax = 49,
  #          alpha = .5)
  
  
  #theme(legend.position=c(-80,38)) 


#legend.position = c(.95, .95),
#x = c(-84, -75),y = c(35.25, 40.6)

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
monitoring_map_draw <- ggdraw(monitoring_map)+deqlogo
ggsave(plot = monitoring_map_draw, file = paste0(export_path, "tables_maps/Xfigures/","monitoring_map.png",sep = ""), width=6.5, height=4.95)


######################################################################################################
######################################################################################################
