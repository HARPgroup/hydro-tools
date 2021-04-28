###################################################################################################### 
# LOAD FILES
######################################################################################################
site <- "http://deq2.bse.vt.edu/d.dh/"
  
basepath <- "/var/www/R/"
source(paste(basepath,"config.local.private",sep = '/'))

#LOAD MAP LAYERS
source(paste(github_location,"/HARParchive/HARP-2020-2021/Cumulative Impact River Mile/CIA_maps.R",sep = '/'))
if(!exists("baselayers")) {baselayers <- load_MapLayers(site = site)} #Load map layers if they're not already loaded in the RStudio environment

source(paste(hydro_tools,"GIS_functions/base.layers.R",sep = '/'))
source(paste(hydro_tools,"GIS_functions/base.map.R",sep = '/'))
###################################################################################################### 
# GENERATE MAP
######################################################################################################

# BASEMAP EXAMPLE ##################################################################
# DEFAULT MAP EXTENT IS STATEWIDE
baselayers.gg <- base.layers(baselayers)

source(paste(hydro_tools,"GIS_functions/base.map.R",sep = '/'))
basemap.obj <- base.map(baselayers.gg)
ggsave(plot = basemap.obj, file = paste0(export_path, "tables_maps/Xfigures/","basemap.png",sep = ""), width=6.5, height=4.95)
####################################################################################

# LAYER OVERLAY EXAMPLE ############################################################
mb.gg <- baselayers.gg[[which(names(baselayers.gg) == "mb.gg")]]
minorbasin_layer <- geom_polygon(data = mb.gg,aes(x = long, y = lat, group = group),color="black", fill = NA,lwd=0.8)
minorbasin_map <- basemap.obj + minorbasin_layer

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
minorbasin_map <- ggdraw(minorbasin_map)+deqlogo
ggsave(plot = minorbasin_map, file = paste0(export_path, "tables_maps/Xfigures/","minorbasin_map.png",sep = ""), width=6.5, height=4.95)
######################################################################################################
######################################################################################################
