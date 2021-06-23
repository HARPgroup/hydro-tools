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

# BASEMAP ############################################################################################
baselayers.gg <- base.layers(baselayers)
basemap.obj <- base.map(baselayers.gg)
#ggsave(plot = basemap.obj, file = paste0(export_path, "tables_maps/Xfigures/","basemap.png",sep = ""), width=6.5, height=4.95)
####################################################################################

######################################################################################################
######################################################################################################
# MONITORING STATIONS MAP

sw_features <- read.csv(paste(site,"monitoring-stations-sw-export",sep=""))
gw_features <- read.csv(paste(site,"monitoring-stations-gw-export",sep=""))

sw_intake_layer <- geom_point(data = sw_features,aes(x = longitude, y = latitude, color="aliceblue"),size=2, shape=17, show.legend = TRUE)
gw_well_layer <- geom_point(data = gw_features,aes(x = longitude, y = latitude, color="antiquewhite"),size=2, show.legend = TRUE)

monitoring_map <- basemap.obj + sw_intake_layer + gw_well_layer + theme(legend.position = c(0.12, 0.9)) +

  scale_color_manual("Legend", values=c("blue","brown4"),
                               labels=c("Streamflow Gage","Observation Well")) +
  
  guides(colour = guide_legend(override.aes = list(size = c(2, 2),
                                                   shape = c(17, 19))))

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
monitoring_map_draw <- ggdraw(monitoring_map)+deqlogo
ggsave(plot = monitoring_map_draw, file = paste0(export_path, "tables_maps/Xfigures/","monitoring_map.png",sep = ""), width=6.5, height=4.95)

######################################################################################################
######################################################################################################
# DROUGHT REGIONS MAP
fips.gg <- baselayers.gg[[which(names(baselayers.gg) == "fips.gg")]]
fip_layer <- geom_polygon(data = fips.gg,aes(x = long, y = lat, group = group), color="black", fill = "gray",alpha=0.5,lwd=0.5,na.rm=TRUE)
  
fips_map <- basemap.obj + fip_layer
ggsave(plot = fips_map, file = paste0(export_path, "tables_maps/Xfigures/","fips_map.png",sep = ""), width=6.5, height=4.95)


######################################################################################################
#Region test
region_bs <- paste('SELECT *
              FROM "fips.gg"
              WHERE fips_code IN (51191,51167,51169,51173,51520,51185,51720,51105,51027,51051,51195)',sep="")
region_bs <- sqldf(region_bs)



region_bs <- paste('SELECT *
              FROM "fips.gg"
              WHERE fips_code IN (51191,51167,51169,51173,51520,51185,51720,51105,51027,51051,51195)
              GROUP BY fips_name     
                   ',sep="")
region_bs <- sqldf(region_bs)


#colnames(fips.gg)

# region_bs <- paste('SELECT *
#               FROM "fips.gg"
#               WHERE fips_name IN ("Bristol")',sep="")
# region_bs <- sqldf(region_bs)
bs_layer <- geom_polygon(data = region_bs,aes(x = long, y = lat, group = group), color="black", fill = "yellow",alpha=0.5,lwd=0.5,na.rm=TRUE)
bs_centroid <- geom_point(data = region_bs,aes(x = fips_longitude, y = fips_latitude, color="black"),size=2, show.legend = FALSE)
#bs_layer <-  geom_sf(data = region_bs,aes(geometry = fips_geom), color="gray20",fill="black",lwd=0.5, inherit.aes = FALSE)


# bs_layer.sf <- st_as_sf(region_bs, wkt = 'fips_geom')
# bs_layer <- geom_sf(data = bs_layer.sf,aes(geometry = fips_geom),color = 'black',lwd=0.4)


#layer.df <- data.frame(name = "EXAMPLE_NAME",geom = layer_wkt)
layer.sf <- st_as_sf(region_bs, wkt = 'fips_geom')
layer.gg <- geom_sf(data = layer.sf,aes(geometry = fips_geom),color = 'blue',lwd=0.4, inherit.aes = FALSE)




#fips_map <- basemap.obj + bs_layer
#fips_map <- basemap.obj + fip_layer + bs_centroid + bs_layer
fips_map <- basemap.obj + layer.gg
ggsave(plot = fips_map, file = paste0(export_path, "tables_maps/Xfigures/","fips_map.png",sep = ""), width=6.5, height=4.95)
