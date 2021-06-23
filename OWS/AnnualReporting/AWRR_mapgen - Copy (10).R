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
# 
# sw_features <- read.csv(paste(site,"monitoring-stations-sw-export",sep=""))
# gw_features <- read.csv(paste(site,"monitoring-stations-gw-export",sep=""))
# 
# sw_intake_layer <- geom_point(data = sw_features,aes(x = longitude, y = latitude, color="aliceblue"),size=2, shape=17, show.legend = TRUE)
# gw_well_layer <- geom_point(data = gw_features,aes(x = longitude, y = latitude, color="antiquewhite"),size=2, show.legend = TRUE)
# 
# monitoring_map <- basemap.obj + sw_intake_layer + gw_well_layer + theme(legend.position = c(0.12, 0.9)) +
# 
#   scale_color_manual("Legend", values=c("blue","brown4"),
#                                labels=c("Streamflow Gage","Observation Well")) +
#   
#   guides(colour = guide_legend(override.aes = list(size = c(2, 2),
#                                                    shape = c(17, 19))))
# 
# deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
# monitoring_map_draw <- ggdraw(monitoring_map)+deqlogo
# ggsave(plot = monitoring_map_draw, file = paste0(export_path, "tables_maps/Xfigures/","monitoring_map.png",sep = ""), width=6.5, height=4.95)

######################################################################################################
######################################################################################################
# DROUGHT REGIONS MAP
# fips.gg <- baselayers.gg[[which(names(baselayers.gg) == "fips.gg")]]
# fip_layer <- geom_polygon(data = fips.gg,aes(x = long, y = lat, group = group), color="black", fill = "gray",alpha=0.5,lwd=0.5,na.rm=TRUE)

# fips_map <- basemap.obj + fip_layer
# ggsave(plot = fips_map, file = paste0(export_path, "tables_maps/Xfigures/","fips_map.png",sep = ""), width=6.5, height=4.95)


######################################################################################################
#Region test
# region_bs <- paste('SELECT *
#               FROM "fips.gg"
#               WHERE fips_code IN (51191,51167,51169,51173,51520,51185,51720,51105,51027,51051,51195)
#                    ',sep="")
# region_bs <- sqldf(region_bs)
# 
# 
# region_ch <- paste('SELECT *
#               FROM "fips.gg"
#               WHERE fips_code IN (51025,51081,51053,51620,51111,51135,51149,51175,51181,51183,51595)
#                    ',sep="")
# region_ch <- sqldf(region_ch)


# region_bs <- paste('SELECT *
#               FROM "fips.gg"
#               WHERE fips_name IN (51191,51167,51169,51173,51520,51185,51720,51105,51027,51051,51195) AND
#               fips_name = "Lee"
#                    ',sep="")
# region_bs <- sqldf(region_bs)

# region_bs <- paste('SELECT *
#               FROM "fips.gg"
#               WHERE fips_name IN ("Lee","Bristol","Wise")
#                 ',sep="")
# region_bs <- sqldf(region_bs)


# bs_centroid <- geom_point(data = region_bs,aes(x = fips_longitude, y = fips_latitude, color="aliceblue"),size=2, show.legend = FALSE)
# 
# bs_layer_2 <- geom_polygon(data = region_bs,aes(x = long, y = lat, group = group), 
#                             color="blue", fill = "green",lwd=0.5,na.rm=TRUE, show.legend = TRUE)




# group_sf<- st_as_sf(region_bs, wkt = 'fips_geom')
# geom4 <- geom_sf(data = group_sf,aes(geometry = fips_geom,fill = 'antiquewhite3'), inherit.aes = FALSE, show.legend = "Drought Evaluation Regions")
# 
# ch_sf<- st_as_sf(region_ch, wkt = 'fips_geom')
# ch_layer <- geom_sf(data = ch_sf,aes(geometry = fips_geom,fill = 'antiquewhite4'), inherit.aes = FALSE, show.legend = "Drought Evaluation Regions")
# 
# 
# #fips_map <- basemap.obj + fip_layer + geom4 + bs_centroid +
# fips_map <- basemap.obj + fip_layer + geom4 + 
#                                       ch_layer +
# #fips_map <- basemap.obj + fip_layer + bs_layer_2 + bs_centroid +
# # fips_map <- basemap.obj + fip_layer + bs.gg #+
#   #theme(plot.margin = unit(c(-0.8,0.2,0.1,0.1), "cm"))
#   
#   theme(legend.position = c(0.2, 0.9)) +
#   # scale_color_manual("Legend", values=c("blue"),
#   #                                 labels=c("Big Sandy")) #+
# 
#   scale_fill_manual(name = "Drought Evaluation Regions", values = c("khaki1","slateblue2"),
#                     labels=c("Big Sandy","Chowan"),
#                      guide = guide_legend(override.aes = list(shape = c(20,20)))) #+
#   # scale_fill_manual(name = "circles", values = cols.fill,
#   #                   guide = guide_legend(override.aes = list(shape = c(20, 20), color = cols.fill, size = 8)))
# 
# 
#   
# ggsave(plot = fips_map, file = paste0(export_path, "tables_maps/Xfigures/","fips_map.png",sep = ""), width=6.5, height=4.95)
# 


######################################################################################################
# 
fips.csv <- baselayers[[which(names(baselayers) == "fips.csv")]]
layer_df <- fips.csv

# rbs <- paste('SELECT *, "blue" AS col
#               FROM "layer.df"
#               WHERE fips_code IN (51191,51167,51169,51173,51520,51185,51720,51105,51027,51051,51195)
#                    ',sep="")
# rbs <- sqldf(rbs)


region <-sqldf(paste('SELECT *,
                          CASE
                            WHEN fips_code IN (51191,51167,51169,51173,51520,51185,51720,51105,51027,51051,51195) THEN "antiquewhite"
                            WHEN fips_code IN (51025,51081,51053,51620,51111,51135,51149,51175,51181,51183,51595) THEN "antiquewhite1"
                            WHEN fips_code IN (51131,51001) THEN "antiquewhite4"
                            ELSE "white"
                          END AS col
                        FROM layer_df 
                        ',sep=""))


 layer.sf <- st_as_sf(region, wkt = 'fips_geom')
 layer.gg <- geom_sf(data = layer.sf,aes(fill = factor(col)),lwd=0.4, inherit.aes = FALSE, show.legend =TRUE)
 

 finalmap.obj <- basemap.obj + layer.gg +
  theme(legend.position = c(0.2, 0.9)#,
        #plot.margin = unit(c(-5,0.2,-5,0.1), "cm")
        #plot.margin=grid::unit(c(0,0,0,0), "mm")
        ) +
   scale_fill_manual(name = "Drought Evaluation Regions",
                     values = c("khaki1","slateblue2","darkolivegreen2","white"),
                     labels = c("Big Sandy","Chowan","Eastern Shore","white")
                     )

 #summary(as.factor(layer.sf$col))
 #unique(layer.sf$col)
 
deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
drought_map_draw <- ggdraw(finalmap.obj)+deqlogo 
ggsave(plot = drought_map_draw, file = paste0(export_path, "tables_maps/Xfigures/","finalmap.obj.png",sep = ""), width=6.5, height=4.95)








