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

rivs.gg <- baselayers.gg[[which(names(baselayers.gg) == "rivs.gg")]]
reservoirs.gg <- baselayers.gg[[which(names(baselayers.gg) == "reservoirs.gg")]]
fips_csv <- baselayers[[which(names(baselayers) == "fips.csv")]]

fips_df <-sqldf(paste('SELECT *,fips_geom AS geom,
                          CASE
                            WHEN fips_code IN (51191,51167,51169,51173,51520,51185,51720,51105,51027,51051,51195) THEN "antiquewhite"
                            WHEN fips_code IN (51025,51081,51053,51620,51111,51135,51149,51175,51181,51183,51595) THEN "antiquewhite1"
                            WHEN fips_code IN (51131,51001) THEN "antiquewhite2"
                            WHEN fips_code IN (51049,51075,51085,51087,51009,51570,51125,51145,51147,51041,51670,51680,51730,51760,51007,51065,51003,51011,51029,51540) THEN "antiquewhite3"
                            WHEN fips_code IN (51021,51197,51035,51063,51071,51121,51155,51640,51077,51750) THEN "antiquewhite4"
                            WHEN fips_code IN (51057,51073,51099,51103,51097,51101,51133,51159,51115,51119,51033,51193) THEN "aquamarine"
                            WHEN fips_code IN (51047,51109,51113,51137,51157,51177,51179,51630,51079) THEN "aquamarine1"
                            WHEN fips_code IN (51013,51059,51061,51610,51153,51600,51107,51683,51685,51510) THEN "aquamarine2"
                            WHEN fips_code IN (51031,51037,51067,51083,51590,51089,51161,51690,51019,51770,51775,51143,51117,51515,51141) THEN "aquamarine3"
                            WHEN fips_code IN (51820,51840,51171,51015,51043,51165,51660,51187,51790,51139,51069) THEN "aquamarine4"
                            WHEN fips_code IN (51093,51550,51800,51810,51710,51740) THEN "azure"
                            WHEN fips_code IN (51045,51163,51678,51530,51005,51091,51580,51023,51017) THEN "azure2"
                            WHEN fips_code IN (51830,51036,51095,51127,51650,51199,51735,51700) THEN "azure3"
                            ELSE "white"
                          END AS col
                        FROM fips_csv 
                        WHERE fips_code NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES


 fips.sf <- st_as_sf(fips_df, wkt = 'geom')
 fips.gg <- geom_sf(data = fips.sf,aes(fill = factor(col)),lwd=0.4, inherit.aes = FALSE, show.legend =TRUE)

 finalmap.obj <- basemap.obj + fips.gg +
                 theme(legend.position = c(0.23, 0.78),
                       legend.title=element_text(size=10),
                       legend.text=element_text(size=8),
                       aspect.ratio = 12.05/16
                       ) +
                 guides(fill=guide_legend(ncol=2))+
                 scale_fill_manual(name = "Drought Evaluation Regions",
                                   values = c("khaki1","slateblue2","darkolivegreen2","blue4","chocolate1",
                                              "darkcyan","darkkhaki","indianred1","aquamarine","lightpink",
                                              "chartreuse4","dodgerblue","bisque1","white"),
                                   labels = c("Big Sandy","Chowan","Eastern Shore","Middle James","New River",
                                              "Northern Coastal Plain","Northern Piedmont","Northern Virginia",
                                              "Roanoke","Shenandoah","Southeast Virginia","Upper James","York James","white")
                                   )+
 
 geom_path(data = rivs.gg, aes(x = long, y = lat, group = group), color="dodgerblue3",lwd=0.4,na.rm=TRUE) +
 geom_point(data = reservoirs.gg, aes(x = long, y = lat), color="dodgerblue3", size=0.09)
   
 #summary(as.factor(layer.sf$col))
 #unique(layer.sf$col)
 
deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
drought_map_draw <- ggdraw(finalmap.obj)+deqlogo 
ggsave(plot = drought_map_draw, file = paste0(export_path, "tables_maps/Xfigures/","finalmap.obj.png",sep = ""), width=6.5, height=4.95)









