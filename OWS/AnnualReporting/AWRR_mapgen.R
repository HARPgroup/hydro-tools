###################################################################################################### 
# LOAD FILES
######################################################################################################
library("dataRetrieval")
syear = 2016
eyear = 2020
color_list <- sort(colors())

options(scipen=9999)

#site <- "http://deq2.bse.vt.edu/d.dh/"
site <- "https://deq1.bse.vt.edu/d.dh/"

basepath <- "/var/www/R/"
source(paste(basepath,"config.local.private",sep = '/'))
source(paste(hydro_tools,"GIS_functions/base.layers.R",sep = '/'))
source(paste(hydro_tools,"GIS_functions/base.map.R",sep = '/'))
if(!exists("baselayers")) {baselayers <- load_MapLayers(site = site)} #Load map layers if they're not already 
#FINAL MAP VERSIONS SAVE HERE
folder <- "U:/OWS/foundation_datasets/awrr/2021/"
folder <- paste0("U:/OWS/foundation_datasets/awrr/",eyear+1,"/")
export_path <- paste0("U:/OWS/Report Development/Annual Water Resources Report/October ",eyear+1," Report/overleaf/")

###################################################################################################### 
# GENERATE MAP
######################################################################################################

# BASEMAP ############################################################################################
baselayers.gg <- base.layers(baselayers)
basemap.obj <- base.map(baselayers.gg)
#ggsave(plot = basemap.obj, file = paste0(export_path, "tables_maps/Xfigures/","basemap.png",sep = ""), width=6.5, height=4.95)


#LOAD RIVERS AND RESERVOIRS LAYERS
rivs.gg <- baselayers.gg[[which(names(baselayers.gg) == "rivs.gg")]]
rivs.gg <- geom_path(data = rivs.gg, aes(x = long, y = lat, group = group), color="dodgerblue3",lwd=0.4,na.rm=TRUE)
res_csv <- baselayers[[which(names(baselayers) == "MajorReservoirs.csv")]]
res.sf <- st_as_sf(res_csv, wkt = 'geom')
res.gg <- geom_sf(data = res.sf,color="dodgerblue3",lwd=0.4, inherit.aes = FALSE, show.legend =FALSE)

#LOAD FIPS LAYER
fips_csv <- baselayers[[which(names(baselayers) == "fips.csv")]]

####################################################################################

######################################################################################################
### MONITORING STATIONS MAP ##########################################################################

# ##FROM VAHYDRO:
# sw_features <- read.csv(paste(site,"monitoring-stations-sw-export",sep=""))
# gw_features <- read.csv(paste(site,"monitoring-stations-gw-export",sep=""))
# 
# sw.gg <- geom_point(data = sw_features,aes(x = longitude, y = latitude, color="aliceblue"),size=2, shape=17, show.legend = TRUE)
# gw.gg <- geom_point(data = gw_features,aes(x = longitude, y = latitude, color="antiquewhite"),size=2, show.legend = TRUE)

###FROM NWIS:
#CURRENT/ACTIVE SURFACE WATER GAGES (Stream, Tidal, Lake, Canal)
sw_features <- whatNWISsites(stateCd = "VA", 
                             parameterCd = "00060",
                             siteType = c("ST","ST-TS","LK","ST-CA"),
                             siteStatus = "active")
sw.sf <- st_as_sf(sw_features, coords = c("dec_long_va", "dec_lat_va"),crs = 4326)
sw.sf %>% st_transform(crs=4326)
sw.sf$ms_type <- "SW"

#CURRENT/ACTIVE GROUNDWATER WELLS
gw_features <- whatNWISsites(stateCd = "VA", 
                             parameterCd = "72019",
                             siteStatus = "active")
gw.sf <- st_as_sf(gw_features, coords = c("dec_long_va", "dec_lat_va"),crs = 4326)
gw.sf %>% st_transform(crs=4326)
gw.sf$ms_type <- "GW"

#COMBINE GAGES AND WELLS INTO SINGLE LAYER
ms.sf <- rbind(sw.sf, gw.sf)
ms.gg <- geom_sf(data = ms.sf,aes(color=ms_type, shape = ms_type),size=1.4, inherit.aes = FALSE, show.legend =TRUE)

#MAP
monitoring_map <- basemap.obj + ms.gg +
                  theme(legend.position = c(0.167, 0.89),
                        legend.title=element_text(size=10),
                        legend.text=element_text(size=8),
                        aspect.ratio = 12.05/16) +
                  guides(colour = guide_legend(override.aes = list(size = c(4, 3)))) +
                  scale_color_manual(name = "Groundwater & Surface Water \n Monitoring Stations", 
                                     values=c("brown4", "blue"),
                                     labels=c("Observation Well", "Streamflow Gage")) +
                  scale_shape_manual(name = "Groundwater & Surface Water \n Monitoring Stations", 
                                     labels=c("Observation Well", "Streamflow Gage"),
                                     values = c(19, 17))

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
monitoring_map_draw <- ggdraw(monitoring_map)+deqlogo
#ggsave(plot = monitoring_map_draw, file = paste0(export_path, "/awrr/2021/","MonitoringStationsMap.pdf",sep = ""), width=6.5, height=4.95) #Working map saves here
ggsave(plot = drought_map_draw, file = paste0(export_path, "MonitoringStationsMap.pdf",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE


######################################################################################################
### # DROUGHT REGIONS MAP ############################################################################

fips_df <-sqldf(paste('SELECT *,fips_geom AS geom,
                          CASE
                            WHEN fips_code IN (51191,51167,51169,51173,51520,51185,51720,51105,51027,51051,51195) THEN "',color_list[1],'"
                            WHEN fips_code IN (51025,51081,51053,51620,51111,51135,51149,51175,51181,51183,51595) THEN "',color_list[2],'"
                            WHEN fips_code IN (51131,51001) THEN "',color_list[3],'"
                            WHEN fips_code IN (51049,51075,51085,51087,51009,51570,51125,51145,51147,51041,51670,51680,51730,51760,51007,51065,51003,51011,51029,51540) THEN "',color_list[4],'"
                            WHEN fips_code IN (51021,51197,51035,51063,51071,51121,51155,51640,51077,51750) THEN "',color_list[5],'"
                            WHEN fips_code IN (51057,51073,51099,51103,51097,51101,51133,51159,51115,51119,51033,51193) THEN "',color_list[6],'"
                            WHEN fips_code IN (51047,51109,51113,51137,51157,51177,51179,51630,51079) THEN "',color_list[7],'"
                            WHEN fips_code IN (51013,51059,51061,51610,51153,51600,51107,51683,51685,51510) THEN "',color_list[8],'"
                            WHEN fips_code IN (51031,51037,51067,51083,51590,51089,51161,51690,51019,51770,51775,51143,51117,51515,51141) THEN "',color_list[9],'"
                            WHEN fips_code IN (51820,51840,51171,51015,51043,51165,51660,51187,51790,51139,51069) THEN "',color_list[10],'"
                            WHEN fips_code IN (51093,51550,51800,51810,51710,51740) THEN "',color_list[11],'"
                            WHEN fips_code IN (51045,51163,51678,51530,51005,51091,51580,51023,51017) THEN "',color_list[12],'"
                            WHEN fips_code IN (51830,51036,51095,51127,51650,51199,51735,51700) THEN "',color_list[13],'"
                            ELSE "white"
                          END AS col
                        FROM fips_csv 
                        WHERE fips_code NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES

 fips.sf <- st_as_sf(fips_df, wkt = 'geom')
 fips.gg <- geom_sf(data = fips.sf,aes(fill = factor(col)),lwd=0.4, inherit.aes = FALSE, show.legend =TRUE)

 finalmap.obj <- basemap.obj + fips.gg +
                 theme(legend.position = c(0.23, 0.782),
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
                rivs.gg +
                res.gg

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
drought_map_draw <- ggdraw(finalmap.obj)+deqlogo 
#ggsave(plot = drought_map_draw, file = paste0(export_path, "tables_maps/Xfigures/","finalmap.obj.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = drought_map_draw, file = paste0(export_path, "DroughtRegions.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# GROUNDWATER WITHDRAWAL BY LOCALITY  #######################################################
gw_locality <- read.csv(paste("U:/OWS/foundation_datasets/awrr/",eyear,"/ByLocality.csv",sep=""))

fips_df <-sqldf(paste('SELECT a.*, a.fips_geom AS geom, b."GW.Withdrawal",
                          CASE
                            WHEN b."GW.Withdrawal" < 1 THEN "',color_list[1],'"
                            WHEN b."GW.Withdrawal" BETWEEN 1 AND 2.5 THEN "',color_list[2],'"
                            WHEN b."GW.Withdrawal" BETWEEN 2.5 AND 5 THEN "',color_list[3],'"
                            WHEN b."GW.Withdrawal" BETWEEN 5 AND 10 THEN "',color_list[4],'"
                            WHEN b."GW.Withdrawal" > 10 THEN "',color_list[5],'"
                            ELSE "white"
                          END AS col
                        FROM fips_csv AS a
                        LEFT JOIN gw_locality AS b
                        ON a.fips_code = b.FIPS_CODE
                        WHERE a.fips_code NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES

fips.sf <- st_as_sf(fips_df, wkt = 'geom')
fips.gg <- geom_sf(data = fips.sf,aes(fill = factor(col)),lwd=0.4, inherit.aes = FALSE, show.legend =TRUE)

finalmap.obj <- basemap.obj + fips.gg +
  # ggtitle(paste0(eyear," Groundwater Withdrawal by Locality")) +
  # theme(plot.title = element_text(vjust = - 10, hjust = .3)
  theme(legend.position = c(0.20, 0.833),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        aspect.ratio = 12.05/16
  ) +
  #guides(fill=guide_legend(ncol=2))+
  scale_fill_manual(name = paste0(eyear," Groundwater Withdrawal (MGD)"),
                    values = c("#FFFF80","#71EB2F","#3DB868","#216E9E","#0C1078","white"),
                    labels = c("< 1.0","1.0 to 2.5","2.5 to 5.0","5.0 to 10.0","> 10.0","white")
  ) +
  rivs.gg +
  res.gg

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
gw_locality_map_draw <- ggdraw(finalmap.obj)+deqlogo 
#ggsave(plot = gw_locality_map_draw, file = paste0(export_path, "awrr/2021/","map_gw_locality.png",sep = ""), width=6.5, height=4.95)  #Working map saves here
ggsave(plot = gw_locality_map_draw, file = paste0(export_path, "Locality_Groundwater_Map.png",sep = ""), width=6.5, height=4.95)  #FINAL MAP SAVES HERE
#############################################################################################
# SURFACE WATER WITHDRAWAL BY LOCALITY  #######################################################
gw_locality <- read.csv(paste("U:/OWS/foundation_datasets/awrr/",eyear,"/ByLocality.csv",sep=""))

fips_df <-sqldf(paste('SELECT a.*, a.fips_geom AS geom, b."SW.Withdrawal",
                          CASE
                            WHEN b."SW.Withdrawal" < 1 THEN "',color_list[1],'"
                            WHEN b."SW.Withdrawal" BETWEEN 1 AND 5 THEN "',color_list[2],'"
                            WHEN b."SW.Withdrawal" BETWEEN 5 AND 25 THEN "',color_list[3],'"
                            WHEN b."SW.Withdrawal" BETWEEN 25 AND 50 THEN "',color_list[4],'"
                            WHEN b."SW.Withdrawal" > 50 THEN "',color_list[5],'"
                            ELSE "white"
                          END AS col
                        FROM fips_csv AS a
                        LEFT JOIN gw_locality AS b
                        ON a.fips_code = b.FIPS_CODE
                        WHERE a.fips_code NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES

fips.sf <- st_as_sf(fips_df, wkt = 'geom')
fips.gg <- geom_sf(data = fips.sf,aes(fill = factor(col)),lwd=0.4, inherit.aes = FALSE, show.legend =TRUE)

finalmap.obj <- basemap.obj + fips.gg +
  # ggtitle(paste0(eyear," Groundwater Withdrawal by Locality")) +
  # theme(plot.title = element_text(vjust = - 10, hjust = .3)
  theme(legend.position = c(0.206, 0.833),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        aspect.ratio = 12.05/16
  ) +
  #guides(fill=guide_legend(ncol=2))+
  scale_fill_manual(name = paste0(eyear," Surface Water Withdrawal (MGD)"),
                    values = c("#FFFF80","#71EB2F","#3DB868","#216E9E","#0C1078","white"),
                    labels = c("< 1.0","1.0 to 5.0","5.0 to 25.0","25.0 to 50.0","> 50.0","white")
  ) +
  rivs.gg +
  res.gg

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
gw_locality_map_draw <- ggdraw(finalmap.obj)+deqlogo 
#ggsave(plot = gw_locality_map_draw, file = paste0(export_path, "awrr/2021/","map_sw_locality.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = drought_map_draw, file = paste0(export_path, "Locality_SurfaceWater_Map.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE


#############################################################################################
# TOTAL WITHDRAWAL BY LOCALITY  #######################################################
gw_locality <- read.csv(paste("U:/OWS/foundation_datasets/awrr/",eyear,"/ByLocality.csv",sep=""))

fips_df <-sqldf(paste('SELECT a.*, a.fips_geom AS geom, b."Total.Withdrawal",
                          CASE
                            WHEN b."Total.Withdrawal" < 1 THEN "',color_list[1],'"
                            WHEN b."Total.Withdrawal" BETWEEN 1 AND 5 THEN "',color_list[2],'"
                            WHEN b."Total.Withdrawal" BETWEEN 5 AND 25 THEN "',color_list[3],'"
                            WHEN b."Total.Withdrawal" BETWEEN 25 AND 50 THEN "',color_list[4],'"
                            WHEN b."Total.Withdrawal" > 50 THEN "',color_list[5],'"
                            ELSE "white"
                          END AS col
                        FROM fips_csv AS a
                        LEFT JOIN gw_locality AS b
                        ON a.fips_code = b.FIPS_CODE
                        WHERE a.fips_code NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES

fips.sf <- st_as_sf(fips_df, wkt = 'geom')
fips.gg <- geom_sf(data = fips.sf,aes(fill = factor(col)),lwd=0.4, inherit.aes = FALSE, show.legend =TRUE)

finalmap.obj <- basemap.obj + fips.gg +
  # ggtitle(paste0(eyear," Groundwater Withdrawal by Locality")) +
  # theme(plot.title = element_text(vjust = - 10, hjust = .3)
  theme(legend.position = c(0.16, 0.833),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        aspect.ratio = 12.05/16
  ) +
  #guides(fill=guide_legend(ncol=2))+
  scale_fill_manual(name = paste0(eyear," Total Withdrawal (MGD)"),
                    values = c("#FFFF80","#71EB2F","#3DB868","#216E9E","#0C1078","white"),
                    labels = c("< 1.0","1.0 to 5.0","5.0 to 25.0","25.0 to 50.0","> 50.0","white")
  ) +
  rivs.gg +
  res.gg

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
gw_locality_map_draw <- ggdraw(finalmap.obj)+deqlogo 
#ggsave(plot = gw_locality_map_draw, file = paste0(export_path, "awrr/2021/","map_total_locality.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = drought_map_draw, file = paste0(export_path, "Locality_Total_Map.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE


#############################################################################################
# Agriculture (Non-Irrigation) Water Withdrawals by Withdrawal Point Location################

mp_point <- read.csv(paste("U:/OWS/foundation_datasets/awrr/",eyear+1,"/mp_all_",syear,"-",eyear,".csv",sep=""))

#try natural breaks or size bins nex year
mp_df <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "mgd" < 0.05 THEN 2
                            WHEN "mgd" BETWEEN 0.05 AND 0.5 THEN 3
                            WHEN "mgd" BETWEEN 0.5 AND 1 THEN 4
                            WHEN "mgd" BETWEEN 1 AND 5 THEN 5
                            WHEN "mgd" > 5 THEN 6
                            ELSE 0
                          END AS point_size
                        FROM mp_point AS a
                        WHERE Year = ',eyear,'
                        AND Use_Type = "agriculture"
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES


mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, size = factor(point_size)), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)

fips_df <- sqldf('SELECT *
                 FROM fips_csv
                 WHERE fips_code NOT LIKE "3%"') #select all in fips_csv and take out NC fips codes

fips.sf <- st_as_sf(fips_df, wkt = 'fips_geom')
fips.gg <- geom_sf(data = fips.sf,colour = "black",fill = NA, lwd=0.3, inherit.aes = FALSE, show.legend = FALSE)

ag_map <- basemap.obj + fips.gg + rivs.gg + res.gg + mp.gg +
  theme(legend.position = c(0.179, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        aspect.ratio = 12.05/16
  ) +
  scale_size_manual(name=paste0(eyear," Agriculture (Non-Irrigation) \n Water Withdrawals (MGD)"), values=c(2,3,4,5,6,0),
                     labels=c("< 0.05","0.05 - 0.5","0.5 - 1.0","1.0 - 5.0","> 5.0"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
ag_map_draw <- ggdraw(ag_map)+deqlogo

#ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_ag_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = drought_map_draw, file = paste0(export_path, "Agriculture_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# Irrigation Water Withdrawals by Withdrawal Point Location################

mp_point <- read.csv(paste("U:/OWS/foundation_datasets/awrr/",eyear+1,"/mp_all_",syear,"-",eyear,".csv",sep=""))

#try natural breaks or size bins next year
mp_df <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "mgd" < 0.05 THEN 2
                            WHEN "mgd" BETWEEN 0.05 AND 0.10 THEN 3
                            WHEN "mgd" BETWEEN 0.10 AND 0.25 THEN 4
                            WHEN "mgd" BETWEEN 0.25 AND 0.50 THEN 5
                            WHEN "mgd" > 0.50 THEN 6
                            ELSE 0
                          END AS point_size
                        FROM mp_point AS a
                        WHERE Year = ',eyear,'
                        AND Use_Type = "irrigation"
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES

mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, size = factor(point_size)), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)

fips_df <- sqldf('SELECT *
                 FROM fips_csv
                 WHERE fips_code NOT LIKE "3%"') #select all in fips_csv and take out NC fips codes

fips.sf <- st_as_sf(fips_df, wkt = 'fips_geom')
fips.gg <- geom_sf(data = fips.sf,colour = "black",fill = NA, lwd=0.3, inherit.aes = FALSE, show.legend = FALSE)

ag_map <- basemap.obj + fips.gg + rivs.gg + res.gg + mp.gg +
  theme(legend.position = c(0.153, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        aspect.ratio = 12.05/16
  ) +
  scale_size_manual(name=paste0(eyear," Irrigation (Agriculture) \n Water Withdrawals (MGD)"), values=c(2,3,4,5,6,0),
                    labels=c("< 0.05","0.05 - 0.10","0.10 - 0.25","0.25 - 0.50","> 0.50"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
ag_map_draw <- ggdraw(ag_map)+deqlogo

#ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_irr_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = drought_map_draw, file = paste0(export_path, "Irrigation_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# Commercial Water Withdrawals by Withdrawal Point Location################

#mp_point <- read.csv(paste("U:/OWS/foundation_datasets/awrr/",eyear+1,"/mp_all_",syear,"-",eyear,".csv",sep=""))

#try natural breaks or size bins next year
mp_df <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "mgd" < 0.05 THEN 2
                            WHEN "mgd" BETWEEN 0.05 AND 0.10 THEN 3
                            WHEN "mgd" BETWEEN 0.10 AND 0.25 THEN 4
                            WHEN "mgd" BETWEEN 0.25 AND 0.50 THEN 5
                            WHEN "mgd" > 0.50 THEN 6
                            ELSE 0
                          END AS point_size
                        FROM mp_point AS a
                        WHERE Year = ',eyear,'
                        AND Use_Type = "commercial"
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES

mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, size = factor(point_size)), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)

fips_df <- sqldf('SELECT *
                 FROM fips_csv
                 WHERE fips_code NOT LIKE "3%"') #select all in fips_csv and take out NC fips codes

fips.sf <- st_as_sf(fips_df, wkt = 'fips_geom')
fips.gg <- geom_sf(data = fips.sf,colour = "black",fill = NA, lwd=0.3, inherit.aes = FALSE, show.legend = FALSE)

ag_map <- basemap.obj + fips.gg + rivs.gg + res.gg + mp.gg +
  theme(legend.position = c(0.146, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        aspect.ratio = 12.05/16
  ) +
  scale_size_manual(name=paste0(eyear," Commerical \n Water Withdrawals (MGD)"), values=c(2,3,4,5,6,0),
                    labels=c("< 0.05","0.05 - 0.10","0.10 - 0.25","0.25 - 0.50","> 0.50"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
ag_map_draw <- ggdraw(ag_map)+deqlogo

#ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_com_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = drought_map_draw, file = paste0(export_path, "Commerical_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# Mining Water Withdrawals by Withdrawal Point Location################

#mp_point <- read.csv(paste("U:/OWS/foundation_datasets/awrr/",eyear+1,"/mp_all_",syear,"-",eyear,".csv",sep=""))

#try natural breaks or size bins next year
mp_df <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "mgd" < 0.05 THEN 2
                            WHEN "mgd" BETWEEN 0.05 AND 0.5 THEN 3
                            WHEN "mgd" BETWEEN 0.5 AND 1 THEN 4
                            WHEN "mgd" BETWEEN 1 AND 5 THEN 5
                            WHEN "mgd" > 5 THEN 6
                            ELSE 0
                          END AS point_size
                        FROM mp_point AS a
                        WHERE Year = ',eyear,'
                        AND Use_Type = "mining"
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES


mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, size = factor(point_size)), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)

fips_df <- sqldf('SELECT *
                 FROM fips_csv
                 WHERE fips_code NOT LIKE "3%"') #select all in fips_csv and take out NC fips codes

fips.sf <- st_as_sf(fips_df, wkt = 'fips_geom')
fips.gg <- geom_sf(data = fips.sf,colour = "black",fill = NA, lwd=0.3, inherit.aes = FALSE, show.legend = FALSE)

ag_map <- basemap.obj + fips.gg + rivs.gg + res.gg + mp.gg +
  theme(legend.position = c(0.146, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        aspect.ratio = 12.05/16
  ) +
  scale_size_manual(name=paste0(eyear," Mining \n Water Withdrawals (MGD)"), values=c(2,3,4,5,6,0),
                    labels=c("< 0.05","0.05 - 0.5","0.5 - 1.0","1.0 - 5.0","> 5.0"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
ag_map_draw <- ggdraw(ag_map)+deqlogo

#ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_min_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = drought_map_draw, file = paste0(export_path, "Mining_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# Manufacturing Water Withdrawals by Withdrawal Point Location################

mp_point <- read.csv(paste("U:/OWS/foundation_datasets/awrr/",eyear+1,"/mp_all_",syear,"-",eyear,".csv",sep=""))

#try natural breaks or size bins next year
#plotting 0 MGD values because withdrawal points were still reported this year
mp_df <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "mgd" < 0.05 THEN 2
                            WHEN "mgd" BETWEEN 0.05 AND 0.5 THEN 3
                            WHEN "mgd" BETWEEN 0.5 AND 5 THEN 4
                            WHEN "mgd" BETWEEN 5 AND 25 THEN 5
                            WHEN "mgd" > 25 THEN 6
                            ELSE 0
                          END AS point_size
                        FROM mp_point AS a
                        WHERE Year = ',eyear,'
                        AND Use_Type = "manufacturing"
                    AND a.HydroID NOT LIKE "398760"
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES
#Point that was excluded is a permitted Tyson well with HydroID 398760 because Point appears in the ocean, and was 0 MGD in 2020 anyways

mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, size = factor(point_size)), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)

fips_df <- sqldf('SELECT *
                 FROM fips_csv
                 WHERE fips_code NOT LIKE "3%"') #select all in fips_csv and take out NC fips codes

fips.sf <- st_as_sf(fips_df, wkt = 'fips_geom')
fips.gg <- geom_sf(data = fips.sf,colour = "black",fill = NA, lwd=0.3, inherit.aes = FALSE, show.legend = FALSE)

ag_map <- basemap.obj + fips.gg + rivs.gg + res.gg + mp.gg +
  theme(legend.position = c(0.146, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        aspect.ratio = 12.05/16
  ) +
  scale_size_manual(name=paste0(eyear," Manufacturing \n Water Withdrawals (MGD)"), values=c(2,3,4,5,6,0),
                    labels=c("< 0.05","0.05 - 0.5","0.5 - 5","5 - 25","> 25"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
ag_map_draw <- ggdraw(ag_map)+deqlogo

#ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_man_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = drought_map_draw, file = paste0(export_path, "Manufacturing_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE


#############################################################################################
# Public Water Supply Water Withdrawals by Withdrawal Point Location################

#mp_point <- read.csv(paste("U:/OWS/foundation_datasets/awrr/",eyear+1,"/mp_all_",syear,"-",eyear,".csv",sep=""))

#try natural breaks or size bins next year
mp_df <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "mgd" < 0.05 THEN 2
                            WHEN "mgd" BETWEEN 0.05 AND 0.5 THEN 3
                            WHEN "mgd" BETWEEN 0.5 AND 5 THEN 4
                            WHEN "mgd" BETWEEN 5 AND 25 THEN 5
                            WHEN "mgd" > 25 THEN 6
                            ELSE 0
                          END AS point_size
                        FROM mp_point AS a
                        WHERE Year = ',eyear,'
                        AND Use_Type = "municipal"
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE, NC LOCALITIES


mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, size = factor(point_size)), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)

fips_df <- sqldf('SELECT *
                 FROM fips_csv
                 WHERE fips_code NOT LIKE "3%"') #select all in fips_csv and take out NC fips codes

fips.sf <- st_as_sf(fips_df, wkt = 'fips_geom')
fips.gg <- geom_sf(data = fips.sf,colour = "black",fill = NA, lwd=0.3, inherit.aes = FALSE, show.legend = FALSE)

ag_map <- basemap.obj + fips.gg + rivs.gg + res.gg + mp.gg +
  theme(legend.position = c(0.146, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        aspect.ratio = 12.05/16
  ) +
  scale_size_manual(name=paste0(eyear," Public Water Supply \n Water Withdrawals (MGD)"), values=c(2,3,4,5,6,0),
                    labels=c("< 0.05","0.05 - 0.5","0.5 - 5","5 - 25","> 25"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
ag_map_draw <- ggdraw(ag_map)+deqlogo

#ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_pws_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = drought_map_draw, file = paste0(export_path, "PublicWaterSupply_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# Power Generation Water Withdrawals by Withdrawal Point Location################

#five MP with missing lat/lon in the mp_all_wide_power csv, each less than 0.02mgd, each with another facility mp that does show on map, so lat/lon was not corrected this year
mp_point <- read.csv(paste("U:/OWS/foundation_datasets/awrr/",eyear+1,"/mp_all_wide_power_",syear,"-",eyear,".csv",sep=""))


mp_point[is.na(mp_point)] <- 0 # Convert NAs to zeros because NA means 0mgd was reported

mp_point <- sqldf(paste0('SELECT *, "X',eyear,'"/365 AS "',eyear,'mgd"
                         FROM mp_point
                         ')) #convert mgy values into mgd using SQL

#Fossil Power
#order mgd column by descending size so larger mgd points appear behind smaller ones
mp_df_f <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "',eyear,'mgd" < 0.5 THEN 2
                            WHEN "',eyear,'mgd" BETWEEN 0.5 AND 5 THEN 3
                            WHEN "',eyear,'mgd" BETWEEN 5 AND 50 THEN 4
                            WHEN "',eyear,'mgd" BETWEEN 50 AND 500 THEN 5
                            WHEN "',eyear,'mgd" > 500 THEN 6
                            ELSE 0
                          END AS point_size
                        FROM mp_point AS a
                        WHERE Use_Type = "fossilpower"
                    AND a.fips NOT LIKE "3%"
                      ORDER BY "',eyear,'mgd" DESC 
                      ',sep="")) #EXCLUDE NC LOCALITIES

#Nuclear Power
#order mgd column by descending size so larger mgd points appear behind smaller ones
mp_df_n <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "',eyear,'mgd" < 0.5 THEN 2
                            WHEN "',eyear,'mgd" BETWEEN 0.5 AND 5 THEN 3
                            WHEN "',eyear,'mgd" BETWEEN 5 AND 50 THEN 4
                            WHEN "',eyear,'mgd" BETWEEN 50 AND 500 THEN 5
                            WHEN "',eyear,'mgd" > 500 THEN 6
                            ELSE 0
                          END AS point_size
                        FROM mp_point AS a
                        WHERE Use_Type = "nuclearpower"
                    AND a.fips NOT LIKE "3%"
                      ORDER BY "',eyear,'mgd" DESC 
                      ',sep="")) #EXCLUDE, NC LOCALITIES

#adding color category here so fossil and nuclear have a category that can show up in the legend
mp_f.gg <- geom_point(data = mp_df_f,aes(x = lon, y = lat, size = factor(point_size), color = "Fossil Power"), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)
mp_n.gg <- geom_point(data = mp_df_n,aes(x = lon, y = lat, size = factor(point_size), color = "Nuclear Power"), fill="orange2", alpha=0.75, shape=21, show.legend = TRUE)


fips_df <- sqldf('SELECT *
                 FROM fips_csv
                 WHERE fips_code NOT LIKE "3%"') #select all in fips_csv and take out NC fips codes

fips.sf <- st_as_sf(fips_df, wkt = 'fips_geom')
fips.gg <- geom_sf(data = fips.sf,colour = "black",fill = NA, lwd=0.3, inherit.aes = FALSE, show.legend = FALSE)



ag_map <- basemap.obj + fips.gg + rivs.gg + res.gg + mp_f.gg + mp_n.gg + 
  theme(legend.position = c(0.146, 0.755),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        aspect.ratio = 12.05/16,
        legend.spacing.y = unit(0, "cm"),
  ) +
  guides(color = guide_legend(order = 1, override.aes = list(fill = c("#0C1078", "orange2"), size = 4)), #order=1 moves the fossil-nuclear color legend to show first
         size = guide_legend(order = 2, override.aes = list(shape=21, colour="black", fill=NA)) #override.aes overwrites size legend with new specifications
         ) +
  scale_color_manual(name=paste0(eyear," Power Generation \n Water Withdrawals (MGD)"), #show fossil and nuclear in legend using color categories
                     values = c("black", "black"),
                     breaks = c("Fossil Power", "Nuclear Power")) +
  scale_size_manual(name=paste0(name=NULL), values=c(2,3,4,5,6,0),
                    labels=c("< 0.5","0.5 - 5","5 - 50","50 - 500","> 500"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
ag_map_draw <- ggdraw(ag_map)+deqlogo

#ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_pow_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = drought_map_draw, file = paste0(export_path, "Power_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# Surface Water Withdrawal Permitting Activities ############################################

#PULL IN OWS Permit List from local file
mp_permit <- read.csv(paste(folder,"ows_permit_list.csv",sep=""))

#must convert columns with date info to character data type so sqldf can recognize the date format
mp_permit$Permit.Start2 <- as.character(as.Date(mp_permit$Permit.Start, format = "%m/%d/%Y"))

#filter for SW that are currently active or in admin continued status
mp_point_sw <- sqldf('SELECT a.*
                     FROM mp_permit AS a
                     WHERE a."Permit.Program" LIKE "%VWP%"
                     AND a."Status" IN ("active", "expired")')

#filter for New Permit Issuances - bins for point color
mp_df <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "Permit.Start2" >= "2020-01-01" THEN 3
                          ELSE 2
                          END AS point_size
                        FROM mp_point_sw AS a
                    ORDER BY "Permit.Start2" ASC',sep="")) 

  mp.gg <- geom_point(data = mp_df,aes(x = Facility.Longitude, y = Facility.Latitude, size = factor(point_size), fill=factor(point_size)), alpha=0.9, shape=21, show.legend = TRUE)
  
  fips_df <- sqldf('SELECT *
                   FROM fips_csv
                   WHERE fips_code NOT LIKE "3%"') #select all in fips_csv and take out NC fips codes
  
  fips.sf <- st_as_sf(fips_df, wkt = 'fips_geom')
  fips.gg <- geom_sf(data = fips.sf,colour = "black",fill = NA, lwd=0.3, inherit.aes = FALSE, show.legend = FALSE)
  
  permit_map <- basemap.obj + fips.gg + rivs.gg + res.gg + mp.gg +
    theme(legend.position = c(0.268, .9075),
          legend.title=element_text(size=10),
          legend.text=element_text(size=8),
          aspect.ratio = 12.05/16) +
    guides(size = guide_legend(override.aes = list(size = c(2,3))),
           fill = guide_legend(override.aes = list(fill = c("#0C1078", "orange2")))) +
      scale_size_manual(name=paste0(eyear," Surface Water Withdrawal Permitting Activities"), values=c(2,3), labels=c("Active Surface Water Withdrawal Permits", paste0("Issued Since January ",eyear))) +
    scale_fill_manual(name=paste0(eyear," Surface Water Withdrawal Permitting Activities"), values=c("#0C1078", "orange2"),  labels=c("Active Surface Water Withdrawal Permits", paste0("Issued Since January ",eyear))) 
  
  deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
  permit_map_draw <- ggdraw(permit_map)+deqlogo
  
  #ggsave(plot = permit_map_draw, file = paste0(export_path, "/awrr/2021/","VWPermits_AWRR_2020.pdf",sep = ""), width=6.5, height=4.95) #Working map saves here
  ggsave(plot = permit_map_draw, file = paste0(export_path, "VWPermits_AWRR_2020.pdf",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE
#############################################################################################
# Groundwater Withdrawal Permitting Activities ##############################################
  
  #PULL IN OWS Permit List from local file
  mp_permit <- read.csv(paste(folder,"ows_permit_list.csv",sep=""))
  
  #must convert columns with date info to character data type so sqldf can recognize the date format
  mp_permit$Permit.Start2 <- as.character(as.Date(mp_permit$Permit.Start, format = "%m/%d/%Y"))
  
  #filter for SW that are currently active or in admin continued status
  mp_point_gw <- sqldf('SELECT a.*
                     FROM mp_permit AS a
                     WHERE a."Permit.Program" LIKE "%GWP%"
                     AND a."Status" IN ("active", "expired")')
  
  #filter for New Permit Issuances - bins for point color
  mp_df <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "Permit.Start2" >= "2020-01-01" THEN 4
                          ELSE 3
                          END AS point_size
                        FROM mp_point_gw AS a
                    ORDER BY "Permit.Start2" ASC',sep="")) 
  
  mp.gg <- geom_point(data = mp_df,aes(x = Facility.Longitude, y = Facility.Latitude, fill=factor(point_size)), alpha=0.9, size = 2, shape=21, inherit.aes = FALSE, show.legend = TRUE)
  
  #COUNTY LAYER
  fips_df <- sqldf('SELECT *
                   FROM fips_csv
                   WHERE fips_code NOT LIKE "3%"') #select all in fips_csv and take out NC fips codes
  fips.sf <- st_as_sf(fips_df, wkt = 'fips_geom')
  fips.gg <- geom_sf(data = fips.sf,colour = "black",fill = NA, lwd=0.3, inherit.aes = FALSE, show.legend = FALSE)
  
  #GWMA LAYER
  #PULL IN GWMA wkt from Search By HydroID page
  gwma_df <- read.csv(paste(folder,"GWMA_wkt.csv",sep=""))
  gwma_df <- sqldf('SELECT *, CASE
                    WHEN HydroID = 194537
                    THEN 1
                    WHEN HydroID = 441638
                    THEN 2
                    ELSE 0
                    END AS fill_order
                   FROM gwma_df')
  gwma.sf <- st_as_sf(gwma_df, wkt = 'Geometry')
  gwma.gg <- geom_sf(data = gwma.sf,aes(fill = factor(fill_order)),colour = "black", shape = 22, lwd=0.4, alpha = 0.5, inherit.aes = FALSE, show.legend = FALSE)
  
  # MAP
  permit_map <- basemap.obj + gwma.gg + fips.gg + rivs.gg + res.gg + mp.gg +
    theme(legend.position = c(0.264, .8555),
          legend.title=element_text(size=10),
          legend.text=element_text(size=8),
          aspect.ratio = 12.05/16) +
    guides(fill = guide_legend(override.aes = list(fill = c("pink","darkorchid2","#0C1078","orange2"),
                                                   alpha = c(.5,.5,1,1),
                                                   size = c(4,4,3,3),
                                                   shape = c(22,22,21,21)))) +
    scale_fill_manual(name=paste0(eyear," Groundwater Withdrawal Permitting Activities"), 
                      values=c("pink",
                               "darkorchid2",
                               "#0C1078",
                               "orange2"), 
                      labels=c("Eastern Virginia Groundwater Management Area",
                               "Eastern Shore Groundwater Management Area",
                               "Active Groundwater Withdrawal Permits", 
                               paste0("Issued Since January ",eyear))) 
  
  deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
  permit_map_draw <- ggdraw(permit_map)+deqlogo
  
  ggsave(plot = permit_map_draw, file = paste0(export_path, "/awrr/2021/","xGWPermits_AWRR_2020.png",sep = ""), width=6.5, height=4.95) #Working map saves here
  
#############################################################################################
# WSP Regions Map ###########################################################################

#PULL IN WSP Regions WKT from VAHydro
# wsp_regions <- read.csv(paste(site,"region-to-localities-fips-export/all",sep=""))
  
  #PULL IN WSP Region List from local file
  wsp_regions <- read.csv(paste(folder,"ows_wsp_regions_wkt.csv",sep=""))
  wsp_regions$region_name <- as.character(wsp_regions$region_name)
  
   wsp_df <- sqldf('SELECT *  
         FROM wsp_regions
         WHERE geom IS NOT NULL
         AND "FIPS.Code" NOT LIKE "3%"')
  
  wsp.sf <- st_as_sf(wsp_df, wkt = 'geom')
  wsp.gg <- geom_sf(data = wsp.sf,aes(fill = factor(region_name)),lwd=0.4, inherit.aes = FALSE, show.legend =TRUE)
  
  finalmap.obj <- basemap.obj + wsp.gg +
    theme(legend.position = "bottom",
          legend.title=element_text(size=10),
          legend.text=element_text(size=8),
          aspect.ratio = 12.05/16
    ) +
    guides(fill=guide_legend(ncol=2))+
    scale_fill_manual(name = NULL,
                      values = c("#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
                                 "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
                                 "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
                                 "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
                                 "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
                                 "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
                                 "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
                                 "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
                                 "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
                                 "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
                                 "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
                                 "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
                                 "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C"),
                      # Many color options for +10 color ramps - used the following suggestion: User Tatarize suggests in his blog: 5) A set of 64 maximally dissimilar colors:
                      #https://graphicdesign.stackexchange.com/questions/3682/where-can-i-find-a-large-palette-set-of-contrasting-colors-for-coloring-many-d
                      labels = c(unique(wsp_df[c("region_name")]),"white")
    )+
    rivs.gg +
    res.gg
  
  deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.401, y = -0.032) #LEFT BOTTOM LOGO
  wsp_map_draw <- ggdraw(finalmap.obj)+deqlogo 
  #ggsave(plot = wsp_map_draw, file = paste0(export_path, "/awrr/2021/","xxWSP_Planning_Regions_Map.png",sep = ""), width=10, height=15) #Working map saves here
  ggsave(plot = wsp_map_draw, file = paste0(export_path, "/awrr/2021/","WSP_Planning_Regions_Map.pdf",sep = ""), width=10, height=15) #FINAL MAP SAVES HERE

#############################################################################################
# ZOOMED IN EXTENT - Groundwater Withdrawal Permitting Activities ##############################################
  
  #GWMA extent
  extent = data.frame( 
    x = c(-77.7, -75),
    y = c(36, 40.6))
  
  baselayers.gg <- base.layers(baselayers, 
                               extent = data.frame(
                                 x = c(-77.7, -75),
                                 y = c(36, 40.6)))
  basemap.obj <- base.map(baselayers.gg, 
                          extent = data.frame(
                            x = c(-77.7, -75),
                            y = c(36, 40.6)),
                          plot_margin = c(.25,0,.25,0), #top, right, bottom, left
                          plot_zoom = 8,
                          scale_bar = F)
  
  ggsave(plot = basemap.obj, file = paste0(export_path, "/awrr/2021/","basemap.png",sep = ""), width=6.5, height=4.95)
  
  # BOUNDING BOX
  bb=readWKT(paste0("POLYGON((",extent$x[1]," ",extent$y[1],",",extent$x[2]," ",extent$y[1],",",extent$x[2]," ",extent$y[2],",",extent$x[1]," ",extent$y[2],",",extent$x[1]," ",extent$y[1],"))",sep=""))
  bbProjected <- SpatialPolygonsDataFrame(bb,data.frame("id"), match.ID = FALSE)
  bbProjected@data$id <- rownames(bbProjected@data)
  bbPoints <- fortify(bbProjected, region = "id")
  bbDF <- merge(bbPoints, bbProjected@data, by = "id")
  
  bb.gg <- bbDF
  
  #PULL IN OWS Permit List from local file
  mp_permit <- read.csv(paste(folder,"ows_permit_list.csv",sep=""))
  
  #must convert columns with date info to character data type so sqldf can recognize the date format
  mp_permit$Permit.Start2 <- as.character(as.Date(mp_permit$Permit.Start, format = "%m/%d/%Y"))
  
  #filter for SW that are currently active or in admin continued status
  mp_point_gw <- sqldf('SELECT a.*
                     FROM mp_permit AS a
                     WHERE a."Permit.Program" LIKE "%GWP%"
                     AND a."Status" IN ("active", "expired")')
  
  #filter for New Permit Issuances - bins for point color
  mp_df <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "Permit.Start2" >= "2020-01-01" THEN 4
                          ELSE 3
                          END AS point_size
                        FROM mp_point_gw AS a
                    ORDER BY "Permit.Start2" ASC',sep="")) 
  
  mp.gg <- geom_point(data = mp_df,aes(x = Facility.Longitude, y = Facility.Latitude, fill=factor(point_size)), alpha=0.9, size = 2, shape=21, inherit.aes = FALSE, show.legend = TRUE)
  
  #COUNTY LAYER
  fips_df <- sqldf('SELECT *
                   FROM fips_csv
                   WHERE fips_code NOT LIKE "3%"') #select all in fips_csv and take out NC fips codes
  fips.sf <- st_as_sf(fips_df, wkt = 'fips_geom')
  fips.gg <- geom_sf(data = fips.sf,colour = "black",fill = NA, lwd=0.3, inherit.aes = FALSE, show.legend = FALSE)
  
  #GWMA LAYER
  #PULL IN GWMA wkt from Search By HydroID page
  gwma_df <- read.csv(paste(folder,"GWMA_wkt.csv",sep=""))
  gwma_df <- sqldf('SELECT *, CASE
                    WHEN HydroID = 194537
                    THEN 1
                    WHEN HydroID = 441638
                    THEN 2
                    ELSE 0
                    END AS fill_order
                   FROM gwma_df')
  gwma.sf <- st_as_sf(gwma_df, wkt = 'Geometry')
  gwma.gg <- geom_sf(data = gwma.sf,aes(fill = factor(fill_order)),colour = "black", shape = 22, lwd=0.4, alpha = 0.5, inherit.aes = FALSE, show.legend = FALSE)
  
  
  library("ggspatial")
  
  # MAP
  permit_map <- basemap.obj + gwma.gg + fips.gg + rivs.gg + res.gg + mp.gg +
    theme(legend.position = c(0.31, .85),
          legend.title=element_text(size=7),
          legend.text=element_text(size=5)) +
    guides(fill = guide_legend(override.aes = list(fill = c("pink","darkorchid2","#0C1078","orange2"),
                                                   alpha = c(.5,.5,1,1),
                                                   size = c(3.5,3.5,2.5,2.5),
                                                   shape = c(22,22,21,21)))) +
    scale_fill_manual(name=paste0(eyear," Groundwater Withdrawal \n Permitting Activities"), 
                      values=c("pink",
                               "darkorchid2",
                               "#0C1078",
                               "orange2"), 
                      labels=c("Eastern Virginia GWMA",
                               "Eastern Shore GWMA",
                               "Active Groundwater Withdrawal Permits", 
                               paste0("Issued Since January ",eyear))) +
    #annotation_scale(location = "br", plot_unit = "mi", width_hint = 0.3)
    #ADD SCALE BAR
    # ggsn::scalebar(bb.gg, location = 'bottomright', dist_unit = 'mi', dist = 100,
    #                transform = TRUE, model = 'WGS84')
  # #ADD SCALE BAR
  # ggsn::scalebar(bb.gg, location = 'bottomright', dist = 100, dist_unit = 'mi',
  #                transform = TRUE, model = 'WGS84',st.bottom=FALSE,
  #                st.size = 2.5, st.dist = 0.0285, height = 0.03,
  #                anchor = c(
  #                  x =  -76.152998,
  #                  y = 36.499674
  #                ))

  deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.13, height = 1, x = -.14, y = -0.43) #LEFT BOTTOM LOGO
  permit_map_draw <- ggdraw(permit_map)+deqlogo
  
  #ggsave(plot = permit_map_draw, file = paste0(export_path, "/awrr/2021/","xtest_GWPermits_AWRR_2020.png",sep = ""), width=6.5, height=4.95) #Working map saves here
  ggsave(plot = permit_map_draw, file = paste0(export_path, "/awrr/2021/","GWPermits_AWRR_2020.pdf",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE
  