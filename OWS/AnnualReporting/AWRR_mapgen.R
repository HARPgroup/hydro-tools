###################################################################################################### 
# LOAD FILES
######################################################################################################
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

#FROM VAHYDRO:
sw_features <- read.csv(paste(site,"monitoring-stations-sw-export",sep=""))
gw_features <- read.csv(paste(site,"monitoring-stations-gw-export",sep=""))

sw.gg <- geom_point(data = sw_features,aes(x = longitude, y = latitude, color="aliceblue"),size=2, shape=17, show.legend = TRUE)
gw.gg <- geom_point(data = gw_features,aes(x = longitude, y = latitude, color="antiquewhite"),size=2, show.legend = TRUE)

#FROM NWIS:
# sw_features <- whatNWISsites(stateCd = "VA", parameterCd = "00060")
# sw.sf <- st_as_sf(sw_features, coords = c("dec_long_va", "dec_lat_va"),crs = 4326)
# sw.gg <- geom_sf(data = sw.sf,color="aliceblue",size=2, shape=17, inherit.aes = FALSE, show.legend =TRUE)
# 
# gw_features <- whatNWISsites(stateCd = "VA", parameterCd = "72019")
# gw.sf <- st_as_sf(gw_features, coords = c("dec_long_va", "dec_lat_va"),crs = 4326)
# gw.gg <- geom_sf(data = gw.sf,color="antiquewhite",size=2, inherit.aes = FALSE, show.legend =TRUE)

monitoring_map <- basemap.obj + gw.gg + sw.gg + theme(legend.position = c(0.12, 0.9)) +
                  theme(legend.position = c(0.11, 0.905),
                        legend.title=element_text(size=10),
                        legend.text=element_text(size=8),
                        aspect.ratio = 12.05/16
                  ) +
                  scale_color_manual("Legend", values=c("blue","brown4"),
                                               labels=c("Streamflow Gage","Observation Well")) +
                  guides(colour = guide_legend(override.aes = list(size = c(2, 2),
                                                                   shape = c(17, 19)))) +
                  rivs.gg +
                  res.gg

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
monitoring_map_draw <- ggdraw(monitoring_map)+deqlogo
ggsave(plot = monitoring_map_draw, file = paste0(export_path, "tables_maps/Xfigures/","monitoring_map.png",sep = ""), width=6.5, height=4.95)

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
                rivs.gg +
                res.gg

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
drought_map_draw <- ggdraw(finalmap.obj)+deqlogo 
ggsave(plot = drought_map_draw, file = paste0(export_path, "tables_maps/Xfigures/","finalmap.obj.png",sep = ""), width=6.5, height=4.95)

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
ggsave(plot = gw_locality_map_draw, file = paste0(export_path, "awrr/2021/","map_gw_locality.png",sep = ""), width=6.5, height=4.95)

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
ggsave(plot = gw_locality_map_draw, file = paste0(export_path, "awrr/2021/","map_sw_locality.png",sep = ""), width=6.5, height=4.95)


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
ggsave(plot = gw_locality_map_draw, file = paste0(export_path, "awrr/2021/","map_total_locality.png",sep = ""), width=6.5, height=4.95)


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

#option 2 for case statement bin breakup
# WHEN "mgd" < 0.05 THEN 1
# WHEN "mgd" BETWEEN 0.05 AND 0.5 THEN 2
# WHEN "mgd" BETWEEN 0.5 AND 1 THEN 3
# WHEN "mgd" > 1 THEN 4


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

ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_ag_mp.png",sep = ""), width=6.5, height=4.95)

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
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE point with lat/lon in WV, AND NC LOCALITIES

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

ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_irr_mp.png",sep = ""), width=6.5, height=4.95)

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
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE point with lat/lon in WV, AND NC LOCALITIES

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

ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_com_mp.png",sep = ""), width=6.5, height=4.95)

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

ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_min_mp.png",sep = ""), width=6.5, height=4.95)

#############################################################################################
# Manufacturing Water Withdrawals by Withdrawal Point Location################

mp_point <- read.csv(paste("U:/OWS/foundation_datasets/awrr/",eyear+1,"/mp_all_",syear,"-",eyear,".csv",sep=""))

#try natural breaks or size bins next year
#removing 84 instances of 0 MGD (out of 183 category 2's)
mp_df <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "mgd" BETWEEN 0.00000000000001 AND 0.05 THEN 1
                            WHEN "mgd" BETWEEN 0.05 AND 0.5 THEN 2
                            WHEN "mgd" BETWEEN 0.5 AND 5 THEN 3
                            WHEN "mgd" BETWEEN 5 AND 25 THEN 4
                            WHEN "mgd" > 25 THEN 5
                            ELSE 0
                          END AS point_size
                        FROM mp_point AS a
                        WHERE Year = ',eyear,'
                        AND Use_Type = "manufacturing"
                    AND a.HydroID NOT LIKE "398760"
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES
#Point that was excluded is a permitted Tyson well with HydroID 398760 because Point appears in the ocean

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
  scale_size_manual(name=paste0(eyear," Manufacturing \n Water Withdrawals (MGD)"), values=c(1,2,3,4,5,0),
                    labels=c("< 0.05","0.05 - 0.5","0.5 - 5","5 - 25","> 25"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
ag_map_draw <- ggdraw(ag_map)+deqlogo

ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_man_mp_4.png",sep = ""), width=6.5, height=4.95)

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

ggsave(plot = ag_map_draw, file = paste0(export_path, "/awrr/2021/","map_pws_mp.png",sep = ""), width=6.5, height=4.95)

#############################################################################################
# Power Generation Water Withdrawals by Withdrawal Point Location################

mp_point <- read.csv(paste("U:/OWS/foundation_datasets/awrr/",eyear+1,"/mp_all_wide_power_",syear,"-",eyear,".csv",sep=""))


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
                        WHERE Use_Type = "fossilpower"
                    AND a.fips NOT LIKE "3%"',sep="")) #EXCLUDE, NC LOCALITIES
#WHERE Year = ',eyear,'
#AND a.X2020 NOT LIKE null
