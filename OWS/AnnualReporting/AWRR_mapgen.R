# THIS SCRIPT CURRENTLY PRODUCES 18 MAP IMAGES (AS PNG OR PDF)
library("dataRetrieval")
library("ggspatial")
library("rgeos") #BB Added this line, as the readWKT function is needed. Will lose support in 2024
color_list <- sort(colors())
options(scipen=9999)
site <- "http://deq1.bse.vt.edu/d.dh/" ##BB Removed the :81 after edu, was causing errors
basepath <- "/var/www/R/"
source(paste(basepath,"config.local.private",sep = '/'))
## Have been moved to the depricated folder, but are still used
source(paste(hydro_tools,"GIS_functions/deprecated_mapgen_scripts/base.layers.R",sep = '/'))
source(paste(hydro_tools,"GIS_functions/deprecated_mapgen_scripts/base.map.R",sep = '/'))
if(!exists("baselayers")) {baselayers <- load_MapLayers(site = site)} #Load map layers if they're not already. Need VPN

#LOAD IN DATA######################################################################################## 
# LOAD ALL FOUNDATION DATA
syear = 2018
eyear = 2022
source_directory <- paste0(foundation_location,"/OWS/foundation_datasets/awrr/",eyear+1,"") #SOURCE LOCATION

ByLocality <- read.csv(paste(source_directory,"/ByLocality.csv",sep=""))
mp_all_mgy <- read.csv(paste(source_directory,"/mp_all_mgy_",syear,"-",eyear,".csv",sep="")) #GM this is mp_all_mgy now
##mp_all_wide <- read.csv(paste(source_directory,"/mp_all_mgy_",syear,"-",eyear,".csv",sep="")) #BB Redundant, mp_all_mgy is already wide
mp_all_wide_power <- read.csv(paste(source_directory,"/mp_power_mgy_",syear,"-",eyear,".csv",sep="")) #BB mp_al_wide_power replaced with mp_power_mgy
ows_permit_list <- read.csv(paste(source_directory,"/ows_permit_list.csv",sep="")) #GM - from https://deq1.bse.vt.edu/d.dh/ows-permit-list, filter by active and expired permits, do manual check for incorrect cells 
ows_permit_list$Permit.Start2 <- as.character(as.Date(ows_permit_list$Permit.Start, format = "%m/%d/%Y"))#must convert columns with date info to character data type so sqldf can recognize the date format

#MAP OUTPUT LOCATION
export_path <- paste0(foundation_location,"/OWS/Report Development/Annual Water Resources Report/October ",eyear+1," Report/overleaf/") 
# export_path <- paste0("C:/Users/nrf46657/Desktop/GitHub/hydro-tools/OWS/AnnualReporting/TEST_MAPS/") #FOR TESTING
working_path <- "C:/Users/rnv55934/Documents/Docs/AnnualReport/2022/" #FOR TESTING

#GM SET UP MP_ALL FOR MAPGEN
eyearX <- paste0("X",eyear)
mp_all <- sqldf(paste0('SELECT "MP_hydroid" as HydroID, "Hydrocode" as Hydrocode, "Source_Type" as Source_Type, "MP_Name" as MP_Name, 
                       "Facility_hydroid" as Facility_hydroid, "Facility" as Facility, "Use_Type" as Use_Type, "Latitude" as lat, "Longitude" as lon, "FIPS_Code" as FIPS, ',eyearX,' as ',eyearX,',
                       (',eyearX,')/365 as mgd
                       FROM mp_all_mgy'))
# removed   WHERE Year = ',eyear,'  from each point map, add AND WHERE mgd IS NOT NULL


#GENERATE BASEMAP ############################################################################################
baselayers.gg <- base.layers(baselayers) # Need readWKT function here
basemap.obj <- base.map(baselayers.gg) 

sf::sf_use_s2(FALSE) #needed for adding DEQ logo to maps ## Moved here since basemap sets it TRUE

#LOAD RIVERS AND RESERVOIRS LAYERS
rivs.gg <- baselayers.gg[[which(names(baselayers.gg) == "rivs.gg")]]
rivs.gg <- geom_path(data = rivs.gg, aes(x = long, y = lat, group = group), color="dodgerblue3",lwd=0.4,na.rm=TRUE)
res_csv <- baselayers[[which(names(baselayers) == "MajorReservoirs.csv")]]
res.sf <- st_as_sf(res_csv, wkt = 'geom', crs = st_crs(4326))
res.gg <- geom_sf(data = res.sf,color="dodgerblue3",lwd=0.4, inherit.aes = FALSE, show.legend =FALSE)

#LOAD FIPS LAYER
fips_csv <- baselayers[[which(names(baselayers) == "fips.csv")]]

fips_df <- sqldf('SELECT *
                 FROM fips_csv
                 WHERE fips_code NOT LIKE "3%"') #select all in fips_csv and take out NC fips codes

fips.sf <- st_as_sf(fips_df, wkt = 'fips_geom', crs = st_crs(4326))
fips.gg <- geom_sf(data = fips.sf,colour = "black",fill = NA, lwd=0.3, inherit.aes = FALSE, show.legend = FALSE)


#LOAD WSP REGIONS LAYER
ows_wsp_regions_wkt <- read.csv(paste(github_location,'/HARParchive/GIS_layers/ows_wsp_regions_wkt.csv',sep=''))

#LOAD GWMA LAYER
GWMA_wkt <- read.csv(paste(github_location,'/HARParchive/GIS_layers/GWMA_wkt.csv',sep=''))

#PROCESS GWMA LAYER
gwma_df <- sqldf('SELECT *, CASE
                  WHEN HydroID = 194537
                  THEN 3
                  WHEN HydroID = 441638
                  THEN 4
                  ELSE 0
                  END AS fill_order
                 FROM GWMA_wkt')
gwma.sf <- st_as_sf(gwma_df, wkt = 'Geometry')
gwma.gg <- geom_sf(data = gwma.sf,aes(fill = factor(fill_order)),colour = "black", shape = 22, lwd=0.4, alpha = 0.5, inherit.aes = FALSE, show.legend = FALSE)
## END READING IN DATA ###############
# MONITORING STATIONS MAP ##########################################################################

# #FROM VAHYDRO:
# sw_features <- read.csv(paste(site,"monitoring-stations-sw-export",sep=""))
# gw_features <- read.csv(paste(site,"monitoring-stations-gw-export",sep=""))
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
                  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
                  theme(legend.position = c(0.167, 0.89),
                        legend.title=element_text(size=10),
                        legend.text=element_text(size=8),
                        legend.background = element_rect(fill="lightblue",
                                                         size=0.5, linetype="solid"),
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
ggsave(plot = monitoring_map_draw, file = paste0(export_path, "MonitoringStationsMap.pdf",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE



### # DROUGHT REGIONS MAP - ###########################
##STATIC - DONT NEED TO RERUN ############################################################################
drought_fips_df <-sqldf(paste('SELECT *,fips_geom AS geom,
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

drought_fips.sf <- st_as_sf(drought_fips_df, wkt = 'geom', crs = st_crs(4326))
drought_fips.gg <- geom_sf(data = drought_fips.sf,aes(fill = factor(col)),lwd=0.4, inherit.aes = FALSE, show.legend =TRUE)

finalmap.obj <- basemap.obj + drought_fips.gg +
                rivs.gg +
                res.gg +
                coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
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
                                   )

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
drought_map_draw <- ggdraw(finalmap.obj)+deqlogo 
#ggsave(plot = drought_map_draw, file = paste0(export_path, "tables_maps/Xfigures/","finalmap.obj.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = drought_map_draw, file = paste0(export_path, "DroughtRegions.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# GROUNDWATER WITHDRAWAL BY LOCALITY  #######################################################

loc_fips_df <-sqldf(paste('SELECT a.*, a.fips_geom AS geom, b."GW.Withdrawal",
                          CASE
                            WHEN b."GW.Withdrawal" < 1 THEN "',color_list[1],'"
                            WHEN b."GW.Withdrawal" BETWEEN 1 AND 2.5 THEN "',color_list[2],'"
                            WHEN b."GW.Withdrawal" BETWEEN 2.5 AND 5 THEN "',color_list[3],'"
                            WHEN b."GW.Withdrawal" BETWEEN 5 AND 10 THEN "',color_list[4],'"
                            WHEN b."GW.Withdrawal" > 10 THEN "',color_list[5],'"
                            ELSE "white"
                          END AS col
                        FROM fips_csv AS a
                        LEFT JOIN ByLocality AS b
                        ON a.fips_code = b.FIPS_CODE
                        WHERE a.fips_code NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES

loc_fips.sf <- st_as_sf(loc_fips_df, wkt = 'geom', crs = st_crs(4326))
loc_fips.gg <- geom_sf(data = loc_fips.sf,aes(fill = factor(col)),lwd=0.4, inherit.aes = FALSE, show.legend =TRUE)

finalmap.obj <- basemap.obj + loc_fips.gg +
  rivs.gg + res.gg +
  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
  # ggtitle(paste0(eyear," Groundwater Withdrawal by Locality")) +
  # theme(plot.title = element_text(vjust = - 10, hjust = .3)
  theme(legend.position = c(.20, 0.833),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        aspect.ratio = 12.05/16
  ) +
  #guides(fill=guide_legend(ncol=2))+
  scale_fill_manual(name = paste0(eyear," Groundwater Withdrawal (MGD)"),
                    values = c("#FFFF80","#71EB2F","#3DB868","#216E9E","#0C1078","white"),
                    labels = c("< 1.0","1.0 to 2.5","2.5 to 5.0","5.0 to 10.0","> 10.0","white")
  )

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
gw_locality_map_draw <- ggdraw(finalmap.obj)+deqlogo 
#ggsave(plot = gw_locality_map_draw, file = paste0(export_path, "awrr/2021/","map_gw_locality.png",sep = ""), width=6.5, height=4.95)  #Working map saves here
ggsave(plot = gw_locality_map_draw, file = paste0(export_path, "Locality_Groundwater_Map.png",sep = ""), width=6.5, height=4.95)  #FINAL MAP SAVES HERE
#############################################################################################
# SURFACE WATER WITHDRAWAL BY LOCALITY  #######################################################

loc_fips_df <-sqldf(paste('SELECT a.*, a.fips_geom AS geom, b."SW.Withdrawal",
                          CASE
                            WHEN b."SW.Withdrawal" < 1 THEN "',color_list[1],'"
                            WHEN b."SW.Withdrawal" BETWEEN 1 AND 5 THEN "',color_list[2],'"
                            WHEN b."SW.Withdrawal" BETWEEN 5 AND 25 THEN "',color_list[3],'"
                            WHEN b."SW.Withdrawal" BETWEEN 25 AND 50 THEN "',color_list[4],'"
                            WHEN b."SW.Withdrawal" > 50 THEN "',color_list[5],'"
                            ELSE "white"
                          END AS col
                        FROM fips_csv AS a
                        LEFT JOIN ByLocality AS b
                        ON a.fips_code = b.FIPS_CODE
                        WHERE a.fips_code NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES

loc_fips.sf <- st_as_sf(loc_fips_df, wkt = 'geom', crs = st_crs(4326))
loc_fips.gg <- geom_sf(data = loc_fips.sf,aes(fill = factor(col)),lwd=0.4, inherit.aes = FALSE, show.legend =TRUE)

finalmap.obj <- basemap.obj + loc_fips.gg +
  rivs.gg + res.gg +
  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
  theme(legend.position = c(0.206, 0.833),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid"),
        aspect.ratio = 12.05/16
  ) +
  #guides(fill=guide_legend(ncol=2))+
  scale_fill_manual(name = paste0(eyear," Surface Water Withdrawal (MGD)"),
                    values = c("#FFFF80","#71EB2F","#3DB868","#216E9E","#0C1078","white"),
                    labels = c("< 1.0","1.0 to 5.0","5.0 to 25.0","25.0 to 50.0","> 50.0","white")
  ) 

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
sw_locality_map_draw <- ggdraw(finalmap.obj)+deqlogo 
#ggsave(plot = sw_locality_map_draw, file = paste0(export_path, "awrr/2021/","map_sw_locality.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = sw_locality_map_draw, file = paste0(export_path, "Locality_SurfaceWater_Map.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE


#############################################################################################
# TOTAL WITHDRAWAL BY LOCALITY  #######################################################

loc_fips_df <-sqldf(paste('SELECT a.*, a.fips_geom AS geom, b."Total.Withdrawal",
                          CASE
                            WHEN b."Total.Withdrawal" < 1 THEN "',color_list[1],'"
                            WHEN b."Total.Withdrawal" BETWEEN 1 AND 5 THEN "',color_list[2],'"
                            WHEN b."Total.Withdrawal" BETWEEN 5 AND 25 THEN "',color_list[3],'"
                            WHEN b."Total.Withdrawal" BETWEEN 25 AND 50 THEN "',color_list[4],'"
                            WHEN b."Total.Withdrawal" > 50 THEN "',color_list[5],'"
                            ELSE "white"
                          END AS col
                        FROM fips_csv AS a
                        LEFT JOIN ByLocality AS b
                        ON a.fips_code = b.FIPS_CODE
                        WHERE a.fips_code NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES

loc_fips.sf <- st_as_sf(loc_fips_df, wkt = 'geom', crs = st_crs(4326))
loc_fips.gg <- geom_sf(data = loc_fips.sf,aes(fill = factor(col)),lwd=0.4, inherit.aes = FALSE, show.legend =TRUE)

finalmap.obj <- basemap.obj + loc_fips.gg +
  rivs.gg + res.gg +
  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
  theme(legend.position = c(0.16, 0.833),
        legend.title=element_text(size=10),
        legend.text=element_text(size=10),
        aspect.ratio = 12.05/16
  ) +
  #guides(fill=guide_legend(ncol=2))+
  scale_fill_manual(name = paste0(eyear," Total Withdrawal (MGD)"),
                    values = c("#FFFF80","#71EB2F","#3DB868","#216E9E","#0C1078","white"),
                    labels = c("< 1.0","1.0 to 5.0","5.0 to 25.0","25.0 to 50.0","> 50.0","white")
  )

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
total_locality_map_draw <- ggdraw(finalmap.obj)+deqlogo 
#ggsave(plot = total_locality_map_draw, file = paste0(export_path, "awrr/2021/","map_total_locality.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = total_locality_map_draw, file = paste0(export_path, "Locality_Total_Map.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE


#############################################################################################
# Agriculture (Non-Irrigation) Water Withdrawals by Withdrawal Point Location################

#NOTE - may need to reset size bins and legend.position if data changes (GM)

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
                        FROM mp_all AS a
                        WHERE Use_Type = "agriculture"
                        AND mgd IS NOT NULL
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES


mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, size = factor(point_size)), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)

ag_map <-  basemap.obj + fips.gg + res.gg + mp.gg +
  #  rivs.gg +
  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
  theme(legend.position = c(0.146, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid"),
        aspect.ratio = 12.05/16
  ) +
  scale_size_manual(name=paste0(eyear," Agriculture (Non-Irrigation) \n Water Withdrawals (MGD)"), values=c(2,3,4,5,6,0),
                     labels=c("< 0.05","0.05 - 0.5","0.5 - 1.0","1.0 - 5.0","> 5.0"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
ag_map_draw <- ggdraw(ag_map)+deqlogo


#ggsave(plot = ag_map_draw, file = paste0(working_path,"map_ag_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here
ggsave(plot = ag_map_draw, file = paste0(export_path, "Agriculture_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# Irrigation Water Withdrawals by Withdrawal Point Location################

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
                        FROM mp_all AS a
                        WHERE Use_Type = "irrigation"
                        AND mgd IS NOT NULL
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES

mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, size = factor(point_size)), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)

irr_map <-  basemap.obj + fips.gg + res.gg + mp.gg +
  #  rivs.gg +
  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
  theme(legend.position = c(0.146, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid"),
        aspect.ratio = 12.05/16
  ) +
  scale_size_manual(name=paste0(eyear," Irrigation (Agriculture) \n Water Withdrawals (MGD)"), values=c(2,3,4,5,6,0),
                    labels=c("< 0.05","0.05 - 0.10","0.10 - 0.25","0.25 - 0.50","> 0.50"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
irr_map_draw <- ggdraw(irr_map)+deqlogo

#ggsave(plot = irr_map_draw, file = paste0(working_path,"map_irr_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = irr_map_draw, file = paste0(export_path, "Irrigation_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# Commercial Water Withdrawals by Withdrawal Point Location################

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
                        FROM mp_all AS a
                        WHERE Use_Type = "commercial"
                        AND mgd IS NOT NULL
                        AND (lat < 41 AND lat >34)
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES

mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, size = factor(point_size)), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)

comm_map <- basemap.obj + fips.gg + res.gg + mp.gg +
#  rivs.gg +
  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
  theme(legend.position = c(0.146, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid"),
        aspect.ratio = 12.05/16
  ) +
  scale_size_manual(name=paste0(eyear," Commerical \n Water Withdrawals (MGD)"), values=c(2,3,4,5,6,0),
                    labels=c("< 0.05","0.05 - 0.10","0.10 - 0.25","0.25 - 0.50","> 0.50"))


deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
comm_map_draw <- ggdraw(comm_map)+deqlogo

#ggsave(plot = comm_map_draw, file = paste0(working_path,"map_com_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = comm_map_draw, file = "C:\\Users\\ejp42531\\Desktop\\The shoulders of broken giants\\Commerical_PointMap_LegendFill.png", width=6.5, height=4.95) #FINAL MAP SAVES HERE
# ggsave(plot = comm_map_draw, file = paste0(export_path, "Commerical_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# Mining Water Withdrawals by Withdrawal Point Location################

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
                        FROM mp_all AS a
                        WHERE Use_Type = "mining"
                        AND mgd IS NOT NULL
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES


mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, size = factor(point_size)), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)

min_map <-  basemap.obj + fips.gg + res.gg + mp.gg +
  #  rivs.gg +
  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
  theme(legend.position = c(0.146, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid"),
        aspect.ratio = 12.05/16
  ) +
  scale_size_manual(name=paste0(eyear," Mining \n Water Withdrawals (MGD)"), values=c(2,3,4,5,6,0),
                    labels=c("< 0.05","0.05 - 0.5","0.5 - 1.0","1.0 - 5.0","> 5.0"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
min_map_draw <- ggdraw(min_map)+deqlogo

#ggsave(plot = min_map_draw, file = paste0(working_path,"map_min_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = comm_map_draw, file = "C:\\Users\\ejp42531\\Desktop\\The shoulders of broken giants\\Mining_PointMap.png", width=6.5, height=4.95) #FINAL MAP SAVES HERE
ggsave(plot = min_map_draw, file = paste0(export_path, "Mining_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# Manufacturing Water Withdrawals by Withdrawal Point Location################

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
                        FROM mp_all AS a
                        WHERE Use_Type = "manufacturing"
                        AND mgd IS NOT NULL
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE NC LOCALITIES
#Including Tyson well hydroid 398760 that was excluded last year because lat/lon is now on land 

mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, size = factor(point_size)), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)

man_map <-  basemap.obj + fips.gg + res.gg + mp.gg +
  #  rivs.gg +
  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
  theme(legend.position = c(0.146, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid"),
        aspect.ratio = 12.05/16
  ) +
  scale_size_manual(name=paste0(eyear," Manufacturing \n Water Withdrawals (MGD)"), values=c(2,3,4,5,6,0),
                    labels=c("< 0.05","0.05 - 0.5","0.5 - 5","5 - 25","> 25"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
man_map_draw <- ggdraw(man_map)+deqlogo

#ggsave(plot = man_map_draw, file = paste0(working_path,"map_man_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = man_map_draw, file = paste0(export_path, "Manufacturing_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE


#############################################################################################
# Public Water Supply Water Withdrawals by Withdrawal Point Location################

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
                        FROM mp_all AS a
                        WHERE Use_Type = "municipal"
                        AND mgd IS NOT NULL
                    AND a.FIPS NOT LIKE "3%"',sep="")) #EXCLUDE, NC LOCALITIES


mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, size = factor(point_size)), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)

pws_map <-  basemap.obj + fips.gg + res.gg + mp.gg +
  #  rivs.gg +
  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
  theme(legend.position = c(0.146, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid"),
        aspect.ratio = 12.05/16
  ) +
  scale_size_manual(name=paste0(eyear," Public Water Supply \n Water Withdrawals (MGD)"), values=c(1,2,3,4,5,0), #GM smaller point sizes for pws so individual points can be discerned
                    labels=c("< 0.05","0.05 - 0.5","0.5 - 5","5 - 25","> 25"))



deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
pws_map_draw <- ggdraw(pws_map)+deqlogo

#ggsave(plot = pws_map_draw, file = paste0(working_path,"map_pws_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = pws_map_draw, file = paste0(export_path, "PublicWaterSupply_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# Power Generation Water Withdrawals by Withdrawal Point Location################

#five MP with missing lat/lon in the mp_all_wide_power csv, each less than 0.02mgd, each with another facility mp that does show on map, so lat/lon was not corrected this year
mp_all_wide_power[is.na(mp_all_wide_power)] <- 0 # Convert NAs to zeros because NA means 0mgd was reported

mp_all_wide_power <- sqldf(paste0('SELECT *, "X',eyear,'"/365 AS "',eyear,'mgd"
                         FROM mp_all_wide_power
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
                        FROM mp_all_wide_power AS a
                        WHERE Use_Type = "fossilpower"
                        AND a."FIPS_Code" NOT LIKE "3%"
                        AND a."Fips_Code" NOT IN (0)
                        AND a.MP_Hydroid NOT IN(65370)
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
                        FROM mp_all_wide_power AS a
                        WHERE Use_Type = "nuclearpower"
                        AND a."FIPS_Code" NOT LIKE "3%"
                      ORDER BY "',eyear,'mgd" DESC 
                      ',sep="")) #EXCLUDE, NC LOCALITIES

#adding color category here so fossil and nuclear have a category that can show up in the legend
mp_f.gg <- geom_point(data = mp_df_f,aes(x = lon, y = lat, size = factor(point_size), color = "Fossil Power"), fill="#0C1078", alpha=0.9, shape=21, show.legend = TRUE)
mp_n.gg <- geom_point(data = mp_df_n,aes(x = lon, y = lat, size = factor(point_size), color = "Nuclear Power"), fill="orange2", alpha=0.75, shape=21, show.legend = TRUE)


pow_map <-  basemap.obj + fips.gg + res.gg +  mp_f.gg + mp_n.gg +
  #  rivs.gg +
  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
  theme(legend.position = c(0.146, 0.817),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid"),
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
pow_map_draw <- ggdraw(pow_map)+deqlogo

#ggsave(plot = pow_map_draw, file = paste0(working_path,"map_pow_mp.png",sep = ""), width=6.5, height=4.95) #Working map saves here 
ggsave(plot = pow_map_draw, file = paste0(export_path, "Power_PointMap.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# Surface Water Withdrawal Permitting Activities ############################################

#filter for SW that are currently active or in admin continued status
mp_point_sw <- sqldf('SELECT a.*
                     FROM ows_permit_list AS a
                     WHERE a."Permit.Program" LIKE "%VWP%"
                     AND a."Status" IN ("active", "expired")')

#filter for New Permit Issuances - bins for point color
mp_df <-sqldf(paste('SELECT *,
                          CASE
                            WHEN "Permit.Start2" >= "',eyear,'-01-01" THEN 3
                          ELSE 2
                          END AS point_size
                        FROM mp_point_sw AS a
                    ORDER BY "Permit.Start2" ASC',sep="")) 
#GM update for eyear

mp.gg <- geom_point(data = mp_df,aes(x = Facility.Longitude, y = Facility.Latitude, size = factor(point_size), fill=factor(point_size)), alpha=0.9, shape=21, show.legend = TRUE)

sw_permit_map <- basemap.obj + fips.gg + rivs.gg + res.gg + mp.gg +
  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
  theme(legend.position = c(0.268, .9075),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid"),
        aspect.ratio = 12.05/16) +
  guides(size = guide_legend(override.aes = list(size = c(2,3))),
         fill = guide_legend(override.aes = list(fill = c("#0C1078", "orange2")))) +
    scale_size_manual(name=paste0(eyear," Surface Water Withdrawal Permitting Activities"), values=c(2,3), labels=c("Active Surface Water Withdrawal Permits", paste0("Issued Since January ",eyear))) +
  scale_fill_manual(name=paste0(eyear," Surface Water Withdrawal Permitting Activities"), values=c("#0C1078", "orange2"),  labels=c("Active Surface Water Withdrawal Permits", paste0("Issued Since January ",eyear))) 

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
sw_permit_map_draw <- ggdraw(sw_permit_map)+deqlogo

# ggsave(plot = sw_permit_map_draw, file = paste0(working_path,"VWPermits_AWRR.pdf",sep = ""), width=6.5, height=4.95) #Working map saves here #GM update filenames 
ggsave(plot = sw_permit_map_draw, file = paste0(export_path, "VWPermits_AWRR.pdf",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE
#############################################################################################
# Groundwater Withdrawal Permitting Activities - RUN This as Precursor ##############################################

#filter for SW that are currently active or in admin continued status
mp_point_gw <- sqldf('SELECT a.*
                   FROM ows_permit_list AS a
                   WHERE a."Permit.Program" LIKE "%GWP%"
                   AND a."Status" IN ("active", "expired")')

#filter for New Permit Issuances - bins for point color
mp_df <-sqldf(paste('SELECT *,
                        CASE
                          WHEN "Permit.Start2" >= "',eyear,'-01-01" THEN 4
                        ELSE 3
                        END AS point_size
                      FROM mp_point_gw AS a
                  ORDER BY "Permit.Start2" ASC',sep="")) 

mp.gg <- geom_point(data = mp_df,aes(x = Facility.Longitude, y = Facility.Latitude, fill=factor(point_size)), alpha=0.9, size = 2, shape=21, inherit.aes = FALSE, show.legend = TRUE)

# #GWMA LAYER
gwma_df <- sqldf('SELECT *, CASE
                  WHEN HydroID = 194537
                  THEN 1
                  WHEN HydroID = 441638
                  THEN 2
                  ELSE 0
                  END AS fill_order
                 FROM GWMA_wkt')
gwma.sf <- st_as_sf(gwma_df, wkt = 'Geometry', crs = st_crs(4326))
gwma.gg <- geom_sf(data = gwma.sf,aes(fill = factor(fill_order)),colour = "black", shape = 22, lwd=0.4, alpha = 0.5, inherit.aes = FALSE, show.legend = FALSE)

# MAP
gw_permit_map <- basemap.obj + gwma.gg + fips.gg + rivs.gg + res.gg + mp.gg +
  coord_sf(xlim = c(-84,-75), ylim = c(35.25,40.6),expand = F) +
  theme(legend.position = c(0.264, .8555),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid"),
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
gw_permit_map_draw <- ggdraw(gw_permit_map)+deqlogo

#ggsave(plot = gw_permit_map_draw, file = paste0(working_path, "GWPermits_AWRR.png",sep = ""), width=6.5, height=4.95) #Working map saves here
ggsave(plot = gw_permit_map_draw, file = paste0(export_path, "GWPermits_AWRR.pdf",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# ZOOMED IN EXTENT - Groundwater Withdrawal Permitting Activities - USE THIS Version in Annual Report ##############################################

#GWMA extent
GWMA.extent = data.frame( 
  x = c(-77.7, -75),
  y = c(36, 40.6))

GWMA.baselayers.gg <- base.layers(baselayers, 
                                  extent = GWMA.extent)
GWMA.basemap.obj <- base.map(GWMA.baselayers.gg, 
                             extent = GWMA.extent,
                             plot_margin = c(.25,0,.25,0), #top, right, bottom, left
                             plot_zoom = 8,
                             scale_bar = F)
# ggsave(plot = GWMA.basemap.obj, file = paste0(export_path,"GWMA.basemap.png",sep = ""), width=6.5, height=4.95)

# MAP
gw_permit_map_zoom <- GWMA.basemap.obj + gwma.gg + fips.gg + rivs.gg + res.gg + mp.gg +
  coord_sf(xlim = GWMA.extent$x, ylim = GWMA.extent$y,expand = F) +
  theme(legend.position = c(0.31, .85),
        legend.title=element_text(size=7),
        legend.background = element_rect(fill="lightblue",
                                         size=0.5, linetype="solid"),
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
                             paste0("Issued Since January ",eyear)))

deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.13, height = 1, x = -.14, y = -0.43) #LEFT BOTTOM LOGO
gw_permit_map_zoom_draw <- ggdraw(gw_permit_map_zoom)+deqlogo

#ggsave(plot = gw_permit_map_zoom_draw, file = paste0(working_path,"xtest_GWPermits_AWRR.png",sep = ""), width=6.5, height=4.95) #Working map saves here
ggsave(plot = gw_permit_map_zoom_draw, file = paste0(export_path, "GWPermits_AWRR.pdf",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

#############################################################################################
# WSP Regions Map - STATIC - DONT NEED TO RERUN ###########################################################################

 ows_wsp_regions_wkt$region_name <- as.character(ows_wsp_regions_wkt$region_name)

 wsp_df <- sqldf('SELECT *  
       FROM ows_wsp_regions_wkt
       WHERE geom IS NOT NULL
       AND "FIPS_Code" NOT LIKE "3%"')

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
ggsave(plot = wsp_map_draw, file = paste0(export_path, "WSP_Planning_Regions_Map.pdf",sep = ""), width=10, height=15) #FINAL MAP SAVES HERE

#############################################################################################

# End of Figures in Annual Report ################

#############################################################################################
# Unpermitted VS. Surface Water Withdrawal Permitting Activities - NOT IN AR ############################################

#PERMIT LAYER
#filter for SW that are currently active or in admin continued status
mp_permit_vwp <- sqldf('SELECT a.*
                   FROM ows_permit_list AS a
                   WHERE a."Permit.Program" LIKE "%VWP%"
                   AND a."Status" IN ("active", "expired")')


#MPs LAYER
#join/union the SW MPs into 1 layer
mp_all_wide_sw <- sqldf('SELECT "HydroID","Source_Type","MP_Name","Facility_HydroID","Facility","Use_Type", "FIPS","lat","lon","X2016","X2017","X2018","X2019","X2020","Locality"
                FROM mp_all_mgy
                WHERE "Source_Type" LIKE "Surface Water"')
mp_all_wide_power_sw <- sqldf('SELECT "HydroID","Source_Type","MP_Name","Facility_hydroID" AS "Facility_HydroID","Facility","Use_Type", "fips" as "FIPS","lat","lon","X2016","X2017","X2018","X2019","X2020","Locality"
                FROM mp_all_wide_power
                WHERE "Source_Type" LIKE "Surface Water"')
mp <- sqldf('SELECT *
            FROM mp_all_wide_sw
            UNION all 
            SELECT * 
            FROM mp_all_wide_power_sw')

#filter for MPs with and without Permits
mp_df <-sqldf(paste('SELECT a.*,b."Permit",b."Permit.ID", b."Permit.Program", b."Status"
                      FROM mp AS a
                    LEFT OUTER JOIN mp_permit_vwp AS b
                    ON a."Facility_HydroID" = b."VA.Hydro.Facility.ID" ',sep="")) 
# write.csv(mp_df, paste0(export_path, "/awrr/2021/","mp_unpermitted_vs_permitted_VWP.csv",sep = ""), row.names = F)

mp_df <- sqldf('SELECT *, CASE
                          WHEN "Permit.ID" IS NULL
                          THEN 0
                          WHEN "Permit.ID" IS NOT NULL
                          THEN 1
                          ELSE "Outlier"
                          END AS has_permit
               FROM mp_df
               ORDER BY has_permit')

mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, fill=factor(has_permit), size = factor(has_permit)), alpha=0.9, shape=21, show.legend = TRUE)

permit_swvs_map <- basemap.obj + fips.gg + rivs.gg + res.gg + mp.gg +
  theme(legend.position = c(0.18, .9069),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        aspect.ratio = 12.05/16) +
  guides(size = guide_legend(override.aes = list(size = c(2,3))),
         fill = guide_legend(override.aes = list(fill = c("#0C1078", "yellow")))) +
  scale_size_manual(name="Surface Water Withdrawal ", values=c(2,3), labels=c("Unpermitted Surface Water Intakes", "Permitted Surface Water Intakes")) +
  scale_fill_manual(name="Surface Water Withdrawal ", 
                    values=c("#0C1078", "yellow"), 
                    labels=c("Unpermitted Surface Water Intakes", "Permitted Surface Water Intakes")) 
#permit_map
deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
permit_swvs_map_draw <- ggdraw(permit_swvs_map)+deqlogo

# ggsave(plot = permit_swvs_map_draw, file = paste0(export_path, "/awrr/2021/","Unpermitted_vs_VWP.png",sep = ""), width=6.5, height=4.95) #Working map saves here
ggsave(plot = permit_swvs_map_draw, file = paste0(export_path, "Unpermitted_vs_VWP.png",sep = ""), width=6.5, height=4.95) #Working map saves here

#ggsave(plot = permit_swvs_map_draw, file = paste0(export_path, "VWPermits_AWRR_2020.pdf",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE
#############################################################################################
# Unpermitted VS. Groundwater Withdrawal Permitting Activities - NOT IN AR ##############################

#PERMIT LAYER
#filter for SW that are currently active or in admin continued status
mp_permit_gwp <- sqldf('SELECT a.*
                   FROM ows_permit_list AS a
                   WHERE a."Permit.Program" LIKE "%GWP%"
                   AND a."Status" IN ("active", "expired")')


#MPs LAYER
#join/union the SW MPs into 1 layer
mp_all_wide_gw <- sqldf('SELECT "HydroID","Source_Type","MP_Name","Facility_HydroID","Facility","Use_Type", "FIPS","lat","lon","X2016","X2017","X2018","X2019","X2020","Locality"
                FROM mp_all_wide
                WHERE "Source_Type" LIKE "Groundwater"')
mp_all_wide_power_gw <- sqldf('SELECT "HydroID","Source_Type","MP_Name","Facility_hydroID" AS "Facility_HydroID","Facility","Use_Type", "fips" as "FIPS","lat","lon","X2016","X2017","X2018","X2019","X2020","Locality"
                FROM mp_all_wide_power
                WHERE "Source_Type" LIKE "Groundwater"')
mp <- sqldf('SELECT *
            FROM mp_all_wide_gw
            UNION all 
            SELECT * 
            FROM mp_all_wide_power_gw')

#filter for MPs with and without Permits
mp_df <-sqldf(paste('SELECT a.*,b."Permit",b."Permit.ID", b."Permit.Program", b."Status"
                      FROM mp AS a
                    LEFT OUTER JOIN mp_permit_gwp AS b
                    ON a."Facility_HydroID" = b."VA.Hydro.Facility.ID" ',sep="")) 
# write.csv(mp_df, paste0(export_path, "/awrr/2021/","mp_unpermitted_vs_permitted_GWP.csv",sep = ""), row.names = F)

mp_df <- sqldf('SELECT *, CASE
                          WHEN "Permit.ID" IS NULL
                          THEN 0
                          WHEN "Permit.ID" IS NOT NULL
                          THEN 1
                          ELSE "Outlier"
                          END AS has_permit
               FROM mp_df
               ORDER BY has_permit')

mp.gg <- geom_point(data = mp_df,aes(x = lon, y = lat, fill=factor(has_permit)), alpha=0.9, shape=21, show.legend = TRUE)

# GWMA LAYER
gwma_df <- sqldf('SELECT *, CASE
                  WHEN HydroID = 194537
                  THEN 3
                  WHEN HydroID = 441638
                  THEN 4
                  ELSE 0
                  END AS fill_order
                 FROM GWMA_wkt')
gwma.sf <- st_as_sf(gwma_df, wkt = 'Geometry')
gwma.gg <- geom_sf(data = gwma.sf,aes(fill = factor(fill_order)),colour = "black", shape = 22, lwd=0.4, alpha = 0.5, inherit.aes = FALSE, show.legend = FALSE)

permit_gwvs_map <- basemap.obj + fips.gg + gwma.gg + rivs.gg + res.gg + mp.gg +
  theme(legend.position = c(0.235, .854),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        aspect.ratio = 12.05/16) +
  guides(fill = guide_legend(override.aes = list(alpha = c(1,1,.5,.5),
                                                 size = c(3,3,3,3),
                                                 shape = c(21,21,22,22)))) +
  scale_fill_manual(name="Groundwater Withdrawal ", 
                    values=c("#0C1078", "yellow","pink","darkorchid2"), 
                    labels=c("Unpermitted Groundwater Wells",
                             "Permitted Groundwater Wells",
                             "Eastern Virginia Groundwater Management Area",
                             "Eastern Shore Groundwater Management Area"
                             )) 
#permit_map
deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
permit_gwvs_map_draw <- ggdraw(permit_gwvs_map)+deqlogo

# ggsave(plot = permit_gwvs_map_draw, file = paste0(export_path, "/awrr/2021/","Unpermitted_vs_GWP.png",sep = ""), width=6.5, height=4.95) #Working map saves here
ggsave(plot = permit_gwvs_map_draw, file = paste0(export_path, "Unpermitted_vs_GWP.png",sep = ""), width=6.5, height=4.95) #Working map saves here

#ggsave(plot = permit_gwvs_map_draw, file = paste0(export_path, "GWPermits_AWRR_2020.pdf",sep = ""), width=6.5, height=4.95)
#FINAL MAP SAVES HER######################################################################################################

### This used to repeat itself. Deleted section 01-09-24. If something breaks, check back through github
