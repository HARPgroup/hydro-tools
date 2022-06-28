###################################################################################################### 
# THIS SCRIPT CURRENTLY PRODUCES 18 MAP IMAGES (AS PNG OR PDF)
######################################################################################################
# library("dataRetrieval")
library("ggspatial")
color_list <- sort(colors())
options(scipen=9999)
sf::sf_use_s2(FALSE) #needed for adding DEQ logo to maps
site <- "http://deq1.bse.vt.edu:81/d.dh/"
basepath <- "/var/www/R/"
source(paste(basepath,"config.local.private",sep = '/'))
source(paste(hydro_tools,"GIS_functions/base.layers.R",sep = '/'))
source(paste(hydro_tools,"GIS_functions/base.map.R",sep = '/'))
if(!exists("baselayers")) {baselayers <- load_MapLayers(site = site)} #Load map layers if they're not already 

library('hydrotools') #required for RomDataSource
basepath<-'/var/www/R'
source('/var/www/R/config.R')
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)

#MAP OUTPUT LOCATION
#export_path <- paste0("U:/OWS/Report Development/Annual Water Resources Report/October ",eyear+1," Report/overleaf/") #FINAL LOCATION
# export_path <- paste0("C:/Users/nrf46657/Desktop/GitHub/hydro-tools/examples/TEST_MAPS/") #FOR TESTING
export_path <- paste0("C:/Users/jklei/Desktop/GitHub/hydro-tools/examples/TEST_MAPS/") #FOR TESTING

# ###################################################################################################### 
# # LOAD ALL FOUNDATION DATA
# syear = 2016
# eyear = 2020
# source_directory <- paste0("U:/OWS/foundation_datasets/awrr/",eyear+1,"") #SOURCE LOCATION
# 
# ByLocality <- read.csv(paste(source_directory,"/ByLocality.csv",sep=""))
# mp_all <- read.csv(paste(source_directory,"/mp_all_",syear,"-",eyear,".csv",sep=""))
# mp_all_wide <- read.csv(paste(source_directory,"/mp_all_wide_",syear,"-",eyear,".csv",sep=""))
# mp_all_wide_power <- read.csv(paste(source_directory,"/mp_all_wide_power_",syear,"-",eyear,".csv",sep=""))
# ows_permit_list <- read.csv(paste(source_directory,"/ows_permit_list.csv",sep=""))
# ows_permit_list$Permit.Start2 <- as.character(as.Date(ows_permit_list$Permit.Start, format = "%m/%d/%Y"))#must convert columns with date info to character data type so sqldf can recognize the date format
###################################################################################################### 


###################################################################################################### 
# GENERATE BASEMAP
######################################################################################################

# BASEMAP ############################################################################################
baselayers.gg <- base.layers(baselayers)
basemap.obj <- base.map(baselayers.gg)

#LOAD RIVERS AND RESERVOIRS LAYERS
rivs.gg <- baselayers.gg[[which(names(baselayers.gg) == "rivs.gg")]]
rivs.gg <- geom_path(data = rivs.gg, aes(x = long, y = lat, group = group), color="dodgerblue3",lwd=0.4,na.rm=TRUE)
res_csv <- baselayers[[which(names(baselayers) == "MajorReservoirs.csv")]]
res.sf <- st_as_sf(res_csv, wkt = 'geom')
res.gg <- geom_sf(data = res.sf,color="dodgerblue3",lwd=0.4, inherit.aes = FALSE, show.legend =FALSE)

#LOAD FIPS LAYER
fips_csv <- baselayers[[which(names(baselayers) == "fips.csv")]]

#LOAD WSP REGIONS LAYER
# ows_wsp_regions_wkt <- read.csv(paste(github_location,'/HARParchive/GIS_layers/ows_wsp_regions_wkt.csv',sep=''))

#LOAD GWMA LAYER
# GWMA_wkt <- read.csv(paste(github_location,'/HARParchive/GIS_layers/GWMA_wkt.csv',sep=''))

#PROCESS GWMA LAYER
# gwma_df <- sqldf('SELECT *, CASE
#                   WHEN HydroID = 194537
#                   THEN 3
#                   WHEN HydroID = 441638
#                   THEN 4
#                   ELSE 0
#                   END AS fill_order
#                  FROM GWMA_wkt')
# gwma.sf <- st_as_sf(gwma_df, wkt = 'Geometry')
# gwma.gg <- geom_sf(data = gwma.sf,aes(fill = factor(fill_order)),colour = "black", shape = 22, lwd=0.4, alpha = 0.5, inherit.aes = FALSE, show.legend = FALSE)
####################################################################################

# ######################################################################################################
# ### MONITORING STATIONS MAP ##########################################################################
# 
# # #FROM VAHYDRO:
# # sw_features <- read.csv(paste(site,"monitoring-stations-sw-export",sep=""))
# # gw_features <- read.csv(paste(site,"monitoring-stations-gw-export",sep=""))
# # sw.gg <- geom_point(data = sw_features,aes(x = longitude, y = latitude, color="aliceblue"),size=2, shape=17, show.legend = TRUE)
# # gw.gg <- geom_point(data = gw_features,aes(x = longitude, y = latitude, color="antiquewhite"),size=2, show.legend = TRUE)
# 
# ###FROM NWIS:
# #CURRENT/ACTIVE SURFACE WATER GAGES (Stream, Tidal, Lake, Canal)
# sw_features <- whatNWISsites(stateCd = "VA", 
#                              parameterCd = "00060",
#                              siteType = c("ST","ST-TS","LK","ST-CA"),
#                              siteStatus = "active")
# sw.sf <- st_as_sf(sw_features, coords = c("dec_long_va", "dec_lat_va"),crs = 4326)
# sw.sf %>% st_transform(crs=4326)
# sw.sf$ms_type <- "SW"
# 
# #CURRENT/ACTIVE GROUNDWATER WELLS
# gw_features <- whatNWISsites(stateCd = "VA", 
#                              parameterCd = "72019",
#                              siteStatus = "active")
# gw.sf <- st_as_sf(gw_features, coords = c("dec_long_va", "dec_lat_va"),crs = 4326)
# gw.sf %>% st_transform(crs=4326)
# gw.sf$ms_type <- "GW"
# 
# #COMBINE GAGES AND WELLS INTO SINGLE LAYER
# ms.sf <- rbind(sw.sf, gw.sf)
# ms.gg <- geom_sf(data = ms.sf,aes(color=ms_type, shape = ms_type),size=1.4, inherit.aes = FALSE, show.legend =TRUE)
# 
# #MAP
# monitoring_map <- basemap.obj + ms.gg +
#                   theme(legend.position = c(0.167, 0.89),
#                         legend.title=element_text(size=10),
#                         legend.text=element_text(size=8),
#                         aspect.ratio = 12.05/16) +
#                   guides(colour = guide_legend(override.aes = list(size = c(4, 3)))) +
#                   scale_color_manual(name = "Groundwater & Surface Water \n Monitoring Stations", 
#                                      values=c("brown4", "blue"),
#                                      labels=c("Observation Well", "Streamflow Gage")) +
#                   scale_shape_manual(name = "Groundwater & Surface Water \n Monitoring Stations", 
#                                      labels=c("Observation Well", "Streamflow Gage"),
#                                      values = c(19, 17))
#                   
# deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
# monitoring_map_draw <- ggdraw(monitoring_map)+deqlogo
# #ggsave(plot = monitoring_map_draw, file = paste0(export_path, "/awrr/2021/","MonitoringStationsMap.pdf",sep = ""), width=6.5, height=4.95) #Working map saves here
# ggsave(plot = monitoring_map_draw, file = paste0(export_path, "MonitoringStationsMap.pdf",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE


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
fips.gg <- geom_sf(data = fips.sf,aes(fill = factor(col)),lwd=0.4, alpha=0.5, inherit.aes = FALSE, show.legend =TRUE)



#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
gages <- read.csv(paste(base_url,"/drought-gage-feature-export",sep=""))

gage_geom_list <- list()
for (w in 1:length(gages[,1])) {
  gage_w <- gages[w,]
  
  gage_w_sf <- st_as_sf(gage_w, wkt = 'geom')
  gage_w_geom <- geom_sf(data = gage_w_sf,aes(geometry = geom,colour = color_list[1], shape=color_list[1], linetype=color_list[1]), inherit.aes = FALSE,size=3)
  gage_w_bbox <- st_bbox(gage_w_sf)
  gage_w_bbox <- data.frame(x = gage_w_bbox$xmin, y = gage_w_bbox$ymin)
  gage_w_label <- geom_label_repel(data = gage_w_bbox, aes(x = x, y = y, group = 1, label = intakes$name),size = 3,fill="white",box.padding =1,max.iter=20000)
  
  gage_geom_list <-  append(gage_geom_list, gage_w_geom) 
}
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
wells <- read.csv(paste(base_url,"/drought-well-feature-export",sep=""))

well_geom_list <- list()
for (w in 1:length(wells[,1])) {
  well_w <- wells[w,]
  
  well_w_sf <- st_as_sf(well_w, wkt = 'geom')
  well_w_geom <- geom_sf(data = well_w_sf,aes(geometry = geom,colour = color_list[2], shape=color_list[2], linetype=color_list[2]), inherit.aes = FALSE,size=3)
  well_w_bbox <- st_bbox(well_w_sf)
  well_w_bbox <- data.frame(x = well_w_bbox$xmin, y = well_w_bbox$ymin)
  well_w_label <- geom_label_repel(data = well_w_bbox, aes(x = x, y = y, group = 1, label = intakes$name),size = 3,fill="white",box.padding =1,max.iter=20000)
  
  well_geom_list <-  append(well_geom_list, well_w_geom) 
}
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
# RESERVOIRS

# reservoir_geom_list <- list()

LakeMoomaw <- om_get_feature(site, hydrocode = "nhdplus_120053459", bundle = 'waterbody', ftype = 'nhd_plus')
LakeMoomaw_sf <- st_as_sf(LakeMoomaw, wkt = 'geom')
LakeMoomaw_geom <- geom_sf(data = LakeMoomaw_sf,aes(geometry = geom,colour = color_list[3], shape=color_list[3], linetype=color_list[3]), inherit.aes = FALSE,size=1.25)

# gage_w_geom <- geom_sf(data = gage_w_sf,aes(geometry = geom,colour = color_list[1]), shape=19, inherit.aes = FALSE,size=3)

SmithMountainLake <- om_get_feature(site, hydrocode = "nhdplus_120053465", bundle = 'waterbody', ftype = 'nhd_plus')
SmithMountainLake_sf <- st_as_sf(SmithMountainLake, wkt = 'geom')
SmithMountainLake_geom <- geom_sf(data = SmithMountainLake_sf,aes(geometry = geom,colour = color_list[3], shape=color_list[3], linetype=color_list[3]), inherit.aes = FALSE,size=1.25)

LakeAnna <- om_get_feature(site, hydrocode = "nhdplus_120053458", bundle = 'waterbody', ftype = 'nhd_plus')
LakeAnna_sf <- st_as_sf(LakeAnna, wkt = 'geom')
LakeAnna_geom <- geom_sf(data = LakeAnna_sf,aes(geometry = geom,colour = color_list[3], shape=color_list[3], linetype=color_list[3]), inherit.aes = FALSE,size=1.25)

KerrReservoir <- om_get_feature(site, hydrocode = "nhdplus_167496465", bundle = 'waterbody', ftype = 'nhd_plus')
KerrReservoir_sf <- st_as_sf(KerrReservoir, wkt = 'geom')
KerrReservoir_geom <- geom_sf(data = KerrReservoir_sf,aes(geometry = geom,colour = color_list[3], shape=color_list[3], linetype=color_list[3]), inherit.aes = FALSE,size=1.25) #,linetype = "11"


# reservoir_geom_list <-  append(reservoir_geom_list, LakeMoomaw_geom) 

reservoir_geom_list <-  list(LakeMoomaw_geom, 
                             SmithMountainLake_geom, 
                             LakeAnna_geom,
                             KerrReservoir_geom) 

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
# states.gg <- baselayers.gg[[which(names(baselayers.gg) == "states.gg")]]
# colnames(states.gg)
# states <- read.table(paste(github_location,"HARParchive/GIS_layers/STATES.tsv",sep = '/'),sep = '\t')
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------


map.obj <- basemap.obj + fips.gg +
  guides(fill = guide_legend(ncol=2, order = 2)) +
  scale_fill_manual(name = "Drought Evaluation Regions",
                    values = c("khaki1","slateblue2","darkolivegreen2","blue4","chocolate1",
                               "darkcyan","darkkhaki","indianred1","aquamarine","lightpink",
                               "chartreuse4","dodgerblue","bisque1","white"),
                    labels = c("Big Sandy","Chowan","Eastern Shore","Middle James","New River",
                               "Northern Coastal Plain","Northern Piedmont","Northern Virginia",
                               "Roanoke","Shenandoah","Southeast Virginia","Upper James","York James","white"),
                    guides(shape = guide_legend(order = 1))
                    )+
  rivs.gg +
  res.gg +
  reservoir_geom_list +
  gage_geom_list +
  well_geom_list + 
  # reservoir_geom_list + 
  # scale_color_manual("Drought Indicator",
  #                    values = c("brown4", "blue", "black"),
  #                    labels = c("Surface Water", "Groundwater", "Reservoir")
  #                    ) +
  scale_color_manual("Drought Indicator",
                     labels = c("Surface Water", "Groundwater", "Reservoir"),
                     values = c("brown4", "blue", "black")) +
  scale_shape_manual("Drought Indicator",
                     labels = c("Surface Water", "Groundwater", "Reservoir"),
                     values = c(19, 17, NA)) +
  scale_linetype_manual("Drought Indicator",
                        labels = c("Surface Water", "Groundwater", "Reservoir"),
                        values = c("blank", "blank", "solid")) +
  theme(legend.position = c(0.34, 0.782),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8),
        aspect.ratio = 12.05/16,
        legend.direction = "vertical",
        legend.box = "horizontal"
  )


deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
drought_map_draw <- ggdraw(map.obj)+deqlogo 
ggsave(plot = drought_map_draw, file = paste0(export_path, "DroughtRegions.png",sep = ""), width=6.5, height=4.95) #FINAL MAP SAVES HERE

##############################################################################################
