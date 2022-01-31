##library('rgdal') #required for readOGR(), spTransform()
#  Please note that rgdal will be retired by the end of 2023,
#  plan transition to sf/stars/terra functions using GDAL and PROJ
#  at your earliest convenience.
#library('rgeos') #required for writeWKT()
#library('hydrotools') #required for RomDataSource
basepath<-'/var/www/R'
source('/var/www/R/config.R')
ds <- RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)


facmap <- function (facility_hydroid = 475973, 
                    scaler = 0.06,
                    plot_zoom = 13,
                    site = "http://deq1.bse.vt.edu:81/d.dh/",
                    export_path = "C:/Users/jklei/Desktop/GitHub/mapper/"
                    ) {


###################################################################################################### 
# LOAD FILES
######################################################################################################
basepath <- "/var/www/R/"
source(paste(basepath,"config.local.private",sep = '/'))
source(paste(hydro_tools,"GIS_functions/base.layers.R",sep = '/'))
source(paste(hydro_tools,"GIS_functions/base.map.R",sep = '/'))
if(!exists("baselayers")) {baselayers <- load_MapLayers(site = site)} #Load map layers if they're not already loaded in the RStudio environment

###################################################################################################### 
# RETRIEVE MPS AND DETERMINE MAPPING EXTENT
######################################################################################################
mps <- read.csv(paste(site,"feature-to-ref-features-export/",facility_hydroid,sep=""))
fac_name <- mps[1,]$parent_name

fac_sf <- st_as_sf(mps[1,], wkt = 'parent_geom')
fac_bbox <- st_bbox(fac_sf)
fac_lat <- fac_bbox$ymin 
fac_lon <- fac_bbox$xmin

extent = data.frame(x = c(fac_lon-scaler, fac_lon+scaler),
                    y = c(fac_lat-scaler, fac_lat+scaler))

######################################################################################################
### BASEMAP OBJECT
######################################################################################################
baselayers.gg <- base.layers(baselayers,extent=extent)
basemap.obj <- base.map(baselayers.gg,extent=extent,
                        plot_margin = c(0.16,0.2,0.16,-3.9), #top, right, bottom, left
                        plot_zoom = plot_zoom,
                        scale_bar = FALSE)

######################################################################################################
### PROCESS MAP LAYERS
######################################################################################################
color_list <- sort(colors())

# PROCESS RSegs
RSeg.csv <- baselayers[[which(names(baselayers) == "RSeg.csv")]]
RSeg_valid_geoms <- paste("SELECT * FROM 'RSeg.csv'WHERE geom != ''") # REMOVE ANY WITH EMPTY GEOMETRY FIELD (NEEDED PRIOR TO GEOPROCESSING) 
RSeg_layer <- sqldf(RSeg_valid_geoms)
RSeg_layer_sf <- st_as_sf(RSeg_layer, wkt = 'geom')
RSeg_layer_geom <- geom_sf(data = RSeg_layer_sf,aes(geometry = geom,colour = color_list[1]),
                           lwd=2,alpha=0, inherit.aes = FALSE,show.legend = "line")

#---------------------------------------------------------------------------------------------------
# #PROCESS NHDPlus FLOWLINES
# localpath <- paste(github_location,"/HARParchive/GIS_layers/VWP_projects/",sep="")
# epsg_code <- "4326"
# shp_path <- "Route_58"
# shp_layer_name <- "NHDPlus_Rt58"
# shp_layer_load <- readOGR(paste(localpath,shp_path,sep=""),shp_layer_name)
# shp_layer <-spTransform(shp_layer_load, CRS(paste("+init=epsg:",epsg_code,sep="")))
# shp_layer_wkt <- writeWKT(shp_layer)
# shp_layer.df <- data.frame(name = "NHDPlus_Flowlines",group = 1,geom = shp_layer_wkt)
# 
# nhdplus_flowlines_sf <- st_as_sf(shp_layer.df, wkt = 'geom')
# nhdplus_flowlines_geom <- geom_sf(data = nhdplus_flowlines_sf,aes(geometry = geom),color = "dodgerblue3",
#                                   lwd=0.75, inherit.aes = FALSE)

#---------------------------------------------------------------------------------------------------
# intakes <- sqldf(paste('SELECT *
#                     FROM mps
#                     WHERE bundle = "Surface Water Intake" AND
#                           fstatus = "active"
#   		              ',sep = ''))

intakes <- sqldf(paste('SELECT *
                    FROM mps
                    WHERE bundle = "Surface Water Intake"
  		              ',sep = ''))

# i <- 1
if (length(intakes[,1]) > 0){
  # intake_list <- list(geom = "",
  #                     label = "")
  intake_geom_list <- list()
  #intake_label_list <- list()
  
  for (i in 1:length(intakes[,1])) {
    intake_i <- intakes[i,]
    
    intake_i_sf <- st_as_sf(intake_i, wkt = 'geom')
    intake_i_geom <- geom_sf(data = intake_i_sf,aes(geometry = geom,colour = color_list[2]), inherit.aes = FALSE,size=3)
    intake_i_bbox <- st_bbox(intake_i_sf)
    intake_i_bbox <- data.frame(x = intake_i_bbox$xmin, y = intake_i_bbox$ymin)
    intake_i_label <- geom_label_repel(data = intake_i_bbox, aes(x = x, y = y, group = 1, label = intakes$name),size = 2,fill="white",box.padding =1,max.iter=20000)
    
    #intake_list$geom <- intake_list$geom + intake_i_geom
    #intake_list <- append(intake_list[[1]], intake_i_geom)
    
    intake_geom_list <-  append(intake_geom_list, intake_i_geom) 
    #intake_label_list <-  append(intake_label_list, intake_i_label) 
  }
  
}

#---------------------------------------------------------------------------------------------------
wells <- sqldf(paste('SELECT *
                    FROM mps
                    WHERE bundle = "Well" AND
                          fstatus = "active"
  		              ',sep = ''))
#w<-1
if (length(wells[,1]) > 0){
  well_geom_list <- list()
  for (w in 1:length(wells[,1])) {
    well_w <- wells[w,]
    
    well_w_sf <- st_as_sf(well_w, wkt = 'geom')
    well_w_geom <- geom_sf(data = well_w_sf,aes(geometry = geom,colour = color_list[3]), inherit.aes = FALSE,size=2)
    well_w_bbox <- st_bbox(well_w_sf)
    well_w_bbox <- data.frame(x = well_w_bbox$xmin, y = well_w_bbox$ymin)
    well_w_label <- geom_label_repel(data = well_w_bbox, aes(x = x, y = y, group = 1, label = intakes$name),size = 2,fill="white",box.padding =1,max.iter=20000)
    
    well_geom_list <-  append(well_geom_list, well_w_geom) 
  }
  
}

#---------------------------------------------------------------------------------------------------
# TRANSFERS
transfers <- sqldf(paste('SELECT *
                    FROM mps
                    WHERE bundle = "Transfer"
  		              ',sep = ''))

transfers_hydrocodes <- transfers$hydrocode
transfers_codes <- substring(transfers_hydrocodes, 11,19)

conveyances <- data.frame()

# basepath<-'/var/www/R'
# source('/var/www/R/config.R')
# ds <- RomDataSource$new(site, rest_uname)
# ds$get_token(rest_pw)

#c<-2
for (c in 1:length(transfers_codes)) {
  
  conveyance_hydrocode <- paste("vwuds_conv-",transfers_codes[c],sep="")
  conv <- om_get_feature(site, 
                         hydrocode = conveyance_hydrocode, 
                         bundle = 'conveyance', 
                         ftype = 'water_transfer')
  if (length(conv)==1){next}
  conveyances <- rbind(conveyances, conv) 
}

if (nrow(conveyances) > 0){
  conveyance_geom_list <- list()
  for (c in 1:length(conveyances[,1])) {
    conveyance_c <- conveyances[c,]
    
    conveyance_c_sf <- st_as_sf(conveyance_c, wkt = 'geom')
    conveyance_c_geom <- geom_sf(data = conveyance_c_sf,aes(geometry = geom,colour = color_list[4]), inherit.aes = FALSE,size=1.25,linetype = "11")
    conveyance_c_bbox <- st_bbox(conveyance_c_sf)
    conveyance_c_bbox <- data.frame(x = conveyance_c_bbox$xmin, y = conveyance_c_bbox$ymin)
    conveyance_c_label <- geom_label_repel(data = conveyance_c_bbox, aes(x = x, y = y, group = 1, label = conveyances$name),size = 2,fill="white",box.padding =1,max.iter=20000)
    
    conveyance_geom_list <-  append(conveyance_geom_list, conveyance_c_geom) 
  }
}
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------

# LOAD bounding box (needed for scalebar)
bb.gg <- baselayers.gg[[which(names(baselayers.gg) == "bb.gg")]]

###################################################################################################### 
# ADD MAP LAYERS TO BASEMAP OBJECT
######################################################################################################
# minorbasin_map <- basemap.obj + 
#   minorbasin_layer + 
#   RSeg_layer_geom + 
#   well_geom_list + 
#   intake_geom_list + 
#   ###nhdplus_flowlines_geom +
#   # well_1_geom + 
#   # #well_1_point_label +
#   # well_2_geom + 
#   # #well_2_point_label +
#   # well_3_geom + 
#   # #well_3_point_label +
#   ########intake_1_geom + intake_1_label +
#   ###intake_2_geom + intake_2_label +
#   ###intake_3_geom + intake_3_label +
#   # transfer_geom + 
#   # #transfer_point_label +
#   # usgs_02055000_geom + usgs_02055000_point_label +
#   # usgs_02054530_geom + usgs_02054530_point_label +
#   
#   theme(legend.position = c(1.16, 0.833),
#         legend.title=element_text(size=10),
#         legend.text=element_text(size=10)) +
# 
#   # scale_colour_manual(name = c("Legend"),
#   #                     values = c("gray30","dodgerblue3","black"),
#   #                     labels = c("River Segments","NHDPlus","Intake"),
#   #                     guide = guide_legend(override.aes = list(linetype = c("solid","solid","blank"), 
#   #                                                              alpha = c(1,1,1),
#   #                                                              shape = c(NA,NA,16)))) +
#   scale_colour_manual(name = c("Legend"),
#                       values = c("gray30","black","orange"),
#                       labels = c("River Segments","Intake","Well"),
#                       guide = guide_legend(override.aes = list(linetype = c("solid","blank","blank"), 
#                                                                alpha = c(1,1,1),
#                                                                shape = c(NA,16,15)))) +
#                                             
#   ggsn::scalebar(bb.gg, location = 'bottomleft', dist = 1, dist_unit = 'mi',transform = TRUE, model = 'WGS84',
#                  st.bottom=FALSE,st.size = 3.5, st.dist = 0.0285,anchor = c(x = extent$x[1]+0.045,y = extent$y[1]+0.002))
#   

#######################
map <- basemap.obj + 
  #minorbasin_layer + 
  RSeg_layer_geom + 
    theme(legend.position = c(1.16, 0.833),
          legend.title=element_text(size=10),
          legend.text=element_text(size=10)) +
  ggsn::scalebar(bb.gg, location = 'bottomleft', dist = 1, dist_unit = 'mi',transform = TRUE, model = 'WGS84',
                 st.bottom=FALSE,st.size = 3.5, st.dist = 0.0285,anchor = c(x = extent$x[1]+0.045,y = extent$y[1]+0.002))


if (length(wells[,1]) > 0){map <- map + well_geom_list}
if (length(intakes[,1]) > 0){map <- map + intake_geom_list}
if (nrow(conveyances) > 0){map <- map + conveyance_geom_list}


if (length(intakes[,1]) > 0 & length(wells[,1]) > 0 & nrow(conveyances) > 0){
  #leg_values = c("gray30","black","orange","gray40")
  leg_values = c("gray30","darkorchid3","orange","gray40")
  leg_labels = c("River Segments","Intake","Well","Transfer")
  leg_linetype = c("solid","blank","blank","dotted")
  leg_alpha = c(1,1,1,1)
  leg_shape = c(NA,16,15,NA)
} else if (length(intakes[,1]) > 0 & length(wells[,1]) > 0){
  #leg_values = c("gray30","black","orange")
  leg_values = c("gray30","darkorchid3","orange")
  leg_labels = c("River Segments","Intake","Well")
  leg_linetype = c("solid","blank","blank")
  leg_alpha = c(1,1,1)
  leg_shape = c(NA,16,15)
} else if (length(intakes[,1]) > 0){
  #leg_values = c("gray30","black")
  leg_values = c("gray30","darkorchid3")
  leg_labels = c("River Segments","Intake")
  leg_linetype = c("solid","blank")
  leg_alpha = c(1,1)
  leg_shape = c(NA,16)
}

map <- map + scale_colour_manual(name = c("Legend"),
                                 values = leg_values,
                                 labels = leg_labels,
                                 guide = guide_legend(override.aes = list(linetype = leg_linetype,alpha = leg_alpha,shape = leg_shape)))

######################################################################################################
deqlogo <- draw_image(paste(github_location,'/HARParchive/GIS_layers/HiResDEQLogo.tif',sep=''),scale = 0.175, height = 1, x = -.388, y = -0.413) #LEFT BOTTOM LOGO
map <- ggdraw(map)+deqlogo
ggsave(plot = map, file = paste0(export_path,gsub(" ","_",fac_name),"_location_map.png",sep = ""), width=6.5, height=4.95)
######################################################################################################

} #close facmap function
