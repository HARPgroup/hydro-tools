library("rgdal")
library('sf')

#poly_path <- "C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/hydro-tools/GIS_LAYERS/WBD.gdb"
#poly_layer_name <- "WBDHU6"
#poly_layer_name <- "WBDHU8"
#poly_layer_name <- "WBDHU10"
#point_df <- data_sp
  

#Spatial containment function 
# Supply 1) file path to .gdb containing layer of polygon features 
#        2) polygon layer of interest within the .gdb above (must have "Name" and "Code" attributes)
#        3) Large SpatialPointsDataFrame of point features with column of coordinates
#        4) epsg code of interest, default to 4326
# Function returns a Large SpatialPointsDataFrame
sp_contain <- function(poly_path,poly_layer_name,point_df,epsg_code = "4326"){
  
  start_time <- Sys.time()
  print(paste("Start time: ",start_time,sep=""))
  
  # read in polygons
  st <- st_read(poly_path, poly_layer_name)
  sp <- as(st, "Spatial")
  poly_layer <- spTransform(sp, CRS(paste("+init=epsg:",epsg_code,sep="")))
  
  #head(poly_layer)
  #plot(poly_layer)
  
  # tell R that point_df coordinates are in the same lat/lon reference system as the poly_layer data 
  proj4string(point_df) <- proj4string(poly_layer)
  
  # combine is.na() with over() to do the containment test; note that we
  # need to "demote" point_df to a SpatialPolygons object first
  #OLD: inside.poly_layer <- !is.na(over(point_df, as(poly_layer, "SpatialPolygons")))
  inside.poly_layer <- !is.na(over(point_df, as(poly_layer, "SpatialPolygons")))
  
  # what fraction of points are inside a polygon?
  print(paste("Fraction of points within polygon layer: ", round(mean(inside.poly_layer),3),sep=""))
  
  # use 'over' again, this time with poly_layer as a SpatialPolygonsDataFrame
  # object, to determine which polygon (if any) contains each point, and
  # store the polygon name and code as attributes of the point data
  point_df$Poly_Name <- over(point_df, poly_layer)$NAME
  #point_df$Poly_Code <- over(point_df, poly_layer)$HUC6
  #point_df$Poly_Code <- over(point_df, poly_layer)$HUC8
  #point_df$Poly_Code <- over(point_df, poly_layer)$HUC10
  if (layer_name == 'WBDHU6') {point_df$Poly_Code <- over(point_df, poly_layer)$HUC6} 
  if (layer_name == 'WBDHU8') {point_df$Poly_Code <- over(point_df, poly_layer)$HUC8}
  if (layer_name == 'WBDHU10') {point_df$Poly_Code <- over(point_df, poly_layer)$HUC10}  
  
  print(point_df)
  
  end_time <- Sys.time()
  print(paste("Time elapsed: ",round(end_time-start_time,3),sep=""))
  
  return(point_df)
}

