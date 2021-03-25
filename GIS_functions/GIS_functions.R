library("rgdal")

#Spatial containment function 
# Supply 1) file path to .gdb containing layer of polygon features 
#        2) polygon layer of interest within the .gdb above (must have "Name" and "Code" attributes)
#        3) Large SpatialPointsDataFrame of point features with column of coordinates
#        4) HUC6_code code of interest example: '020700' or 'all' for all
#        5) epsg code of interest, default to 4326
# Function returns a Large SpatialPointsDataFrame
sp_contain <- function(poly_path,poly_layer_name,HUC6_code,point_df,epsg_code = "4326"){
  
  start_time <- Sys.time()
  print(paste("Start time: ",start_time,sep=""))
  
  # read in polygons
  poly_layer_load <- readOGR(poly_path,layer=poly_layer_name)
  poly_layer <-spTransform(poly_layer_load, CRS(paste("+init=epsg:",epsg_code,sep="")))
  
  if (HUC6_code != 'all') {
     poly_layer <- poly_layer[poly_layer$HUC6 == HUC6_code,]
  }
  plot(poly_layer)
  
  # tell R that point_df coordinates are in the same lat/lon reference system
  # as the poly_layer data 
  proj4string(point_df) <- proj4string(poly_layer)
  
  # combine is.na() with over() to do the containment test; note that we
  # need to "demote" point_df to a SpatialPolygons object first
  inside.poly_layer <- !is.na(over(point_df, as(poly_layer, "SpatialPolygons")))
  
  # what fraction of points are inside a polygon?
  print(paste("Fraction of points within polygon layer: ", round(mean(inside.poly_layer),3),sep=""))
  
  # use 'over' again, this time with poly_layer as a SpatialPolygonsDataFrame
  # object, to determine which polygon (if any) contains each point, and
  # store the polygon name and code as attributes of the point data
  point_df$Poly_Name <- over(point_df, poly_layer)$Name
  point_df$Poly_Code <- over(point_df, poly_layer)$Code
  
  end_time <- Sys.time()
  print(paste("Time elapsed: ",round(end_time-start_time,3),sep=""))
  
  return(point_df)
}





# poly_path <- paste(localpath,gdb_path,sep="")
# poly_layer_name <- layer_name
# point_df <- data_sp 
  
sp_contain_mb <- function(poly_path,poly_layer_name,point_df,epsg_code = "4326"){
  
  start_time <- Sys.time()
  print(paste("Start time: ",start_time,sep=""))
  
  # read in polygons
  poly_layer_load <- readOGR(poly_path,layer=poly_layer_name)
  proj4string(poly_layer_load)<- CRS("+proj=longlat +datum=WGS84")
  
  poly_layer <-spTransform(poly_layer_load, CRS(paste("+init=epsg:",epsg_code,sep="")))

  
  # tell R that point_df coordinates are in the same lat/lon reference system
  # as the poly_layer data 
  proj4string(point_df) <- proj4string(poly_layer)
  
  # combine is.na() with over() to do the containment test; note that we
  # need to "demote" point_df to a SpatialPolygons object first
  inside.poly_layer <- !is.na(over(point_df, as(poly_layer, "SpatialPolygons")))
  
  # what fraction of points are inside a polygon?
  print(paste("Fraction of points within polygon layer: ", round(mean(inside.poly_layer),3),sep=""))
  
  # use 'over' again, this time with poly_layer as a SpatialPolygonsDataFrame
  # object, to determine which polygon (if any) contains each point, and
  # store the polygon name and code as attributes of the point data
  point_df$MinorBasin_Name <- over(point_df, poly_layer)$name
  point_df$MinorBasin_Code <- over(point_df, poly_layer)$code
  
  end_time <- Sys.time()
  print(paste("Time elapsed: ",round(end_time-start_time,3),sep=""))
  
  return(point_df)
}



# poly_path <- gdb_path
# poly_layer_name <- layer_name
# point_df <- data_sp 

sp_contain_rseg <- function(poly_path,poly_layer_name,point_df,epsg_code = "4326"){
  
  start_time <- Sys.time()
  print(paste("Start time: ",start_time,sep=""))
  
  # read in polygons
  poly_layer_load <- readOGR(poly_path,layer=poly_layer_name)
  proj4string(poly_layer_load)<- CRS("+proj=longlat +datum=WGS84")
  
  poly_layer <-spTransform(poly_layer_load, CRS(paste("+init=epsg:",epsg_code,sep="")))
  
  
  # tell R that point_df coordinates are in the same lat/lon reference system
  # as the poly_layer data 
  proj4string(point_df) <- proj4string(poly_layer)
  
  # combine is.na() with over() to do the containment test; note that we
  # need to "demote" point_df to a SpatialPolygons object first
  inside.poly_layer <- !is.na(over(point_df, as(poly_layer, "SpatialPolygons")))
  
  # what fraction of points are inside a polygon?
  print(paste("Fraction of points within polygon layer: ", round(mean(inside.poly_layer),3),sep=""))
  
  # use 'over' again, this time with poly_layer as a SpatialPolygonsDataFrame
  # object, to determine which polygon (if any) contains each point, and
  # store the polygon name and code as attributes of the point data
  point_df$RSeg_Name <- over(point_df, poly_layer)$name
  point_df$RSeg_Code <- over(point_df, poly_layer)$code
  
  end_time <- Sys.time()
  print(paste("Time elapsed: ",round(end_time-start_time,3),sep=""))
  
  return(point_df)
}





sp_contain_vahydro_rseg <- function(poly_path,poly_layer_name,point_df,epsg_code = "4326"){
  
  start_time <- Sys.time()
  print(paste("Start time: ",start_time,sep=""))
  
  # read in polygons
  poly_layer_load <- readOGR(poly_path,layer=poly_layer_name)
  proj4string(poly_layer_load)<- CRS("+proj=longlat +datum=WGS84")
  
  poly_layer <-spTransform(poly_layer_load, CRS(paste("+init=epsg:",epsg_code,sep="")))
  
  
  # tell R that point_df coordinates are in the same lat/lon reference system
  # as the poly_layer data 
  proj4string(point_df) <- proj4string(poly_layer)
  
  # combine is.na() with over() to do the containment test; note that we
  # need to "demote" point_df to a SpatialPolygons object first
  inside.poly_layer <- !is.na(over(point_df, as(poly_layer, "SpatialPolygons")))
  
  # what fraction of points are inside a polygon?
  print(paste("Fraction of points within polygon layer: ", round(mean(inside.poly_layer),3),sep=""))
  
  # use 'over' again, this time with poly_layer as a SpatialPolygonsDataFrame
  # object, to determine which polygon (if any) contains each point, and
  # store the polygon name and code as attributes of the point data
  point_df$VAHydro_RSeg_Name <- over(point_df, poly_layer)$name
  point_df$VAHydro_RSeg_Code <- over(point_df, poly_layer)$code
  
  end_time <- Sys.time()
  print(paste("Time elapsed: ",round(end_time-start_time,3),sep=""))
  
  return(point_df)
}