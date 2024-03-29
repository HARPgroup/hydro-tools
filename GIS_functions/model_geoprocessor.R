library("data.table")
library("sp")

# function to retrieve & format model segment metric & geometry data
model_geoprocessor <- function(ds,scenario_info,segswhere) {
  
  model_version <- scenario_info[1]
  runid <- scenario_info[2]
  
  # retrieve segments & metric data
  model_data <- data.frame(
    'model_version' = c(model_version),
    'runid' = c(runid),
    'metric' = c('Qout'),
    'runlabel' = c('Qout')
  )
  model_data <- om_vahydro_metric_grid(metric, model_data, ds = ds)
  
  watersheds <- sqldf(paste0("
  SELECT *
  FROM model_data
  WHERE ",segswhere,";
  "))
  #####################################################################
  # retrieve & format geometry data
  watershed_feature <- RomFeature$new(ds, list(hydroid = watersheds$featureid[1]), TRUE)
  watershed_wkt <- watershed_feature$geom
  polygons_sp <- sp::SpatialPolygonsDataFrame(readWKT(watershed_wkt), data=data.frame(hydrocode=watersheds$hydrocode[1],riverseg=watersheds$riverseg[1]))
  
  if (length(watersheds[,1]) > 1){
    #i<-1
    for (i in 2:length(watersheds[,1])){
      print(paste(i," in ",length(watersheds[,1]),sep=""))
      featureid <- watersheds$featureid[i]
      hydrocode <- watersheds$hydrocode[i]
      riverseg <- watersheds$riverseg[i]
      
      watershed_feature <- RomFeature$new(ds, list(hydroid = featureid), TRUE)
      watershed_poly <- sp::SpatialPolygonsDataFrame(readWKT(watershed_feature$geom), data.frame(watershed_feature$hydrocode) )
      # this may be a more elegant way to do this, or at least a bit more readale
      # we would need to change "i in 2:", to "i in 1:" 
      if (is.logical(poly2)) {
        poly2 <- watershed_poly
      } else {
        poly2 <- rbind(poly2, watershed_poly)
      }
      watershed_wkt <- watershed_feature$geom
      polygons_sp <- rbind(polygons_sp, sp::SpatialPolygonsDataFrame(readWKT(watershed_wkt), data.frame(hydrocode=hydrocode,riverseg=riverseg)))
    }
  }
  return(polygons_sp)
}
