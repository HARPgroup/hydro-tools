# function to retrieve & format model segment metric & geometry data
model_geoprocessor <- function(scenario,segswhere) {
  
  model_version <- scenario[1]
  runid <- scenario[2]
  
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
  vahydro_export <- paste(site,"watershed-features-wkt","vahydro",watersheds$hydrocode[1],sep="/")
  watershed_feature <-data.table::fread(vahydro_export, header = T)
  watershed_wkt <- watershed_feature$wkt
  polygons_sp <- sp::SpatialPolygonsDataFrame(readWKT(watershed_wkt), data=data.frame(hydrocode=watersheds$hydrocode[1]))
  
  if (length(watersheds[,1]) > 1){
    #i<-1
    for (i in 2:length(watersheds[,1])){
      print(paste(i," in ",length(watersheds[,1]),sep=""))
      hydrocode_i <- watersheds$hydrocode[i]
      vahydro_export <- paste(site,"watershed-features-wkt","vahydro",hydrocode_i,sep="/")
      watershed_feature <-data.table::fread(vahydro_export, header = T)
      watershed_wkt <- watershed_feature$wkt
      polygons_sp <- rbind(polygons_sp, sp::SpatialPolygonsDataFrame(readWKT(watershed_wkt), data.frame(hydrocode=hydrocode_i)))
    }
  }
  return(polygons_sp)
}
