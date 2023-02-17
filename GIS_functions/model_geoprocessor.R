library("hydrotools")
library("data.table")
library("sp")
basepath='/var/www/R';
source(paste(basepath,'config.R',sep='/'))
ds = RomDataSource$new(site, rest_uname)
ds$get_token(rest_pw)
source(paste(hydro_tools_location,'/R/om_vahydro_metric_grid.R', sep = ''));

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
  watershed_feature <- RomFeature$new(ds, list(hydroid = watersheds$featureid[1]), TRUE)
  watershed_wkt <- watershed_feature$geom
  polygons_sp <- sp::SpatialPolygonsDataFrame(readWKT(watershed_wkt), data=data.frame(hydrocode=watersheds$hydrocode[1]))
  
  if (length(watersheds[,1]) > 1){
    #i<-1
    for (i in 2:length(watersheds[,1])){
      print(paste(i," in ",length(watersheds[,1]),sep=""))
      featureid <- watersheds$featureid[i]
      hydrocode <- watersheds$hydrocode[i]
      
      watershed_feature <- RomFeature$new(ds, list(hydroid = featureid), TRUE)
      watershed_wkt <- watershed_feature$geom
      polygons_sp <- rbind(polygons_sp, sp::SpatialPolygonsDataFrame(readWKT(watershed_wkt), data.frame(hydrocode=hydrocode)))
    }
  }
  return(polygons_sp)
}
