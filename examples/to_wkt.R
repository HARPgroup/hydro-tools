library('rgdal') #required for readOGR(), spTransform()
library('rgeos') #required for writeWKT()



localpath <-"C:/Users/nrf46657/Desktop/VAHydro Development/shapefiles/NC/"
epsg_code <- "4326" #WGS 84

#############################################################################################
# .gdb TO WKT
#############################################################################################

  poly_path <- "NC_COUNTIES.gdb" #name of .gdb
  poly_layer_name <- 'NC_COUNTIES' #layer within the .gdb


  poly_layer_load <- readOGR(paste(localpath,poly_path,sep=""),layer=poly_layer_name)
  poly_layer <-spTransform(poly_layer_load, CRS(paste("+init=epsg:",epsg_code,sep="")))
  writeWKT(poly_layer)

#############################################################################################
# .shp TO WKT
#############################################################################################
  
  shp_path <- "NC_COUNTY_SHAPES" #location of .shp
  shp_layer_name <- "WataugaCounty" #.shp file name (with ".shp" extension left off)
  
  shp_layer_load <- readOGR(paste(localpath,shp_path,sep=""),shp_layer_name)
  shp_layer <-spTransform(shp_layer_load, CRS(paste("+init=epsg:",epsg_code,sep=""))) 
  shp_layer_wkt <- writeWKT(shp_layer)

  
 