library('rgdal') #required for readOGR(), spTransform()
library('sqldf') #required for SQL queries
library('geosphere') #required for areaPolygon()

#localpath <-"C:/Users/nrf46657/Desktop/VAHydro Development/shapefiles/NC/"
localpath <- "C:/Users/jklei/Desktop/GitHub/hydro-tools/GIS_LAYERS/"
epsg_code <- "4326" #WGS 84

#############################################################################################
# Calculate Area of Polygons in .gdb File
#############################################################################################

  poly_path <- "MinorBasins.gdb" #name of .gdb
  poly_layer_name <- 'MinorBasins' #layer within the .gdb

  poly_layer_load <- readOGR(paste(localpath,poly_path,sep=""),layer=poly_layer_name)
  plot(poly_layer_load)
  area_sqm <- areaPolygon(poly_layer_load)
  
  area_df <- data.frame(name = poly_layer_load$name,area_sqm = area_sqm)  
  
  area.sql <- paste("SELECT *,
                    area_sqm/1000000 AS area_sqkm,
                    area_sqm/2589988.110336 AS area_sqmi
                    FROM area_df",sep="")
  area_df <- sqldf(area.sql)

  