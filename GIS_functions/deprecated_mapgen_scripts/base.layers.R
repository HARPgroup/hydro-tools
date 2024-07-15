library(sqldf)

#PROCESS BASE LAYERS
base.layers <- function(baselayers,extent = data.frame(x = c(-84, -75),y = c(35.25, 40.6))){
  
  # # LOAD MAP LAYERS FROM THE baselayers LIST 
  STATES <- baselayers[[which(names(baselayers) == "STATES")]]
  MinorBasins.csv <- baselayers[[which(names(baselayers) == "MinorBasins.csv")]]
  RSeg.csv <- baselayers[[which(names(baselayers) == "RSeg.csv")]]
  MajorRivers.csv <- baselayers[[which(names(baselayers) == "MajorRivers.csv")]]
  fips.csv <- baselayers[[which(names(baselayers) == "fips.csv")]]
  WBDF <- baselayers[[which(names(baselayers) == "WBDF")]]
  
  
  print(extent)
  
  ### BOUNDING BOX ##########
  bb=st_as_sf(extent, coords = c(2,1)) ## Flipping the lat/lon

  ## Set up a geometry of the bbox so things can be clipped based on it
  bbdf <- paste0("POLYGON((",extent$x[1]," ",extent$y[1],",",extent$x[2]," ",extent$y[1],",",extent$x[2]," ",extent$y[2],",",extent$x[1]," ",extent$y[2],",",extent$x[1]," ",extent$y[1],"))")
  bbdf <- st_as_sfc(bbdf)
  
   ### PROCESS STATES LAYER  ############################################################################
  
  # NEED TO REMOVE INDIANA DUE TO FAULTY GEOM
  STATES <- sqldf(paste('SELECT * FROM STATES WHERE state != "IN"',sep=""))
  
  ## Convert the dataframe into an sf df by its geom column
  STATES <- st_as_sf(STATES,wkt=2)
  
  ## Add an ID field 
  STATES$id <- as.numeric(rownames(STATES))
  
  for (i in 1:length(STATES$state)) {
    ## Clip it to the bounding box
    state_geom_clip <- st_intersection(bbdf,STATES$geom[i])
    
    ## Check if the state is entirely out of bbox
    if (length(st_area(state_geom_clip)) == 0) {
      ## Set the ID field to NA, so it can be filtered later
      STATES$id[i] <- NA
      next
    }

    STATES$geom[i] <- state_geom_clip
  }
  
  #REMOVE THOSE STATES THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
  STATES <- STATES[!is.na(STATES$id),]
  
  STATES <- st_set_crs(STATES,4326)

  ### PROCESS Minor Basin LAYER  #######################################################################
  
  mb_data <- MinorBasins.csv
  
  mb_data$id <- as.numeric(rownames(mb_data))
  
  ## Convert the dataframe into an sf df by its geom column
  mb_data <- st_as_sf(mb_data,wkt = 4)
  
  for (i in 1:length(mb_data$code)) {
    ## Clip based on bbox df
    mb_geom_clip <- st_intersection(bbdf,mb_data$geom[i])
    
    if (length(st_area(mb_geom_clip)) == 0) {
      ## Set the ID field to NA, so it can be filtered later
      mb_data$id[i] <- NA
      next
    }

    mb_data$geom[i] <- mb_geom_clip
  }
  
  #REMOVE THOSE BASINS THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
  mb_data <- mb_data[!is.na(mb_data$id),]
  
  mb_data <- st_set_crs(mb_data,4326)
 
  ### PROCESS FIPS LAYER  #############################################################################
  
  fips_data <- fips.csv

  #EXCLUDE NC LOCALITIES
  fips_data <- sqldf('SELECT *
              FROM fips_data
              WHERE fips_code NOT LIKE "3%"')
  
  fips_data$id <- as.numeric(rownames(fips_data))
  
  ## Converting to an sf df
  fips_data <- st_as_sf(fips_data,wkt = 7)
  
  for (i in 1:length(fips_data$fips_hydroid)) {
    ## Clip based on bbox df
    mb_geom_clip <- st_intersection(bbdf,fips_data$fips_geom[i])
    
    if (length(st_area(mb_geom_clip)) == 0) {
      ## Set the ID field to NA, so it can be filtered later
      fips_data$id[i] <- NA
      next
    }
    
    fips_data$fips_geom[i] <- mb_geom_clip
  }
  
  #REMOVE THOSE FIPS THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
  fips_data <- fips_data[!is.na(fips_data$id),]
  
  fips_data <- st_set_crs(fips_data,4326)
  
  ### PROCESS MajorRivers.csv LAYER  ###################################################################
  
  rivs_layer <- MajorRivers.csv
  
  # rivs_layer_sql <- paste('SELECT *
  #             FROM rivs_layer
  #             WHERE GNIS_NAME = "New River" OR GNIS_NAME = "South Fork New River"'
  #                         ,sep="")
  # rivs_layer <- sqldf(rivs_layer_sql)
  
  rivs_layer$id <- rivs_layer$feature
  rivs_layer <- st_as_sf(rivs_layer,wkt = 3)
  
  
  for (i in 1:length(rivs_layer$feature)) {
    ## Clip based on bbox df
    mb_geom_clip <- st_intersection(bbdf,rivs_layer$geom[i])
    
    if (length(st_area(mb_geom_clip)) == 0) {
      ## Set the ID field to NA, so it can be filtered later
      rivs_layer$id[i] <- NA
      next
    }
    
    rivs_layer$geom[i] <- mb_geom_clip
  }
  
  #REMOVE THOSE RIVS THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
  rivs_layer <- rivs_layer[!is.na(rivs_layer$id),]
  
  rivs_layer <- st_set_crs(rivs_layer,4326)
  
  ## Setting a CRS for the bbox
  bbdf <- st_set_crs(bbdf,4326)
  
  ### Creating final object #############
  baselayers.gg <- list("bb.gg" = bbdf, 
                        "states.gg" = STATES,
                        "mb.gg" = mb_data,
                        "fips.gg" = fips_data,
                        "rivs.gg" = rivs_layer,
                        "reservoirs.gg" = WBDF
                        #"ifim.gg" = ifim.df
  )
  return(baselayers.gg)
  
}

#LOAD BASE LAYERS
load_MapLayers <- function(site,localpath = tempdir()){
  library(ggplot2)
  # library(rgeos)
  # library(ggsn)
  # library(rgdal) # needed for readOGR()
  library(dplyr) # needed for case_when()
  library(sf) # needed for st_read()
  library(sqldf)
  library(kableExtra)
  library(viridis) #magma
  #library(wicket) #wkt_centroid() (package was removed from CRAN on 1/27/21)
  # require(wellknown) #Replaces the wicket package for wkt_centroid() ##Removed from CRAN 3/29/23
  library(cowplot) #plot static legend
  library(magick) #plot static legend
  library(ggrepel) #needed for geom_text_repel()
  library(ggmap) #used for get_stamenmap, get_map
  
  #DOWNLOAD STATES AND MINOR BASIN LAYERS DIRECT FROM GITHUB
  print(paste("DOWNLOADING STATES AND MINOR BASIN LAYERS DIRECT FROM GITHUB...",sep=""))  
    
  if(!exists("STATES")) {  
    state_path <- paste(localpath,"/hydro-tools/GIS_functions/deprecated_mapgen_scripts/AWRR_geom_data/STATES.csv", sep = '')
    
    #file downloaded into local directory, as long as file exists it will not be re-downloaded
    if (file.exists(state_path)) {
      print("__STATES LAYER PREVIOUSLY DOWNLOADED")
      STATES <- read.csv(state_path)
    } else {
      STATES_item <- "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/STATES.tsv"
      STATES_filename <- "STATES.tsv"
      
      print(paste("__DOWNLOADING STATES LAYER", sep = ''))
      destfile <- paste(localpath,STATES_filename,sep="\\")
      download.file(STATES_item, destfile = destfile, method = "libcurl")
      print(paste("__LOADING STATES LAYER...", sep = ''))
      STATES <- read.csv(file=paste(localpath,STATES_filename,sep="\\"), header=TRUE, sep="\t")
    }
    #read csv from local directory
    print(paste("__COMPLETE!", sep = ''))
  }  
  
  minorbasin_path <- paste(localpath,"/hydro-tools/GIS_functions/deprecated_mapgen_scripts/AWRR_geom_data/MinorBasins.csv", sep = '')
  
  if(!exists("MinorBasins.csv")) {  
    #file downloaded into local directory, as long as file exists it will not be re-downloaded
    if (file.exists(minorbasin_path)) {
      print("__MinorBasins.csv LAYER PREVIOUSLY DOWNLOADED")
      MinorBasins.csv <- read.csv(minorbasin_path)
      
    } else {
      MinorBasins.csv_item <- "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/MinorBasins.csv"
      MinorBasins.csv_filename <- "MinorBasins.csv.tsv"
      print(paste("__DOWNLOADING MinorBasins.csv LAYER", sep = ''))
      destfile <- paste(localpath,MinorBasins.csv_filename,sep="\\")
      download.file(MinorBasins.csv_item, destfile = destfile, method = "libcurl")
      #read csv from local directory
      print(paste("__LOADING MinorBasins.csv LAYER...", sep = ''))
      MinorBasins.csv <- read.csv(file=paste(localpath,MinorBasins.csv_filename,sep="\\"), header=TRUE, sep=",")
      print(paste("__COMPLETE!", sep = ''))  
    }

  }
  
  RSeg_path <- paste(localpath,"/hydro-tools/GIS_functions/deprecated_mapgen_scripts/AWRR_geom_data/RSeg.csv", sep = '')
  
  #DOWNLOAD RSEG LAYER DIRECT FROM VAHYDRO
  if(!exists("RSeg.csv")) {  
    #file downloaded into local directory, as long as file exists it will not be re-downloaded
    if (file.exists(RSeg_path)) {
      print(paste("__RSeg.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
      RSeg.csv <- read.csv(RSeg_path)
    } else {
      print(paste("DOWNLOADING RSEG LAYER DIRECT FROM VAHYDRO...",sep=""))
      RSeg.csv_item <- paste(site,"vahydro_riversegs_export",sep="")
      RSeg.csv_filename <- "RSeg.csv"
      print(paste("__DOWNLOADING RSeg.csv LAYER", sep = ''))
      destfile <- paste(localpath,RSeg.csv_filename,sep="\\")
      download.file(RSeg.csv_item, destfile = destfile, method = "libcurl")
      #read csv from local directory
      print(paste("__LOADING RSeg.csv LAYER...", sep = ''))
      RSeg.csv <- read.csv(file=paste(localpath,RSeg.csv_filename,sep="\\"), header=TRUE, sep=",")
      print(paste("__COMPLETE!", sep = '')) 
    }
 
  }
  
  MajorRivers_path <- paste(localpath,"/hydro-tools/GIS_functions/deprecated_mapgen_scripts/AWRR_geom_data/MajorRivers.csv", sep = '')
  
  #DOWNLOAD MAJORRIVERS LAYER DIRECT FROM GITHUB
  if(!exists("MajorRivers.csv")) {  
       #file downloaded into local directory, as long as file exists it will not be re-downloaded
    if (file.exists(MajorRivers_path)) {
      print(paste("__MajorRivers.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
      MajorRivers.csv <- read.csv(MajorRivers_path)
    } else {
      print(paste("DOWNLOADING MAJORRIVERS LAYER DIRECT FROM GITHUB...",sep=""))
      MajorRivers.csv_item <- "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/MajorRivers.csv"
      MajorRivers.csv_filename <- "MajorRivers.csv"
      
      print(paste("__DOWNLOADING MajorRivers.csv LAYER", sep = ''))
      destfile <- paste(localpath,MajorRivers.csv_filename,sep="\\")
      download.file(MajorRivers.csv_item, destfile = destfile, method = "libcurl")
      #read csv from local directory
      print(paste("__LOADING MajorRivers.csv LAYER...", sep = ''))
      MajorRivers.csv <- read.csv(file=paste(localpath,MajorRivers.csv_filename,sep="\\"), header=TRUE, sep=",")
      print(paste("__COMPLETE!", sep = '')) 
      
    }
 
  }
  
  #DOWNLOAD FIPS LAYER DIRECT FROM VAHYDRO
  if(!exists("fips.csv")) {  
    
    fips_path <- paste(localpath,"/hydro-tools/GIS_functions/deprecated_mapgen_scripts/AWRR_geom_data/fips.csv", sep = '')

    #file downloaded into local directory, as long as file exists it will not be re-downloaded
    if (file.exists(fips_path)) {
      print(paste("__fips.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
      fips.csv <- read.csv(fips_path)
    } else {
      print(paste("DOWNLOADING FIPS LAYER DIRECT FROM VAHYDRO...",sep=""))
      fips.csv_item <- paste(site,"/usafips_geom_export",sep="")
      fips.csv_filename <- "fips.csv"
      
      print(paste("__DOWNLOADING fips.csv LAYER", sep = ''))
      destfile <- paste(localpath,fips.csv_filename,sep="\\")
      download.file(fips.csv_item, destfile = destfile, method = "libcurl")
      #read csv from local directory
      print(paste("__LOADING fips.csv LAYER...", sep = ''))
      fips.csv <- read.csv(file=paste(localpath,fips.csv_filename,sep="\\"), header=TRUE, sep=",")
      print(paste("__COMPLETE!", sep = '')) 
    }
    
  }
  
  #DOWNLOAD RESERVOIR LAYER FROM LOCAL REPO
  if(!exists("WBDF")) {
    
    WBDF_path <- paste(localpath,"/hydro-tools/GIS_functions/deprecated_mapgen_scripts/AWRR_geom_data/WBDF.csv", sep = '')
    if (file.exists(WBDF_path)) {
      print(paste("__WBDF.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
      WBDF <- read.csv(WBDF_path)
    } else {
      print(paste("__LOADING RESERVOIR LAYER FROM LOCAL REPO...",sep=""))
      WBDF <- read.table(paste(github_location,"HARPArchive/GIS_layers","WBDF.csv",sep="/"), header=TRUE, sep=",")
    }
    
  }
  
  #DOWNLOAD RESERVOIR LAYER FROM LOCAL REPO (AS .SHP)
  print(paste("__LOADING RESERVOIR LAYER FROM LOCAL REPO (AS .SHP)...",sep=""))
  localpath <- paste(github_location,"HARParchive/GIS_layers",sep="/")
  epsg_code <- 4326 #WGS 84
  shp_path <- "MajorReservoirs" #location of .shp
  shp_layer_name <- "MajorReservoirs" #.shp file name (with ".shp" extension left off)
  shp_layer_load <- suppressWarnings(st_read(paste(localpath,shp_path,sep="/"),shp_layer_name))
  shp_layer <-st_transform(shp_layer_load, crs = st_crs(epsg_code)) 
  shp_layer_wkt <- st_as_text(shp_layer$geometry)
  MajorReservoirs.csv <- data.frame("name"="all","geom"=shp_layer_wkt)
  
  # #DOWNLOAD MAJOR RESERVOIRS LAYER DIRECT FROM GITHUB
  # if(!exists("MajorReservoirs.csv")) {  
  #   print(paste("DOWNLOADING MajorReservoirs LAYER DIRECT FROM GITHUB...",sep=""))
  #   MajorReservoirs.csv_item <- "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/MajorReservoirs.csv"
  #   MajorReservoirs.csv_filename <- "MajorReservoirs.csv"
  #   #file downloaded into local directory, as long as file exists it will not be re-downloaded
  #   if (file.exists(paste(localpath, MajorReservoirs.csv_filename, sep = '/')) == FALSE) {
  #     print(paste("__DOWNLOADING MajorReservoirs.csv LAYER", sep = ''))
  #     destfile <- paste(localpath,MajorReservoirs.csv_filename,sep="\\")
  #     download.file(MajorReservoirs.csv_item, destfile = destfile, method = "libcurl")
  #   } else {
  #     print(paste("__MajorReservoirs.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
  #   }
  #   #read csv from local directory
  #   print(paste("__LOADING MajorReservoirs.csv LAYER...", sep = ''))
  #   MajorReservoirs.csv <- read.csv(file=paste(localpath,MajorReservoirs.csv_filename,sep="\\"), header=TRUE, sep=",")
  #   print(paste("__COMPLETE!", sep = ''))  
  # }
  
  #LOAD ANY ADDITIONL MAPPING FUNCTIONS
  source(paste(vahydro_location,"R/wsp/wsp2020/FoundationDataset/geo_summaries/mb.extent.R",sep = '/'))
  
  layers <- list("STATES" = STATES, 
                 "MinorBasins.csv" = MinorBasins.csv,
                 "RSeg.csv" = RSeg.csv,
                 "MajorRivers.csv" = MajorRivers.csv,
                 "fips.csv" = fips.csv,
                 "WBDF" = WBDF,
                 "MajorReservoirs.csv" = MajorReservoirs.csv
  )
  return(layers)
}
