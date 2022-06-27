library(sqldf)

# require("maptools")
# gpclibPermit()
# if (!require(gpclib)) install.packages("gpclib", type="source")

#PROCESS BASE LAYERS
base.layers <- function(baselayers,extent = data.frame(x = c(-84, -75),y = c(35.25, 40.6))){
  
  # # LOAD MAP LAYERS FROM THE baselayers LIST 
  STATES <- baselayers[[which(names(baselayers) == "STATES")]]
  MinorBasins.csv <- baselayers[[which(names(baselayers) == "MinorBasins.csv")]]
  #RSeg.csv <- baselayers[[which(names(baselayers) == "RSeg.csv")]]
  MajorRivers.csv <- baselayers[[which(names(baselayers) == "MajorRivers.csv")]]
  fips.csv <- baselayers[[which(names(baselayers) == "fips.csv")]]
  WBDF <- baselayers[[which(names(baselayers) == "WBDF")]]
  
  
  print(extent)
  ######################################################################################################
  
  # BOUNDING BOX
  bb=readWKT(paste0("POLYGON((",extent$x[1]," ",extent$y[1],",",extent$x[2]," ",extent$y[1],",",extent$x[2]," ",extent$y[2],",",extent$x[1]," ",extent$y[2],",",extent$x[1]," ",extent$y[1],"))",sep=""))
  bbProjected <- SpatialPolygonsDataFrame(bb,data.frame("id"), match.ID = FALSE)
  bbProjected@data$id <- rownames(bbProjected@data)
  print("IM HERE")
 

  bbPoints <- fortify(bbProjected, region = "id") #THIS WAS THE ORIG LINE
  
  # Union together all polygons that make up a region
  # try_require(c("gpclib", "maptools"))
  # library(maptools)
  # gpclibPermit()
  # bbPoints <- unionSpatialPolygons(bbProjected, IDs = "id")
  
  print("IM HERE 2")
  # bbDF <- merge(bbPoints, bbProjected@data, by = "id")
  bbDF <- merge(bbProjected, bbProjected@data, by = "id")
  
  print(bbDF)
  
  print("IM HERE 3")
  ######################################################################################################
  ### PROCESS STATES LAYER  ############################################################################
  ######################################################################################################
  
  # NEED TO REMOVE INDIANA DUE TO FAULTY GEOM
  STATES <- sqldf(paste('SELECT * FROM STATES WHERE state != "IN"',sep=""))
  
  STATES$id <- as.numeric(rownames(STATES))
  state.list <- list()
  
  for (i in 1:length(STATES$state)) {
    state_geom <- readWKT(STATES$geom[i])
    state_geom_clip <- gIntersection(bb, state_geom)
    
    if (is.null(state_geom_clip) == TRUE) {
      # print("STATE OUT OF MINOR BASIN EXTENT - SKIPPING") 
      next
    }
    
    stateProjected <- SpatialPolygonsDataFrame(state_geom_clip, data.frame('id'), match.ID = TRUE)
    stateProjected@data$id <- as.character(i)
    state.list[[i]] <- stateProjected
  }
  
  length(state.list)
  #REMOVE THOSE STATES THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
  state.list <- state.list[which(!sapply(state.list, is.null))]
  length(state.list)
  
  state <- do.call('rbind', state.list)
  state@data <- merge(state@data, STATES, by = 'id')
  state@data <- state@data[,-c(2:3)]
  state.df <- fortify(state, region = 'id')
  # state.df <- merge(state.df, state@data, by = 'id') 
  state.df <- merge(state, state@data, by = 'id') 
  ######################################################################################################
  ### PROCESS Minor Basin LAYER  #######################################################################
  ######################################################################################################
  mb_data <- MinorBasins.csv
  
  mb_data$id <- as.numeric(rownames(mb_data))
  mb.list <- list()
  
  for (z in 1:length(mb_data$code)) {
    mb_geom <- readWKT(mb_data$geom[z])
    mb_geom_clip <- gIntersection(bb, mb_geom)
    
    if (is.null(mb_geom_clip) == TRUE) {
      # print("mb OUT OF MINOR BASIN EXTENT - SKIPPING") 
      next
    }
    
    mbProjected <- SpatialPolygonsDataFrame(mb_geom_clip, data.frame('id'), match.ID = TRUE)
    mbProjected@data$id <- as.character(z)
    mb.list[[z]] <- mbProjected
  }
  
  length(mb.list)
  #REMOVE THOSE MINOR BASINS THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
  mb.list <- mb.list[which(!sapply(mb.list, is.null))]
  length(mb.list)
  
  mb <- do.call('rbind', mb.list)
  mb@data <- merge(mb@data, mb_data, by = 'id')
  mb@data <- mb@data[,-c(2:3)]
  # mb.df <- fortify(mb, region = 'id')
  # mb.df <- merge(mb.df, mb@data, by = 'id')
  mb.df <- merge(mb, mb@data, by = 'id')
  
  #MB.df <- mb.df
  
  
  
  
  
  
  
  
  # mb_data <- MinorBasins.csv
  # 
  # #-------------------------------------------------------------------------------------
  # # MB_df_sql <- paste('SELECT *
  # #             FROM mb_data
  # #             WHERE code = "',minorbasin,'"'
  # #                    ,sep="")
  # # # 
  # # if (minorbasin == "ES") {
  # #   print("COMBINING 2 EASTERN SHORE MINOR BASINS")
  # #   MB_df_sql <- paste('SELECT * FROM mb_data WHERE code = "ES" OR code = "EL"' ,sep="")
  # # }
  # # # 
  # # mb_data <- sqldf(MB_df_sql)
  # # 
  # # print(head(mb_data))
  # #-------------------------------------------------------------------------------------
  #  
  # mb_data$id <- as.character(row_number(mb_data$code))
  # MB.list <- list()
  # 
  # 
  # for (z in 1:length(mb_data$code)) {
  #   MB_geom <- readWKT(mb_data$geom[z])
  #   MB_geom_clip <- gIntersection(bb, MB_geom)
  # 
  #   # if (is.null(MB_geom_clip) == TRUE) {
  #   #   # print("MB OUT OF MINOR BASIN EXTENT - SKIPPING") 
  #   #   next
  #   # }
  #   
  #   MBProjected <- SpatialPolygonsDataFrame(MB_geom_clip, data.frame('id'), match.ID = TRUE)
  # 
  #   MBProjected@data$id <- as.character(z)
  #   MB.list[[z]] <- MBProjected
  # }
  # 
  # MB <- do.call('rbind', MB.list)
  # MB@data <- merge(MB@data, mb_data, by = 'id')
  # MB@data <- MB@data[,-c(2:3)]
  # MB.df <- fortify(MB, region = 'id')
  # MB.df <- merge(MB.df, MB@data, by = 'id')
  # #print("I made it here")
  ######################################################################################################
  ### PROCESS FIPS LAYER  #############################################################################
  ######################################################################################################
  # 
  # # #PADDING TO ENSURE FIPS NAMES DONT GO BEYOND PLOT WINDOW
  # # fips_extent <- data.frame(x = c(extent$x[1]+0.25, extent$x[2]-0.25),
  # #                           y = c(extent$y[1]+0.25, extent$y[2]-0.25))
  # # fips_bb=readWKT(paste0("POLYGON((",fips_extent$x[1]," ",fips_extent$y[1],",",fips_extent$x[2]," ",fips_extent$y[1],",",fips_extent$x[2]," ",fips_extent$y[2],",",fips_extent$x[1]," ",fips_extent$y[2],",",fips_extent$x[1]," ",fips_extent$y[1],"))",sep=""))
  # 
  # fips_layer <- fips.csv
  # fips_layer$id <- fips_layer$fips_hydroid
  # fips.list <- list()
  # 
  # for (f in 1:length(fips_layer$fips_hydroid)) {
  #   fips_geom <- readWKT(fips_layer$fips_centroid[f])
  #   fips_geom_clip <- gIntersection(MB_geom, fips_geom) #SHOW ONLY FIPS NAMES WITHIN MINOR BASIN
  # 
  #   if (is.null(fips_geom_clip) == TRUE) {
  #     # print("FIPS OUT OF MINOR BASIN EXTENT - SKIPPING")
  #     next
  #   }
  # 
  #   fipsProjected <- SpatialPointsDataFrame(fips_geom_clip, data.frame('id'), match.ID = TRUE)
  #   fipsProjected@data$id <- as.character(fips_layer[f,]$id)
  #   fips.list[[f]] <- fipsProjected
  # }
  # 
  # length(fips.list)
  # #REMOVE THOSE FIPS THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
  # fips.list <- fips.list[which(!sapply(fips.list, is.null))]
  # length(fips.list)
  # 
  # if (length(fips.list) != 0) {
  #   #  print("NO FIPS GEOMS WITHIN MINOR BASIN EXTENT - SKIPPING")
  #   fips <- do.call('rbind', fips.list)
  #   fips@data <- merge(fips@data, fips_layer, by = 'id')
  #   fips@data <- fips@data[,-c(2:3)]
  #   fips.df <- data.frame(fips)
  # } else {
  #   print("NO FIPS GEOMS WITHIN MINOR BASIN EXTENT")
  # 
  #   fips.df <- data.frame(id=c(1,2),
  #                         fips_latitude =c(1,2),
  #                         fips_longitude =c(1,2),
  #                         fips_name = c(1,2),
  #                         stringsAsFactors=FALSE)
  # 
  # }

  
  
  ###########
  #colnames(fips.csv)
  fips_data <- fips.csv
  #colnames(fips_data)
  length(fips_data[,1])
  fips_data$fips_name
  
  #EXCLUDE NC LOCALITIES
  fips_data_sql <- paste('SELECT *
              FROM fips_data
              WHERE fips_code NOT LIKE "3%"',sep="")
  fips_data <- sqldf(fips_data_sql)
  
  fips_data$id <- as.numeric(rownames(fips_data))
  fips.list <- list()
  
  #f<-1
  for (f in 1:length(fips_data$fips_hydroid)) {
    fips_geom <- readWKT(fips_data$fips_geom[f])
    fips_geom_clip <- gIntersection(bb, fips_geom)
    
    if (is.null(fips_geom_clip) == TRUE) {
      # print("fips OUT OF MINOR BASIN EXTENT - SKIPPING") 
      next
    }
    
    fipsProjected <- SpatialPolygonsDataFrame(fips_geom_clip, data.frame('id'), match.ID = TRUE)
    fipsProjected@data$id <- as.character(f)
    fips.list[[f]] <- fipsProjected
  }
  
  length(fips.list)
  #REMOVE THOSE MINOR BASINS THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
  fips.list <- fips.list[which(!sapply(fips.list, is.null))]
  length(fips.list)
  
  fips <- do.call('rbind', fips.list)
  fips@data <- merge(fips@data, fips_data, by = 'id')
  fips@data <- fips@data[,-c(2:3)]
  # fips.df <- fortify(fips, region = 'id')
  # fips.df <- merge(fips.df, fips@data, by = 'id') 
  fips.df <- merge(data, fips@data, by = 'id') 
  
  
  ######################################################################################################
  ### PROCESS MajorRivers.csv LAYER  ###################################################################
  ######################################################################################################
  rivs_layer <- MajorRivers.csv
  
  # rivs_layer_sql <- paste('SELECT *
  #             FROM rivs_layer
  #             WHERE GNIS_NAME = "New River" OR GNIS_NAME = "South Fork New River"'
  #                         ,sep="")
  # rivs_layer <- sqldf(rivs_layer_sql)
  
  #------------------------------------------------------------
  riv.centroid.df <-  data.frame(feature=rivs_layer$feature,
                                 GNIS_NAME=rivs_layer$GNIS_NAME,
                                 centroid_longitude="",
                                 centroid_latitude="",
                                 stringsAsFactors=FALSE) 
  
  
  rivs_layer$id <- rivs_layer$feature
  rivs.list <- list()
  
  #r <- 2
  for (r in 1:length(rivs_layer$feature)) {
    riv_geom <- readWKT(rivs_layer$geom[r])
    
    
    riv_geom_centroid <- gCentroid(riv_geom,byid=TRUE)
    riv.centroid.df$centroid_longitude[r] <- riv_geom_centroid$x
    riv.centroid.df$centroid_latitude[r] <- riv_geom_centroid$y  
    
    
    # riv_geom_clip <- gIntersection(MB_geom, riv_geom)
    riv_geom_clip <- riv_geom
    
    if (is.null(riv_geom_clip) == TRUE) {
      # print("OUT OF MINOR BASIN EXTENT - SKIPPING") 
      next
    }
    
    rivProjected <- SpatialLinesDataFrame(riv_geom_clip, data.frame('id'), match.ID = TRUE)
    rivProjected@data$id <-  as.character(rivs_layer[r,]$id)
    rivs.list[[r]] <- rivProjected
  }
  
  length(rivs.list)
  #REMOVE THOSE rivs_layer THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
  rivs.list <- rivs.list[which(!sapply(rivs.list, is.null))]
  length(rivs.list)
  
  rivs <- do.call('rbind', rivs.list)
  rivs@data <- merge(rivs@data, rivs_layer, by = 'id')
  rivs.df <- rivs
  #print(class(rivs.df))
  
  #print(riv.centroid.df)
  
  
  ######################################################################################################
  ### PROCESS IFIM LAYER  ##############################################################################
  ######################################################################################################
  # ifim_layer <- ifim.csv
  # ifim_layer$id <- ifim_layer$hydroid
  # ifim.list <- list()
  # 
  # #w <- 4
  # for (w in 1:length(ifim_layer$hydroid)) {
  #   
  #   ifim_geom <- readWKT(ifim_layer$geom[w])
  #   ifim_geom_clip <- gIntersection(MB_geom, ifim_geom) #SHOW ONLY ifim NAMES WITHIN MINOR BASIN
  #   
  #   if (is.null(ifim_geom_clip) == TRUE) {
  #     # print("ifim OUT OF MINOR BASIN EXTENT - SKIPPING") 
  #     next
  #   }
  #   
  #   ifimProjected <- SpatialPointsDataFrame(ifim_geom_clip, data.frame('id'), match.ID = TRUE)
  #   ifimProjected@data$id <- as.character(ifim_layer[w,]$id)
  #   ifim.list[[w]] <- ifimProjected
  # }
  # 
  # length(ifim.list)
  # #REMOVE THOSE ifim THAT WERE SKIPPED ABOVE (OUT OF MINOR BASIN EXTENT)
  # ifim.list <- ifim.list[which(!sapply(ifim.list, is.null))]
  # 
  # if (length(ifim.list) != 0) {
  #   #  print("NO ifim GEOMS WITHIN MINOR BASIN EXTENT - SKIPPING")
  #   ifim <- do.call('rbind', ifim.list)
  #   ifim@data <- merge(ifim@data, ifim_layer, by = 'id')
  #   ifim@data <- ifim@data[,-c(2:3)]
  #   ifim.df <- data.frame(ifim)
  # } else {
  #   print("NO ifim GEOMS WITHIN MINOR BASIN EXTENT")
  #   
  #   ifim.df <- data.frame(id=c(1,2),
  #                         ifim_latitude =c(1,2), 
  #                         ifim_longitude =c(1,2),
  #                         ifim_name = c(1,2),
  #                         x = c(1,2),
  #                         y = c(1,2),
  #                         stringsAsFactors=FALSE) 
  #   
  # }

 

    
  baselayers.gg <- list("bb.gg" = bbDF, 
                    "states.gg" = state.df,
                    "mb.gg" = mb.df,
                    "fips.gg" = fips.df,
                    "rivs.gg" = rivs.df,
                    "reservoirs.gg" = WBDF
                    #"ifim.gg" = ifim.df
  )
  return(baselayers.gg)
  
}

#LOAD BASE LAYERS
load_MapLayers <- function(site,localpath = tempdir()){
  library(ggplot2)
  library(rgeos)
  library(ggsn)
  library(rgdal) # needed for readOGR()
  library(dplyr) # needed for case_when()
  library(sf) # needed for st_read()
  library(sqldf)
  library(kableExtra)
  library(viridis) #magma
  #library(wicket) #wkt_centroid() (package was removed from CRAN on 1/27/21)
  library(wellknown) #Replaces the wicket package for wkt_centroid()
  library(cowplot) #plot static legend
  library(magick) #plot static legend
  library(ggrepel) #needed for geom_text_repel()
  library(ggmap) #used for get_stamenmap, get_map
  
  #DOWNLOAD STATES AND MINOR BASIN LAYERS DIRECT FROM GITHUB
  print(paste("DOWNLOADING STATES AND MINOR BASIN LAYERS DIRECT FROM GITHUB...",sep=""))  
  
  if(!exists("STATES")) {  
    STATES_item <- "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/STATES.tsv"
    STATES_filename <- "STATES.tsv"
    #file downloaded into local directory, as long as file exists it will not be re-downloaded
    if (file.exists(paste(localpath, STATES_filename, sep = '/')) == FALSE) {
      print(paste("__DOWNLOADING STATES LAYER", sep = ''))
      destfile <- paste(localpath,STATES_filename,sep="\\")
      download.file(STATES_item, destfile = destfile, method = "libcurl")
    } else {
      print(paste("__STATES LAYER PREVIOUSLY DOWNLOADED", sep = ''))
    }
    #read csv from local directory
    print(paste("__LOADING STATES LAYER...", sep = ''))
    STATES <- read.csv(file=paste(localpath,STATES_filename,sep="\\"), header=TRUE, sep="\t")
    print(paste("__COMPLETE!", sep = ''))
  }  
  
  if(!exists("MinorBasins.csv")) {  
    MinorBasins.csv_item <- "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/MinorBasins.csv"
    MinorBasins.csv_filename <- "MinorBasins.csv.tsv"
    #file downloaded into local directory, as long as file exists it will not be re-downloaded
    if (file.exists(paste(localpath, MinorBasins.csv_filename, sep = '/')) == FALSE) {
      print(paste("__DOWNLOADING MinorBasins.csv LAYER", sep = ''))
      destfile <- paste(localpath,MinorBasins.csv_filename,sep="\\")
      download.file(MinorBasins.csv_item, destfile = destfile, method = "libcurl")
    } else {
      print(paste("__MinorBasins.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
    }
    #read csv from local directory
    print(paste("__LOADING MinorBasins.csv LAYER...", sep = ''))
    MinorBasins.csv <- read.csv(file=paste(localpath,MinorBasins.csv_filename,sep="\\"), header=TRUE, sep=",")
    print(paste("__COMPLETE!", sep = ''))  
  }
  
  #DOWNLOAD RSEG LAYER DIRECT FROM VAHYDRO
  if(!exists("RSeg.csv")) {  
    print(paste("DOWNLOADING RSEG LAYER DIRECT FROM VAHYDRO...",sep=""))
    RSeg.csv_item <- paste(site,"vahydro_riversegs_export",sep="")
    RSeg.csv_filename <- "RSeg.csv"
    #file downloaded into local directory, as long as file exists it will not be re-downloaded
    if (file.exists(paste(localpath, RSeg.csv_filename, sep = '/')) == FALSE) {
      print(paste("__DOWNLOADING RSeg.csv LAYER", sep = ''))
      destfile <- paste(localpath,RSeg.csv_filename,sep="\\")
      download.file(RSeg.csv_item, destfile = destfile, method = "libcurl")
    } else {
      print(paste("__RSeg.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
    }
    #read csv from local directory
    print(paste("__LOADING RSeg.csv LAYER...", sep = ''))
    RSeg.csv <- read.csv(file=paste(localpath,RSeg.csv_filename,sep="\\"), header=TRUE, sep=",")
    print(paste("__COMPLETE!", sep = ''))  
  }
  
  #DOWNLOAD MAJORRIVERS LAYER DIRECT FROM GITHUB
  if(!exists("MajorRivers.csv")) {  
    print(paste("DOWNLOADING MAJORRIVERS LAYER DIRECT FROM GITHUB...",sep=""))
    MajorRivers.csv_item <- "https://raw.githubusercontent.com/HARPgroup/HARParchive/master/GIS_layers/MajorRivers.csv"
    MajorRivers.csv_filename <- "MajorRivers.csv"
    #file downloaded into local directory, as long as file exists it will not be re-downloaded
    if (file.exists(paste(localpath, MajorRivers.csv_filename, sep = '/')) == FALSE) {
      print(paste("__DOWNLOADING MajorRivers.csv LAYER", sep = ''))
      destfile <- paste(localpath,MajorRivers.csv_filename,sep="\\")
      download.file(MajorRivers.csv_item, destfile = destfile, method = "libcurl")
    } else {
      print(paste("__MajorRivers.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
    }
    #read csv from local directory
    print(paste("__LOADING MajorRivers.csv LAYER...", sep = ''))
    MajorRivers.csv <- read.csv(file=paste(localpath,MajorRivers.csv_filename,sep="\\"), header=TRUE, sep=",")
    print(paste("__COMPLETE!", sep = ''))  
  }
  
  #DOWNLOAD FIPS LAYER DIRECT FROM VAHYDRO
  if(!exists("fips.csv")) {  
    print(paste("DOWNLOADING FIPS LAYER DIRECT FROM VAHYDRO...",sep=""))
    fips.csv_item <- paste(site,"/usafips_geom_export",sep="")
    fips.csv_filename <- "fips.csv"
    #file downloaded into local directory, as long as file exists it will not be re-downloaded
    if (file.exists(paste(localpath, fips.csv_filename, sep = '/')) == FALSE) {
      print(paste("__DOWNLOADING fips.csv LAYER", sep = ''))
      destfile <- paste(localpath,fips.csv_filename,sep="\\")
      download.file(fips.csv_item, destfile = destfile, method = "libcurl")
    } else {
      print(paste("__fips.csv LAYER PREVIOUSLY DOWNLOADED", sep = ''))
    }
    #read csv from local directory
    print(paste("__LOADING fips.csv LAYER...", sep = ''))
    fips.csv <- read.csv(file=paste(localpath,fips.csv_filename,sep="\\"), header=TRUE, sep=",")
    print(paste("__COMPLETE!", sep = '')) 
  }
  
  #DOWNLOAD RESERVOIR LAYER FROM LOCAL REPO
  if(!exists("WBDF")) {
    print(paste("__LOADING RESERVOIR LAYER FROM LOCAL REPO...",sep=""))
    WBDF <- read.table(file=paste(github_location,"HARPArchive/GIS_layers","WBDF.csv",sep="/"), header=TRUE, sep=",")
  }
  
  #DOWNLOAD RESERVOIR LAYER FROM LOCAL REPO (AS .SHP)
  print(paste("__LOADING RESERVOIR LAYER FROM LOCAL REPO (AS .SHP)...",sep=""))
    localpath <- paste(github_location,"HARParchive/GIS_layers",sep="/")
    epsg_code <- "4326" #WGS 84
    shp_path <- "MajorReservoirs" #location of .shp
    shp_layer_name <- "MajorReservoirs" #.shp file name (with ".shp" extension left off)
    shp_layer_load <- suppressWarnings(readOGR(paste(localpath,shp_path,sep="/"),shp_layer_name))
    shp_layer <-spTransform(shp_layer_load, CRS(paste("+init=epsg:",epsg_code,sep=""))) 
    shp_layer_wkt <- writeWKT(shp_layer)
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
