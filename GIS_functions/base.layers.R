library(sqldf)


base.layers <- function(map_layers,extent = data.frame(x = c(-84, -75),y = c(35.25, 40.6))){
  
  # # LOAD MAP LAYERS FROM THE map_layers LIST 
  STATES <- map_layers[[which(names(map_layers) == "STATES")]]
  MinorBasins.csv <- map_layers[[which(names(map_layers) == "MinorBasins.csv")]]
  #RSeg.csv <- map_layers[[which(names(map_layers) == "RSeg.csv")]]
  MajorRivers.csv <- map_layers[[which(names(map_layers) == "MajorRivers.csv")]]
  # fips.csv <- map_layers[[which(names(map_layers) == "fips.csv")]]
  WBDF <- map_layers[[which(names(map_layers) == "WBDF")]]
  
  
  print(extent)
  ######################################################################################################
  
  # BOUNDING BOX
  bb=readWKT(paste0("POLYGON((",extent$x[1]," ",extent$y[1],",",extent$x[2]," ",extent$y[1],",",extent$x[2]," ",extent$y[2],",",extent$x[1]," ",extent$y[2],",",extent$x[1]," ",extent$y[1],"))",sep=""))
  bbProjected <- SpatialPolygonsDataFrame(bb,data.frame("id"), match.ID = FALSE)
  bbProjected@data$id <- rownames(bbProjected@data)
  bbPoints <- fortify(bbProjected, region = "id")
  bbDF <- merge(bbPoints, bbProjected@data, by = "id")
  
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
  state.df <- merge(state.df, state@data, by = 'id') 
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
  mb.df <- fortify(mb, region = 'id')
  mb.df <- merge(mb.df, mb@data, by = 'id') 
  
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
  # 
  # #print(fips.df)
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
                    #"fips.gg" = fips.df,
                    "rivs.gg" = rivs.df,
                    "reservoirs.gg" = WBDF
                    #"ifim.gg" = ifim.df
  )
  return(baselayers.gg)
  
}
