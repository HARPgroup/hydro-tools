
# CEDSFacility ####
#' Facility object from CEDS
#' @description CEDSFacility is an object that contains  CEDS information on a facility from CEDS.
#' @details Contains standard methods to extract information from facilities, such as GIS data,
#' permits, measuring points, and contacts. \code{CEDSFacility} is meant to provide an easier
#' method to retrieve data from CEDS than writing queries to ODS, as those queries are contained
#' in the methods. This is a child of \code{CEDSEntity}, which contains more generic search 
#' methods and fields that this R6 will call for facility specific information
#' See fields/methods for available data
#' @importFrom R6 R6Class  
#' @param datasource \code{CEDSDataSource} object, which serves as the connection for this object
#' to ODS
#' @param pkid The CEDS ID of the facility of interest. This or config are required to
#' find this facility. If pkid is provided, config is ignored
#' @param config A list of column name value pairs to search for the facility by. These must be
#' column names from the facility table, else it will return an error
#' @param fully_populate Should all fields that can be populated in for CEDSFacility be ran.
#' This means it will pull the permits, MPs, and withdrawals, and load them ahead of time
#' @return Instance of CEDSFacility with populated facility information
#' @examples \dontrun{
#'#Create a CEDSFacility using pkid
#'fac <- CEDSFacility$new(dsCEDS, pkid = 200000203181)
#'# Using the config to specify other names
#'fac2 <- CEDSFacility$new(dsCEDS, config = list("Facility Name" = 'Schaefer Landis Farm LLC'))
#'}
#' @export CEDSFacility

CEDSFacility <- R6Class(
  "CEDSFacility",
  inherit = CEDSEntity,
  
  public = list(
    
    ## Fields of the facility
    #' @field facility_hydroid The hydroid of the facility in the hydro database. For facilities
    #' created after 2024, this information will not be in CEDS
    facility_hydroid = NULL,
    #' @field name THe name of the facility
    name = NULL,
    feat_sf = NULL,
    geom_CRS=4326,
    #' @description
    #' Initialize the CEDSFacility object. Requires either a pkid or a list of other column names.
    #' When supplying a config list, it will only select the first facility, even if multiple exist.
    #' This function works primarily by calling \code{CEDSEntity$initialize}, then setting local fields
    #' @param datasource \code{CEDSDataSource} object, which serves as the connection for this object
    #' to ODS
    #' @param pkid The CEDS ID of the facility of interest. This or config are required to
    #' find this facility. If pkid is provided, config is ignored
    #' @param config A list of column name value pairs to search for the facility by. These must be
    #' column names from the facility table, else it will return an error
    #' @param fully_populate Should all fields that can be populated in for CEDSFacility be ran.
    #' This means it will pull the permits, MPs, and withdrawals, and load them ahead of time
    #' @return Instance of CEDSFacility with populated facility information
    initialize = function(datasource,  pkid = NA, config = list(), fully_populate = FALSE) {

      ## Get the data table from its respective Table/View in CEDS
      super$initialize(datasource, entity_type = "facility", pkid, config)
      
      ## Get coordinate/locality from respective GIS table
      gisdata <- getGIS(self$pkid, self$tbl_info, self$ds)
      
      self$locality <- gisdata$County
      
      sfobj <- st_as_sf(
        data.frame(ID = self$pkid, entity_type = self$entity_type,
                   Longitude = gisdata$Longitude, Latitude = gisdata$Latitude),
        coords = c("Longitude","Latitude"), crs = 4326)
      self$feat_sf <- sfobj
      
      ## Setting other self fields
      self$name <- self$tbl_df$`Facility Name`
      
      ## If it is to be populated ahead of time
      if (fully_populate) {
        self$get_permits(return_df = FALSE)
        self$get_mps(return_df = FALSE)
        self$get_withdrawals(return_df = FALSE)
        self$get_contacts(return_df = FALSE)
      }
      
    },
    #' @description
    #' Pulls all permits for a given facility ID. This searches the VWP, WWR, GWP, and VPDES tables.
    #' These tables all have different structures and column names, so this pulls select columns and
    #' standardizes the names.
    #' @param facility_id The CEDS Facility ID to check for associated permits
    #' @return Data.frame of permits assoicated with the facility. Contains only select identifying 
    #' fields. Permit limits are withdrawals in gallons per month/year. These are not applicable to
    #' WWRs or VPDES (VPDES limits not included)
    get_permits = function(return_df = TRUE) {
      facility_id <- self$pkid
      
      ## Look through VWP, GWP and VPDES 
      gwp_sql <- paste("SELECT 'GWP' AS Permit_Type, GWP_Permit_Number AS Permit_Number, Permit_Id, 
                       Permit_Status AS Classification, Use_Type, Annual_Limit, Monthly_Limit
                       FROM water.GWP_Permits_Vw
                       WHERE CEDS_Facility_ID = ", facility_id)
      gwps <- dbGetQuery(self$ds$connection, gwp_sql)
      
      vwp_sql <- paste("SELECT 'VWP' AS Permit_Type, [Permit Number] AS Permit_Number, [Permit ID] AS Permit_Id, 
                      Classification, cond.Water_Use_Type AS Use_Type, cond.Annual_Limit, cond.Monthly_Limit
                      FROM water.[VWP Permits] v
                      LEFT JOIN water.Water_Withdrawal_Permits_Vw cond
                        ON v.[Permit ID] = cond.Permit_Id
                      WHERE v.[CEDS Facility Id] = ", facility_id)
      vwps <- dbGetQuery(self$ds$connection, vwp_sql)
      
      wwr_sql <- paste("SELECT 'WWR' AS Permit_Type,Registration_number AS Permit_Number, Permit_Id,
                   'Facility_Status' AS Classification, Use_Type, NULL AS Annual_Limit, NULL AS Monthly_Limit
                   FROM water.Water_Withdrawal_Reg_Vw
                   WHERE CEDS_Facility_Id = ", facility_id)
      wwrs <- dbGetQuery(self$ds$connection, wwr_sql)
      
      vpdes_sql <- paste("SELECT 'VPDES' AS Permit_Type,[Permit Number] AS Permit_Number, [Permit ID] AS Permit_Id, 
                         Classification, [Permit Type] AS Use_Type, NULL AS Annual_Limit, NULL AS Monthly_Limit
                         FROM water.[Water Permits] 
                         WHERE [CEDS Facility Id] = ", facility_id)
      vpdes <- dbGetQuery(self$ds$connection, vpdes_sql)
      
      permits <- rbind(gwps,vwps,wwrs,vpdes)
      
      self$permits <- permits
      
      if (return_df) {
        return(permits)
      }
    },
    #' @description
    #' Pulls the measuring points associated with all permits of this facility (that have measuring
    #' points of the type corresponding to .source). If the permits object of this entity is empty,
    #' it will first run \code{get_permits}, setting the permit field. Returns a data.frame of all 
    #' MPs found. No arguments are needed, as it searches based on the facility ID of this object
    #' @param .source What type of measuring points to search for. Options are \code{"SW/GW",
    #' "GW","SW","All"}. "All" is the only option that will include transfers
    #' @return_df Should a data.frame be returned. Defaults to TRUE
    #' @return A dataframe of the measuring points found. This data.frame is a snip form the
    #' measuring points view, and contains no withdrawal information. Also sets the mps field
    #' of the \code{CEDSFacility} object. If no MPs are found, it returns a message.
    get_mps = function(.source = "SW/GW", return_df = TRUE) {
      ## First check if there are already permits on the object
      if (is.null(self$permits)) {
        ## If not, find all permits
        self$get_permits()
      }
      ## The above line sets the permits, so proceed
      ## Only run if there are any withdrawal permits
      if (any(self$permits$Permit_Type != 'VPDES') && !is.null(self$permits)) {
        
        ## Pull out VPDES
        withdrawal_permits <- self$permits[self$permits$Permit_Type != 'VPDES',]
        
        ## Initialize blank list
        p_mps <- list()
        
        ## Go through each withdrawal permit and pull out MPs
        for (permit in withdrawal_permits$Permit_Id) {
          
          loop_mps <- super$get_mps(permit, .source)
          
          ## For versioned permits, MPs can be attached to each of them
          ## So remove any repeated MPs
          loop_mps <- loop_mps[!(loop_mps$Measuring_Point_ID %in% do.call(rbind, p_mps)$Measuring_Point_ID),]
          
          
          p_mps[[length(p_mps) + 1]] <- loop_mps
          
        }
        
        ## Bind them all together
        all_mps <- do.call(rbind, p_mps)
        
        self$mps <- all_mps
        
        ## Should the data frame be returned?
        if (return_df) {
          return(all_mps)
        }
        
      } else {
        message("No withdrawal permits/registrations for this facility")
      }
      
    },
    
    #' @description
    #' Finds a permit attached to this facility (or runs \code{get_permits} if there are not currently
    #' any), then finds the permit specified and returns an R6 \code{CEDSPermit} entity for it
    #' @param type This si the type of permit to be searched. Can be "WWR","VWP","GWP", or "VPDES".
    #' It defaults to FALSE, and in which if there is only 1 type of permit for the facility it
    #' assumes that type.
    #' @param status The status of the permit you are looking for. Defaults to "Active". Can be 
    #' "Active","Application", or "History"  
    #' @return Instance of \code{CEDSPermit} for the searched permit. If it cannot narrow the
    #' permits down to 1, it returns a message and no R6 object.
    ## Getting an R6 object for a permit from this facility
    ## Not saved to the object, as a facility can have multiple permits
    pull_permit = function(type = FALSE,status = "Active") {
      out <- NULL
      
      ## Are the permits already loaded on the facility
      if (is.null(self$permits)) {
        ## If not, find all permits
        self$get_permits()
      }
      
      permit_df <- self$permits
      
      ## Ensure there are permits
      if (nrow(permit_df) == 0) {
        message("No permits found on facility")
        return(out)
      } 
      
      ## If no permit type was specified
      if (type == FALSE) {
        ## Check if there are multiple permits types attached to the faciliy
        if (length(unique(permit_df$Permit_Type)) > 1) {
          ## Need to specify the type of permit
          message(paste0("Must specify permit type between ", paste(unique(permit_df$Permit_Type), 
                                                                    collapse = ",")) )
          return(out)
        } 
        
        ## There is only one kind of permit, so set the type equal to that
        type <- unique(permit_df$Permit_Type)
      }
      
      ## If there is only one permit of the selected type, take it regardless of status
      if (sum(permit_df$Permit_Type == type) == 1) {
        permit_out <- permit_df[permit_df$Permit_Type == type,]
      } else{
        permit_out <- permit_df[permit_df$Permit_Type == type & permit_df$Classification == status,]
      }
      
      
      if (nrow(permit_out) > 1) {
        ## Still have multiple permits, select one created most recently
        permit_out <- permit_out[permit_out$Permit_Id == max(permit_out$Permit_Id),]
      } else if (nrow(permit_out) == 0) {
        message(paste("No",status, type, "permit found for this facility"))
        return(out)
      }
      
      permit <- CEDSPermit$new(self$ds, permit_type = type, pkid = permit_out$Permit_Id)
      
      message(paste("Returning",permit_out$Permit_Type, permit_out$Permit_Id))
      
      return(permit)
    },
    
    #' @description
    #' Create a \code{RomFeature} object for the facility, connecting this CEDS feature to Hydro
    #' @param ds A \code{RomDataSource} object. This is different from a \code{CEDSDataSource} 
    #' object, as it is connected to the Hydro server instead of the CEDS server
    #' @return Instance of \code{RomFeature} for the corresponding facility in the Hydro database.
    #' Searches based on the CEDS ID property (varid 1474, CEDS_ID). It is possible for a facility
    #' in CEDS to have multiple corresponding Hydro facilities. In those cases, a list of features
    #' will be returned
    pull_feature = function(ds) {

      id_keys <- dbGetQuery(ds$connection, paste0(
        "SELECT featureid, propcode
        FROM dh_properties
        WHERE varid = 1474
          AND propcode = '", as.character(self$pkid),"'"
      ))
      
      ft <- list()
      
      if (nrow(id_keys) > 1) {
        
        for(i in 1:nrow(id_keys)) {
          
          ftid <- id_keys$featureid[i]
          
          ft[[paste0("feature-",ftid)]] <-
            RomFeature$new(ds, config = list(hydroid = ftid), T)
          
        }
        
      } else if (nrow(id_keys) == 0) {
        message("There is no corresponding facility in Hydro.")
      } else {
        
        ft <- RomFeature$new(ds, config = list(hydroid = id_keys$featureid), T)
        
      }
      
      return(ft)
      
    },
    
    ## Stealing RomFeatures plot method. A little janky
    plot_method = RomFeature$public_methods$plot_feat
  )
)

 fac <- CEDSFacility$new(dsCEDS, pkid = 200000203181, fully_populate = TRUE)
