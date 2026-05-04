
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
        coords = c("Longitude","Latitude"))
      self$sf <- sfobj
      
      ## Setting other self fields
      self$name <- self$tbl_df$`Facility Name`
      
      ## If it is to be populated ahead of time
      if (fully_populate) {
        self$get_permits()
        self$get_mps()
        self$get_withdrawals()
        self$get_contacts()
      }
      
    },
    #' @description
    #' Queries for permits attached to this facility, and sets the permit field of this object.
    #' Will only look for VWP (withdrwal), GWP, WWR, and VPDES permits, searching for them based
    #' on the CEDS facility Id. No arguments are required, as it will search based on the ID
    #' of this object. Most of the code of this method is in the parent \code{CEDSEntity$get_permits} method
    #' @return A paired down data.frame of the permits. Also sets the \code{permits} field with
    #' that same data.frame
    get_permits = function() {
      ## Calls get_permits form CEDSEntity, passing through the facility ID
      super$get_permits(self$pkid)
      
      return(self$permits)
      
    },
    #' @description
    #' Pulls the measuring points associated with all permits of this facility (that have measuring
    #' points of the type corresponding to .source). If the permits object of this entity is empty,
    #' it will first run \code{get_permits}, setting the permit field. Returns a data.frame of all 
    #' MPs found. No arguments are needed, as it searches based on the facility ID of this object
    #' @param .source What type of measuring points to search for. Options are \code{"SW/GW",
    #' "GW","SW","All"}. "All" is the only option that will include transfers
    #' @return A dataframe of the measuring points found. This data.frame is a snip form the
    #' measuring points view, and contains no withdrawal information. Also sets the mps field
    #' of the \code{CEDSFacility} object. If no MPs are found, it returns a message.
    get_mps = function(.source = "SW/GW") {
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
          loop_mps <- loop_mps[!(loop_mps$GMP_ID %in% do.call(rbind, p_mps)$GMP_ID),]
          
          
          p_mps[[length(p_mps) + 1]] <- loop_mps
          
        }
        
        ## Bind them all together
        all_mps <- do.call(rbind, p_mps)
        
        self$mps <- all_mps
        
        return(all_mps)
        
      } else {
        message("No withdrawal permits/registrations for this facility")
      }
      
    },
    
    #' @description
    #' Pulls the withdrawals from the water.Water_Use_By_Month_Vw for the measuring points on
    #' this facility (as pulled by \code{get_mps}). These withdrawals show either monthly or yearly
    #' use of the all mps in the facility in millions of gallons. This is pulled by the
    #' same method the foundation dataset is pulled, just paired down to only MP/withdrawal fields.
    #' Since this is pulled via a live connection to ODS (still 24 hours behind CEDS), it may be
    #' different from the foundation dataset. All arguments are optional and it can run without them,
    #' they just change the format of the returned data.
    #' @param years Lets you specify years of interest. By default set as FALSE, which will show the
    #' entire period of record. Specifying years (as a numeric vector, i.e. 2015:2025) will only pull
    #' data for those specified years. For entities with many MPs, this can speed up performance.
    #' @param period This will control how the data is aggregated. The data is reported monthly, so by
    #' default there will be no aggregation applied. However, if \code{period} is set to "yearly", then
    #' it will be aggregated by year, showing the total withdrawal of that MP for the given year
    #' @return Data.frame of the withdrawals (in millions of gallons) for the MPs of the facility
    get_withdrawals = function(years = FALSE, period = "monthly") {
      
      ## Call the CEDSEntity get_withdrawals
      withdrawals <- super$get_withdrawals(self$mps, years, period)
      
      return(withdrawals)
      
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
      
      permit_out <- permit_df[permit_df$Permit_Type == type & permit_df$Classification == status,]
      
      if (nrow(permit_out) > 1) {
        ## Still have multiple permits, select one created most recently
        permit_out <- permit_out[permit_out$Permit_Id == max(permit_out$Permit_Id),]
      } else if (permit_out == 0) {
        message(paste("No",status, type, "permit found for this facility"))
        return(out)
      }
      
      permit <- CEDSPermit$new(self$ds, permit_type = type, pkid = permit_out$Permit_Id)
      
      message(paste("Returning",permit_out$Permit_Type, permit_out$Permit_Id))
      
      return(permit)
    },
    #' @description
    #' Find contacts associated with the facility. By default it will only pull "Withdrawal Reporting
    #' Contacts". It will set the \code{contacts} field on this object. This mainly calls the 
    #' \code{CEDSEntity$get_contacts} method, feeding it information for this entity
    #' @param .limit A logical for whether to limit to only "Withdrawal Reporting Contacts", or to 
    #' include all. By default, the result is limited.
    #' @return Returns a data.frame of pulled fields for the contacts. Also sets the 
    #' \code{contacts} field on this object with the same result.
    get_contacts = function(.limit = TRUE) {
      
      self$contacts <- super$get_contacts(pkid = self$pkid, entity_type = 'facility', .limit)
      
      return(self$contacts)
    }
    
    
  )
)

# fac <- CEDSFacility$new(dsCEDS, pkid = 200000203181)
