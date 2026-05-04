
# CEDSPermit ####
#' Facility object from CEDS
#' @description CEDSPermit is an object that contains  CEDS information on a permit from CEDS.
#' @details Contains standard methods to extract information from permits, such as GIS data,
#' measuring points, and contacts. \code{CEDSPermit} is meant to provide an easier
#' method to retrieve data from CEDS than writing queries to ODS, as those queries are contained
#' in the methods. This is a child of \code{CEDSEntity}, which contains more generic search 
#' methods and fields that this R6 will call for permit specific information
#' See fields/methods for available data
#' @importFrom R6 R6Class  
#' @param datasource \code{CEDSDataSource} object, which serves as the connection for this object
#' to ODS
#' @param pkid The CEDS ID of the permit of interest. This or config are required to
#' find this permit If pkid is provided, config is ignored
#' @param config A list of column name value pairs to search for the permit by. These must be
#' column names from the permit table of interest, else it will return an error
#' @return Instance of CEDSPermit with populated permit information
#' @examples \dontrun{
#'#Create a CEDSFacility using pkid
#'p1 <- CEDSPermit$new(dsCEDS, permit_type = "WWR", pkid = 870000002706)
#'# Using the config to specify other names
#'p2 <- CEDSPermit$new(dsCEDS, permit_type = "GWP", permit_number = "GWI000344")
#'}
#' @export CEDSPermit
CEDSPermit <- R6Class(
  "CEDSPermit",
  inherit = CEDSEntity,
  
  public = list(
    #' @field permit_numer The permit number of this permit
    permit_number = NULL,
    
    #' @field facility The facility ID that the permit is connected to. \code{CEDSPermit$pull_facility}
    #' sets this field as the \code{CEDSFacility} object of that facility
    facility = NULL,
    
    #' @field use_type The water use type of the permit (applicable to withdrawal permits)
    use_type = NULL,
    
    #' @description
    #' Initialize the CEDSPermit object. Requires either a pkid or a list of other column names.
    #' When supplying a config list, it will only select the first permit, even if multiple exist.
    #' This function works primarily by calling \code{CEDSEntity$initialize}, then setting local fields
    #' @param datasource \code{CEDSDataSource} object, which serves as the connection for this object
    #' to ODS
    #' @param pkid The CEDS ID of the permit of interest. This or config are required to
    #' find this permit If pkid is provided, config is ignored
    #' @param config A list of column name value pairs to search for the permit by. These must be
    #' column names from the permit table, else it will return an error
    #' @return Instance of CEDSPermit with populated permit information
    initialize = function(datasource, permit_type = c("WWR","VWP","GWP","VPDES"),
                          pkid = NA, permit_number = NA, config = list()) {
      ## If a permit number is supplied, put it into config with the correct column name
      tbl_info <- fn_get_table_names(permit_type)
      
      config[[tbl_info$permit_num_col]] <- permit_number
      
      ## Get the data table from its respective Table/View in CEDS
      super$initialize(datasource, permit_type, pkid, config)
      
      ## Set self fields
      self$permit_number <- self$tbl_df[[tbl_info$permit_num_col]]
      
      self$facility <- self$tbl_df[[tbl_info$facility_field]]
      
      ## Get coordinate/locality from respective GIS table
      gisdata <- getGIS(self$pkid, self$tbl_info, self$ds)
      
      self$locality <- gisdata$County
      
      sfobj <- st_as_sf(
        data.frame(ID = self$pkid, entity_type = self$entity_type, 
                   Longitude = gisdata$Longitude, Latitude = gisdata$Latitude),
        coords = c("Longitude","Latitude"))
      self$sf <- sfobj
      
    },
    
    #' @description
    #' Pulls the measuring points associated with the permit (that have measuring
    #' points of the type corresponding to .source). Returns a data.frame of all 
    #' MPs found. No arguments are needed, as it searches based on the permit ID of this object
    #' @param .source What type of measuring points to search for. Options are \code{"SW/GW",
    #' "GW","SW","All"}. "All" is the only option that will include transfers
    #' @return A dataframe of the measuring points found. This data.frame is a snip form the
    #' measuring points view, and contains no withdrawal information. Also sets the mps field
    #' of the \code{CEDSPermit} object. If no MPs are found, it returns a message.
    ## Fills in mps field
    get_mps = function(.source = "SW/GW") {
      
      pmps <- super$get_mps(self$pkid, .source)
      
      ## Setting mps
      self$mps <- pmps
      
      return(pmps)
      
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
    #' Pulls the facility this permit is associated with and creates a \code{CEDSFacility} object
    #' of it.
    #' @param return_fac A logical where if TRUE this method will return the object, in addition
    #' to setting the facility field
    #' @return Instance of \code{CEDSFacility} for the facility this permit is associated with.
    #' It sets the facility field as this R6 object, and will return nothing unless return_fac is
    #' set to TRUE.
    pull_facility = function(return_fac = F) {
      ## Should this return that object? Or save it as part of itself?
      self$facility <- CEDSFacility$new(self$ds, pkid = self$facility)
      
      if (return_fac) {
        
        return(CEDSFacility$new(self$ds, pkid = self$facility))
      }
    },
    
    #' @description
    #' Find contacts associated with the permit. It will set the \code{contacts} field on 
    #' this object. This mainly calls the #' \code{CEDSEntity$get_contacts} method, feeding it
    #' information for this entity. Pulls the contacts associated directly with the permit, 
    #' except for WWRs, in which it will pull the contacts on the facility. For WWR, by default
    #' it will also limit it down to just the Withdrawal Reporting Contacts
    #' @param .limit A logical for whether to limit to only "Withdrawal Reporting Contacts", or to 
    #' include all. By default, the result is limited. This parameter only affects WWR permits
    #' @return Returns a data.frame of pulled fields for the contacts. Also sets the 
    #' \code{contacts} field on this object with the same result.
    get_contacts = function(.limit = TRUE) {
      
      self$contacts <- super$get_contacts(pkid = self$pkid, entity_type = self$entity_type, .limit)
      
      return(self$contacts)
    }
  )
)

# p1 <- CEDSPermit$new(dsCEDS, permit_type = "WWR", pkid = 870000002706)
# p2 <- CEDSPermit$new(dsCEDS, permit_type = "GWP", permit_number = "GWI000344")
