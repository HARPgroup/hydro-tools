
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
#' @param fully_populate Should all fields that can be populated in for CEDSPermit be ran.
#' This means it will pull the contacts, MPs, and withdrawals, and load them ahead of time
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
    #' @field permit_number The permit number of this permit
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
    #' @param permit_type A character string specifying what the permit of interest is. Could
    #' be one of "WWR","VWP","GWP", or "VPDES". This field is required
    #' @param permit_number The permit number to search for. The pkid is prioritized over this
    #' @param config A list of column name value pairs to search for the permit by. These must be
    #' column names from the permit table, else it will return an error
    #' @param fully_populate Should all fields that can be populated in for CEDSPermit be ran.
    #' This means it will pull the contacts, MPs, and withdrawals, and load them ahead of time
    #' @return Instance of CEDSPermit with populated permit information
    initialize = function(datasource, permit_type = c("WWR","VWP","GWP","VPDES"),
                          pkid = NA, permit_number = NA, config = list(), fully_populate=FALSE) {
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
      
      ## If it is to be populated ahead of time
      if (fully_populate) {
        self$get_mps()
        self$get_withdrawals()
        self$get_contacts()
      }
      
    },
    
    #' @description
    #' Pulls the measuring points associated with the permit (that have measuring
    #' points of the type corresponding to .source). Returns a data.frame of all 
    #' MPs found. No arguments are needed, as it searches based on the permit ID of this object
    #' @param .source What type of measuring points to search for. Options are \code{"SW/GW",
    #' "GW","SW","All"}. "All" is the only option that will include transfers
    #' @param return_df Should a data.frame be returned. Defaults to TRUE
    #' @return A dataframe of the measuring points found. This data.frame is a snip form the
    #' measuring points view, and contains no withdrawal information. Also sets the mps field
    #' of the \code{CEDSPermit} object. If no MPs are found, it returns a message.
    ## Fills in mps field
    get_mps = function(.source = "SW/GW", return_df = TRUE) {
      
      pmps <- super$get_mps(self$pkid, .source)
      
      ## Setting mps
      self$mps <- pmps
      
      if (return_df) {
        return(pmps)
      }
      
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
        
        return(self$facility$clone())
      }
    }
    
  )
)

