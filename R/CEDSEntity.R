# CEDSEntity ####
#' Generic Entity from CEDS
#' @description CEDSEntity is the base entity R6 that others like CEDSFacility inherit
#' @details Contains generic standard methods for searching CEDS and generic fields 
#' for storing entity information. These are inherited by other objects like \code{CEDSFacility}
#' and provide a shared basis for querying methods, like get_mps, so they can work for
#' both \code{CEDSFacility} and \code{CEDSPermit}. This will rarely be called by itself,
#' and is only called by the children functions.
#' @importFrom R6 R6Class  
#' @param datasource \code{CEDSDataSource} object, which serves as the connection for this object
#' to ODS
#' @param entity_type A character string specifying what the feature of interest is. Could
#' be "facility" or the type of permit ("WWR","VWP","GWP","VPDES")
#' @param pkid The CEDS ID of the entity of interest. Must be the ID of the entity itself,
#' so the facility ID for a facility the permit ID of the permit.  This or config are required to
#' find the entity If pkid is provided, config is ignored
#' @param config A list of column name value pairs to search for the entity by. These must be
#' column names from the entity table, else it will return an error
#' @return Instance of CEDSFacility with populated facility information
#' @examples \dontrun{
#'#Create a CEDSFacility using pkid
#'fac <- CEDSFacility$new(dsCEDS, pkid = 200000203181)
#'# Using the config to specify other names
#'fac2 <- CEDSFacility$new(dsCEDS, config = list("Facility Name" = 'Schaefer Landis Farm LLC'))
#'}
#' @export CEDSEntity

CEDSEntity <- R6Class(
  "CEDS_Facility",
  
  public = list(
    
    #' @field pkid Primary ID value of the entity
    pkid = NULL,
    
    #' @field ds Datasource for this entity, created by \code {CEDSDataSource}
    ds = NULL,
    
    #' @field entity_type What type of record is this object representing
    entity_type = NULL,
    
    #' @field tbl_info Result of fn_get_table_names to identify key columns for the entity_type
    tbl_info = NULL,
    
    #' @field tbl_df Row of the entity's respective view for this record
    tbl_df = NULL,
    
    #' @field permits List/df of all permits created by \code{CEDSEntity$get_permits()}
    permits = NULL,
    
    #' @field sf Coordinates of the entity in an sf data.frame
    sf = NULL,
    
    #' @field contacts Contacts associated with the entity, created by \code{CEDSEntity$get_contacts()}
    contacts = NULL,
    
    #' @field mps list/df of measuring points on Withdrawal permits/registraitons, created by \code{CEDSEntity$get_mps()}
    mps = NULL,
    
    #' @field locality Locality that the entity is in
    locality = NULL,
    
    #' @field five_yr_avg The five year average withdrawal of the entity (sum of all MPs)
    five_yr_avg = NULL,
    
    #' @field withdrawals Data.frame of withdrawals of this entity's mps
    withdrawals = NULL,
    
    ## Methods
    #' @description
    #' Initializes the instance of CEDSEntity This can then be fed into other CEDS r6 objects. 
    #' This method is called by the \code{initialize} of other CEDS objects, like \code{CEDSFacility}
    #' @param datasource \code{CEDSDataSource} object, which serves as the connection for this object
    #' to ODS
    #' @param entity_type A character string specifying what the feature of interest is. Could
    #' be "facility" or the type of permit ("WWR","VWP","GWP","VPDES")
    #' @param pkid The CEDS ID of the entity of interest. Must be the ID of the entity itself,
    #' so the facility ID for a facility the permit ID of the permit.  This or config are required to
    #' find the entity If pkid is provided, config is ignored
    #' @param config A list of column name value pairs to search for the entity by. These must be
    #' column names from the entity table, else it will return an error
    #' @return object instance of CEDSEntity 
    initialize = function(datasource, entity_type, pkid = NA, config = list()) {
      if ("CEDSDataSource" %in% class(datasource)) {
        ## Setting self fieds
        self$ds <- datasource
        
        self$entity_type <- entity_type
        
        ## Finding column names
        self$tbl_info <- fn_get_table_names(entity_type)
        
        ## Getting row fromrespective table
        self$tbl_df <- self$ds$get(entity_type, pkid, config, self$tbl_info)
        
        ## Getting pkid (if pid is not defined, still pulls from table)
        self$pkid <- self$tbl_df[[self$tbl_info$pk_colname]]
        
      } else {
        stop("Need to supply a valid CEDSDataSource using CEDSDataSource$new('PROD')")
      }
    },
    
    #' @description
    #' Pulls the measuring points of a withdrawal permit. This method takes apermit CEDS ID and
    #' returns the row of the measuring point view of all measuring points of the desired source type 
    #' (specified). This method only returns MP information, notwithdrawals.
    #' @param permit_id The permit CEDS ID. This can be either a VWP, GWP, or WWR. But it does not
    #' matter which type, since the measuring point table has a permit ID column agnostic of type
    #' @param entity_type A character string specifying what the feature of interest is. Could
    #' be "facility" or the type of permit ("WWR","VWP","GWP","VPDES")
    #' @param source The desired measuring points to pull. By default it is SW/GW (surface water +
    #' groundwater). This can be limited to one or the other, or "All", which includes transfers
    #' @return A data.frame of the rows from the measuring point view for MPs of the selected
    #' source type for that permit. 
    ## Pulls MPs from a withdrawal permit
    get_mps = function(permit_id, source = c("SW/GW","SW","GW","All")) {
      ## Setting WHERE if the desire is to limit
      sql_where <- ""
      ## Chooses the first one, so it defaults to "SW/GW"
      tryCatch(
        {source <- match.arg(source)},
        error = function(e) {
          message(e)
          stop(paste0('Source type must be one of "SW/GW","SW","GW","All" or empty (defaults to SW/GW)'))
        }
      )
      if (source == "SW/GW") {
        sql_where <- "AND MPT_DESCRIPTION != 'Transfer'"
      } else if (source == "SW") {
        sql_where <- "AND MPT_DESCRIPTION = 'Intake'"
      } else if (source == "GW") {
        sql_where <- "AND MPT_DESCRIPTION = 'Well'"
      }

      ## Pull from MP table
      sql <- paste("SELECT * FROM water.Measuring_Point_Vw WHERE Permit_ID = ",permit_id, sql_where)
      
      mps <- dbGetQuery(conn = self$ds$connection, sql)
      
      ## Renaming some of the MP columns since their terrible
      names(mps) <- gsub("GMP_W_|GMP_I_|GMP_","",names(mps))
      
      namekey <- c("ID" = "Measuring_Point_ID", "GMPS_DESCRIPTION" = "MP_Status",
                   "MPT_DESCRIPTION" = "MP_Type","FACILITY_ID" = "CEDS_Facility_ID",
                   "MPWT_DESCRIPTION" = "Well_Subtype","MPTT_DESCRIPTION" = "Transfer_Type")
      
      names(mps)[names(mps) %in% names(namekey)] <- namekey[names(mps)[names(mps) %in% names(namekey)] ]
      
      return(mps)
    },
    
    #' @description
    #' Pulls contacts associated with the entity. For everything but WWR, this is the contacts associated
    #' directly with that entity. For WWR, it is the contacts of the associated facility (since they cannot
    #' be directly attached to a WWR). Each entity has its own contact table, so the names are standardized
    #' in this method.
    #' @param pkid The CEDS ID of the entity of interest. Must be the ID of the entity itself,
    #' so the facility ID for a facility the permit ID of the permit.  
    #' @param entity_type A character string specifying what the feature of interest is. Could
    #' be "facility" or the type of permit ("WWR","VWP","GWP","VPDES")
    #' @param limit Should the result be limited to "Water WIthdrawal Contacts"? This only applies to
    #' facilities and WWRs, since permit contacts often have other 'Contact Purposes'. Can be set to
    #' FALSE to include all facility contacts
    #' @return_df Should a data.frame be returned. Defaults to TRUE
    #' @return Data.frame of contacts for the entity and their contact information
    get_contacts = function(limit = TRUE, return_df =TRUE) {
      ## Not pulled from self since anything could be passed in here
      # tbl_info <- fn_get_table_names(entity_type)

      entity_type <- self$entity_type
      
      ## For WWRs, use the facility field for pkid
      if (entity_type == 'WWR') {
        pkid <- self$tbl_df[[self$tbl_info$facility_field]]
      } else {
        pkid <- self$pkid
      }
      
      ## Pull all contacts for entity  
      sql_contacts <- paste("SELECT * 
                           FROM ", self$tbl_info$contact_tbl,
                            "WHERE ", self$tbl_info$contact_pkcol, "=",pkid)
      
      contacts <- dbGetQuery(self$ds$connection, sql_contacts)
      
      ## Removing spaces from table names (present in facility and water contacts)
      ## Goal is to make relevant names consistent accross tables
      names(contacts) <- tolower(gsub(" ","_", names(contacts)))
      
      ## Facility contact table just appends 'Contact' to all their names for some reason
      ## So remove contact from Everything except Id and Purposes (which should have contact)
      names(contacts) <- gsub("contact_([^i].[^r])","\\1",names(contacts))
      
      contacts_out <- contacts[, c("full_name","email","primary_phone",
                                   "contact_id", "contact_purposes")]
      
      ## For WWR and facility, if otherwise unspecified select only Withdrawal Reporting Contact
      if (entity_type %in% c("WWR", "facility") & (limit)) {
        
        contacts_out <- contacts_out[grepl("Withdrawal Reporting Contact", contacts_out$contact_purpose),]
        
      }
      
      self$contacts <- contacts_out
      
      if (return_df){
        return(self$contacts)
      }
      
    },
    
    #' @description
    #' Pulls the withdrawals from the water.Water_Use_By_Month_Vw for the measuring points on
    #' this entity (as pulled by \code{get_mps}). These withdrawals show either monthly or yearly
    #' use of the all mps in the facility/permit in millions of gallons. This is pulled by the
    #' same method the foundation dataset is pulled, just paired down to only MP/withdrawal fields.
    #' Since this is pulled via a live connection to ODS (still 24 hours behind CEDS), it may be
    #' different from the foundation datset. All arguments are optional and it can run without them,
    #' they just change the format of the returned data.
    #' @param years Lets you specify years of interest. By default set as FALSE, which will show the
    #' entire period of record. Specifying years (as a numeric vector, i.e. 2015:2025) will only pull
    #' data for those specified years. For entities with many MPs, this can speed up performance.
    #' @param period This will control how the data is aggregated. The data is reported monthly, so by
    #' default there will be no aggregation applied. However, if \code{period} is set to "yearly", then
    #' it will be aggregated by year, showing the total withdrawal of that MP for the given year
    #' @return Data.frame of the withdrawals (in millions of gallons) for the MPs of the entity
    get_withdrawals = function(years = FALSE, period = "yearly", return_df =TRUE) {
      
      ## If no mps are loaded, throw an error
      if (is.null(self$mps)) {
        stop("No MPs found on object. Must first run get_mps()")
      }
      
      ## Pull withdrawals for those MPs 
      mps <- self$mps
      
      ## Pull the withdrawals for all MPs. Filter by years if specified
      withdr_sql <- paste0("SELECT Measuring_Point_Id, Withdrawal_Date, Withdraw_Volume 
                            FROM water.Water_Use_By_Month_Vw
                            WHERE Measuring_Point_Id IN (",
                            paste0(as.character(mps$Measuring_Point_ID), collapse = ","),")")
      
      monthly_withdrw <- dbGetQuery(self$ds$connection, withdr_sql)
      
      ## Adding a year column
      monthly_withdrw$year <- substr(monthly_withdrw$Withdrawal_Date,1,4)
      
      ## If years is set, specify those years as part of the WHERE
      if (!is.logical(years)) {
        ## Years should be a vector of numeric years. Stich them together in a WHERE
        where_clause <- paste0("AND years(Withdrawal_Date) IN (", paste(years, collapse = ","),")")
      } else {
        where_clause <- ""
      }
      
      if (period == "yearly") {
        ## Selecting year and aggregating withdrawal volumne
        aggregate_caluse <- "w.Year AS Year,
           ROUND(SUM(w.Withdraw_Volume) / 1000000, 3) AS Water_Use_MGY"
        
        ## Grouping by MP and year
        group_clause <- "GROUP BY mp_permits.Measuring_Point_Id, Source_Type,
          mp_permits.Name, mp_permits.CEDS_Facility_Id,
          mp_permits.Facility_Name, mp_permits.Permit_No, w.Year"
      } else {
        ## Select withdrawal date instead, do not aggregate withdrawal
        aggregate_caluse <- "w.Withdrawal_Date AS Date,
           ROUND(w.Withdraw_Volume / 1000000, 3) AS Water_Use_MGM"
        
        ## For monthly, no need to group
        group_clause <- ""
      }
      
      ## Format the result (group by year if specified)
      withdrawals <-sqldf(paste(
        "SELECT 
        mp_permits.Measuring_Point_Id AS MP_CEDSid,
        CASE 
          WHEN mp_permits.MP_Type = 'Well'   THEN 'Groundwater'
          WHEN mp_permits.MP_Type = 'Intake' THEN 'Surface Water'
        END AS Source_Type,
        mp_permits.Name AS MP_Name,
        mp_permits.CEDS_Facility_Id AS CEDS_Facility_Id,
        mp_permits.Facility_Name AS Facility,
        mp_permits.Permit_No AS Permit_Number,
        CASE 
          WHEN substr(mp_permits.Permit_No,1,3) = 'WWR' THEN NULL
          ELSE mp_permits.PERMIT_CLASSIFICATION_STRING
        END AS Permit_Classification,",
        aggregate_caluse,
        
        "FROM mps mp_permits
        LEFT JOIN monthly_withdrw w
          ON  mp_permits.Measuring_Point_Id = w.Measuring_Point_Id
          
        ",where_clause,  group_clause
      ))
      
      if ("Date" %in% names(withdrawals)) {
        withdrawals$Date <- as.Date(as.POSIXct(withdrawals$Date))
      }
      
      self$withdrawals <- withdrawals
      
      if (return_df) {
        return(withdrawals)
      } else {
        return(NULL)
      }
    }
  )
  
  
)


