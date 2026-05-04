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
      
      mps <- dbGetQuery(dconn = self$ds$connection, sql)
      
      return(mps)
    },
    #' @description
    #' Pulls all permits for a given facility ID. This searches the VWP, WWR, GWP, and VPDES tables.
    #' These tables all have different structures and column names, so this pulls select columns and
    #' standardizes the names.
    #' @param facility_id The CEDS Facility ID to check for associated permits
    #' @return Data.frame of permits assoicated with the facility. Contains only select identifying 
    #' fields. Permit limits are withdrawals in gallons per month/year. These are not applicable to
    #' WWRs or VPDES (VPDES limits not included)
    get_permits = function(facility_id) {
      
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
    #' @param limit Should the result be limited to "Water WIthdrawl Contacts"? This only applies to
    #' facilities and WWRs, since permit contacts often have other 'Contact Purposes'. Can be set to
    #' FALSE to include all facility contacts
    #' @return Data.frame of contacts for the entity and their contact information
    get_contacts = function(pkid, entity_type, limit = TRUE) {
      ## Not pulled from self since anything could be passed in here
      tbl_info <- fn_get_table_names(entity_type)

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
      
      return(contacts_out)
      
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
    get_withdrawals = function(years = FALSE, period = "monthly") {
      ## Pull withdrawals for those MPs 
      
      ## If years is set, specify those years as part of the WHERE
      if (!is.logical(years)) {
        ## Years should be a vector of numeric years. Stich them together in a WHERE
        yrs_clause <- paste0("AND years(Withdrawal_Date) IN (", paste(years, collapse = ","),")")
      } else {
        yrs_clause <- ""
      }
      
      ## Take the mps already on this object (generated in the child object before sent here)
      mps <- self$mps
      
      ## Add in mpids (a vector of the measuring point ids attached to the feature)
      withdr_sql <- paste0("SELECT * FROM water.Water_Use_By_Month_Vw
                            WHERE Measuring_Point_Id IN (", paste0(as.character(mps$GMP_ID), collapse = ","), 
                           , yrs_clause,")")
      
      monthly_withdrw <- dbGetQuery(conn_DBI, withdr_sql)
      
      
      
      withdrawals <- sqldf(paste0("
        SELECT 
        mp_permits.Measuring_Point_Id AS MP_CEDSid,
        mp_permits.MP_Hydroid AS MP_Hydroid,
        CASE 
          WHEN mp_permits.MP_Type = 'Well'   THEN 'Groundwater'
          WHEN mp_permits.MP_Type = 'Intake' THEN 'Surface Water'
        END AS Source_Type,
        mp_permits.MP_Name AS MP_Name,
        --hydroawrr.facility_hydroid,
        mp_permits.CEDS_Facility_Id AS CEDS_Facility_Id,
        mp_permits.Facility_Name AS Facility,
        CASE 
          WHEN substr(mp_permits.Permit_Number,1,3) = 'WWR' THEN NULL
          ELSE mp_permits.Permit_Number
        END AS Permit_Number,
        CASE 
          WHEN substr(mp_permits.Permit_Number,1,3) = 'WWR' THEN NULL
          ELSE mp_permits.Permit_Classification
        END AS Permit_Classification,
        w.Year AS Year,
        ROUND(SUM(w.Withdraw_Volume) / 1000000, 3) AS Water_Use_MGY
        
        FROM mps mp_permits
        LEFT JOIN monthly_withdrw w
          ON  mp_permits.Measuring_Point_Id = w.Measuring_Point_Id
          
        WHERE mp_permits.MP_Type != 'Transfer'
          AND (mp_permits.Water_Use_Type != 'Other' OR mp_permits.Water_Use_Type IS NULL)
          AND mp_permits.CEDS_Facility_ID IS NOT NULL --This is needed to remove unassociated MPs
        
        GROUP BY 
         
        "))
      
      self$withdrawals <- withdrawals
      
      return(withdrawals)
    }
  )
  
  
)


