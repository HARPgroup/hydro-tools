## Settin up a CEDSDataSource R6

# CEDSDataSource ####
#' Datasource for CEDS ODS
#' @description CEDSDataSource mirrors the functionality of RomDataSource, but for the CEDS 
#' ODS server. It servees as the connection to ODS to be passed into the other CEDS R6 
#' objects. This is a read only connection, as there can be no programatic writing to CEDS
#' @details Has a standard method to pull a feature of permit from ODS. Since the tables
#' used by OWS do not have standard names, the supplimentary function \code{fn_get_table_names}
#' is used to standardize the query.
#' @importFrom R6 R6Class  
#' @param server Optional arguemnt to point to PROD or TEST version. Defaults to PROD
#' @return Instance of CEDSDataSource, which can be fed into other CEDS R6 objects
#' @examples \dontrun{
#'#Get new datasource via odbc
#'dsCEDS <- CEDSDatasource()
#'}
#' @export CEDSDataSource
CEDSDataSource <- R6Class(
  "CEDSDataSource",
  private = list(
    ## I dont know if some connection info should be here or not
  ),
  public = list(
    
    #' @field connection The pool connection to ODS. This is called by \code{dbGetQuery} to access CEDS data
    connection = NULL,
    #' @field dbname The server its connected to, either TEST or PROD
    dbname = NULL,
    
    #' @description
    #' Initializes the instance of CEDSDataSource. This can then be fed into other CEDS r6 objects. 
    #' May require up to date drivers (used 'ODBC Driver 18 for SQL Server' at the time of development).
    #' Set up with code form DEQmethods::databaseconnect, so if running on the posit server will use its drivers
    #' @param server The ODS server the object is connected to. Defaults to PROD, but can be set to TEST
    #' @return object instance of CEDSDataSource with populated connection
    initialize = function(server) {
      ## Code taken from DEQmethods databaseconnect. I wasnt sure if I could use this IF this function were going to hydrotools
      if (server == "PROD") {
        serverAddress <- "DEQ-SQLODS-PROD,50000"
        self$dbname <- "PROD"
      }
      else if (server == "TEST") {
        serverAddress <- "DEQ-SQL-TEST,50000"
        self$dbname <- "TEST"
      }
      drivers <- unique(odbc::odbcListDrivers()$name)
      localDriver <- sort(decreasing = TRUE, ((drivers[grepl("ODBC Driver .* for SQL Server", 
                                                             drivers)])))[1]
      if (grepl("C:\\\\Users", Sys.getenv("USERPROFILE"))) {
        self$connection <- dbPool(odbc::odbc(), Driver = localDriver, Server = serverAddress, 
                                  database = "ODS", trusted_connection = "yes", TrustServerCertificate = "yes")
      }
      else {
        self$connection <- dbPool(drv = odbc::odbc(), Driver = Sys.getenv("driver"), 
                                  Server = Sys.getenv("server"), dbname = Sys.getenv("db"), 
                                  UID = Sys.getenv("userid"), PWD = Sys.getenv("pwd"))
      }
    },
    
    #' @description
    #' A generic function to find a variety of 'features', be it a facility or different kind of permits, from
    #' their respective table. Since these tables can have different column names and setups, requires using
    #' the \code{fn_get_table_names} to attempt to standardize. Set up similar to its respective function in
    #' \code{RomDataSource}, in which it searches either by a primary key ID or a list of other supplied fields
    #'  This function will return the row of the respective table.
    #' @param entity_type What type of feature are you looking for. Either a facility, or the specific type of 
    #' permit. This is a set list, of either facility, WWR, VWP, GWP, or VPDES. THe function will fail if it is 
    #' not one of the required types.
    #' @param pkid The priamry ID of the feature you are searching for. This is the CEDS ID of the permit or facility
    #' that will be returned. This has to be the ID of the feature you are looking for, so if the entity_type is a
    #' VWP, it cannot be a facility ID.
    #' @param config A list of column names and their values to search by. This can be any column in the table, but
    #' it must match the exact column name. Since the tables are all different, the values in config must line up with
    #' the respective columns for the entity_type.
    #' @param tbl_info A list as supplied by \code{fn_get_table_names} for the desired entity_type. This is always already created
    #' before \code{get()} is called, so it was added as a parameter instead of rerunning it within the function.
    #' @return object A dataframe of the row of the entity from the table (as determined by entity_type)
    get = function(entity_type = c("facility","WWR","VWP","GWP","VPDES"), pkid = NA, config = list(), tbl_info) {
    ## pk is a character representing the name of the primary ID in the config list, if supplied
    ## The generic get function ffrom different tables. Based on RomDataSource$get
      
      entity_type <- match.arg(entity_type)
      
      entity_names <- tbl_info
      
      ## Setting up SELECT
      sql_select <- "SELECT TOP 1 * "
      
      ## Setting up FROM
      sql_from <- paste("FROM", entity_names$tbl_name)
      
      ## If there is a pk specified, use that to find the feature
      if (!is.na(as.numeric(pkid))) {
        pkid <- as.numeric(pkid)
      }
      
      ## Setting up the WHERE
      sql_where <- ""
      if (!is.null(pkid)& !is.na(pkid)) {
        ## pkid depends on what table your after
        ## If pkid, ignore config
        sql_where <- paste("WHERE",entity_names$pk_col, "=", pkid)
        
      } else {
        
        clauses <- paste0("[",names(config), "] = '", config,"'")
        
        sql_where <- paste("WHERE", paste(clauses, collapse = " AND "))
        
      }
      
      sql_order <- ""
      ## If there is a sort column, include it here
      ## For VPDES and GWP, if searching for a given facility ID or permit number, this ensures the active is selected
      if (!is.null(entity_names$sort_col)) {
        sql_order <- paste(" ORDER BY ",entity_names$sort_col)
      }
      
      sql_query <- paste(sql_select, sql_from, sql_where, sql_order)
      
      entity <- sqldf(sql_query, connection = self$connection, method = "raw")
      
      return(entity)
    }
    
    
  )
  
)
