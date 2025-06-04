#' Post any entity to via an ODBC connection. This allows users to insert or
#' update entities within database.
#'
#' @param entity_type Most often dh_feature or dh_properties. Indicates which 
#'   table to retrieve data from. This will be the target table for the query
#'   constructed from user inputs input list
#' @param pk = primary key column name, e.g. hydroid, pid, ...
#' @param inputs contents of record to post in list (pid, propname, propvalue,
#'   ...). If missing essential values, post will fail. Required information may
#'   be found in each table description on the postgres database. Contact OWS
#'   WSPA data coordinator for more information
#' @param con connection to ODBC server
#' @param obj optional class with extra query info
#' @param debug optional show query pieces
#' @seealso fn_get_odbc, fn_post_rest
#' @export fn_post_odbc
#' @examples 
#' \dontrun{
#' fn_post_odbc('dh_properties','pid',
#' inputs = list(pid = NA,bundle = NA, featureid = NA, etc.),
#' con = datasource$connection, obj = FALSE)
#' }
fn_post_odbc <- function(entity_type, pk, inputs, con, obj=FALSE, debug = FALSE){
  #Search for existing ts matching supplied varkey, featureid, entity_type 
  #print(inputs)
  if (!is.na(pk)) {
    if (pk %in% names(inputs)) {
      pkid <- as.integer(as.character(inputs[[pk]]))
      if (!is.null(pkid)) {
        message(paste("Final na/null check for pk",pk,"val",pkid))
        if (is.na(pkid)) {
          pkid = NULL
        }
      }
    } else {
      pkid = NULL
    }
  } else {
    pkid = NULL
  }
  # this seems to be the only case where using a double bracket [[ ]]
  # is NOT the way to go.  This fails with double brackets, but is.null() tests
  # fail with single brackets.  is.na() appears to work correctly regardless of single
  # or double brackets.
  inputs <- inputs[!is.na(inputs)]
  this_result <- list(
    status = FALSE
  )
  if ( is.null(pkid) ) {
    message(paste0("----- Creating ", entity_type, "..."))
    odbc_sql = fn_guess_insert(entity_type, pk, inputs)
  } else {
    message(paste0("----- Updating ", entity_type, "..."))
    odbc_sql = fn_guess_update(entity_type, pk, inputs)
  }
  if (debug == TRUE) {
    message(paste("Debug: ODBC update/insert:", odbc_sql))
  }
  # temporarily use DBI until we understand more fully what sqldf might be doing
  # see also: 
  pkid <- sqldf(as.character(odbc_sql), connection = con, envir = environment())
  #pkid <- DBI::dbGetQuery(con, as.character(odbc_sql))
  if (nrow(pkid) > 0) {
    pkid <- pkid[1,pk]
  } else {
    pkid = FALSE
  }
  #message(paste("ODBC returned", pkid))
  return(pkid)
}

#' Post any entity to a RESTful web service
#'
#' @param entity_type = dh_feature, dh_properties, ...
#' @param pk = primary key column name, e.g. hydroid, pid, ...
#' @param inputs contents of record to post in list(pid, propname, propvalue, ...)
#' @param con connection to ODBC server
#' @param obj optional class with extra query info
#' @param debug Print out debug info if true
#' @seealso NA
#' @export fn_post_odbc
#' @examples NA
fn_delete_odbc <- function(entity_type, pk, inputs, con, obj=FALSE, debug=FALSE){
  #Search for existing ts matching supplied varkey, featureid, entity_type 
  #print(inputs)
  # try to enable multiple key matches
  if (pk == "") {
    # try to use list of inputs
    dwhere = fn_guess_sql_where(entity_type, pk, inputs)
    odbc_sql = paste("DELETE from", entity_type, "WHERE", dwhere)
  } else {
    # note: we do not currently support non-integer pk columns
    pkid <- as.integer(as.character(inputs[pk]))
    if ( is.na(pkid) | is.null(pkid) ) {
      message(paste0("----- Warning: cannot delete entity", entity_type, "without ", pk))
      return(FALSE)
    } else {
      #message(paste0("----- deleting ", entity_type, "..."))
      odbc_sql = paste("DELETE from", entity_type, "WHERE", pk, "=", pkid)
    }
  }
  if (debug == TRUE) {
    message(paste("Debug: ODBC DELETE", odbc_sql))
  }
  result <- sqldf(as.character(odbc_sql), connection = con)
  return(result)
}

#' Get any entity from a remote database, often dbase2/3 via ODBC connection
#' (often set in an instance of RomDataSource)
#'
#' @param entity_type Most often dh_feature or dh_properties. Indicates which 
#'   table to retrieve data from. This will be the target table for the query
#'   constructed from user inputs input list
#' @param pk Primary key column name, often hydroid or pid. See Readme for
#'   additional information or contact OWSPA Data Coordinatory if you do not
#'   know the appropriate primary key for the table of interest
#' @param inputs A list of values that includes query constructors. Each entry
#'   in this list and its corresponding value will be added to the WHERE clause
#'   of a query that queries the table specified in entity_type with the
#'   exception of "limit", which will only be used to generate the LIMIT clause
#'   for the query. May include a primary key ID, in which case it will be all
#'   that is used in the WHERE clause. Relevant data for list may include
#'   propname, propvalue, hydrocode, or other fields shown in the Hydrotools
#'   Readme e.g. list(pid, propname, propvalue, ...)
#' @param con connection to ODBC server, usually provided via
#'   RomDataSource$connection()
#' @param obj optional class with extra query info specified via sql_select_from
#'   field
#' @param debug Print out debug info if true
#' @return The results of the query constructed based on inputs and pk from
#'   entity_type, typically a data.frame
#' @export fn_get_odbc
#' @examples NA
fn_get_odbc <- function(entity_type, pk, inputs, con, obj=FALSE, debug=FALSE){
  #Search for existing ts matching supplied varkey, featureid, entity_type 
  #if (debug == TRUE) {
  #  message(entity_type)
  #}
  #message(paste(inputs))
  get_sql = FALSE
  #print(obj)
  #If an object (some Rom instance) has been provided, get additional
  #information that user may have provided via sql_select_from field. Should be
  #a base query in the form of fn_guess_sql()
  if (!is.logical(obj)) {
    if ("sql_select_from" %in% names(obj) 
        & (length(obj[["sql_select_from"]]) > 0)
    ) {
      get_sql = obj$sql_select_from
    }
  }
  #If user has not provided sql_select_from field on obj, get the query
  if (is.logical(get_sql)) {
    #With no additional information, return a base query of SELECT * FROM
    #entity_type, ignoring limits and pk for now
    sql_stuff <- fn_guess_sql(entity_type, pk, inputs)
    get_sql = sql_stuff$get_sql
  }
  #Construct the WHERE clause of the query based on information provided by user
  get_where = fn_guess_sql_where(entity_type, pk, inputs)
  #Construct the LIMIT clause of the query if user has provided "limit" in inputs
  limits = fn_guess_limits(entity_type, pk, inputs)
  # put it all together, pasting the base query with any where or limit query
  if (nchar(trimws(get_where)) == 0) {
    where_pre <- ""
  } else {
    where_pre <- "WHERE"
  }
  #Create query by pasting all clauses together
  get_sql <- paste(get_sql, where_pre, get_where, limits)
  if (debug == TRUE) {
    message(get_sql)
  }
  #GET the query from the database, sending the SQL directly to the database via
  #the ODBC connection
  entities <- sqldf(get_sql, connection = con, method = "raw")
  #If nothing is returned, a logical is returned.
  if (is.logical(entities) || nrow(entities) == 0) {
    message("----- This entity does not exist")
    entities = FALSE
  } else {
    #message(paste("Total =", nrow(entities)))
  }
  #Returns the "raw" data from query
  return(entities)
}

#Return a very basic SELECT * FROM entity_type, ignoring pk and inputs
#Here, inputs is irrelevant but it is checked for a limit
fn_guess_sql <- function(entity_type, pk, inputs) {
  sql_stuff <- list()
  if (is.null(inputs$limit)) {
    inputs$limit = 0
  }
  # remove special things that are not part of the columns
  inputs$limit <- NULL
  get_sql = paste("select * from ", entity_type) 
  
  sql_stuff$get_sql <- get_sql
  return(sql_stuff)
}

#' Guess an SQL query with a WHERE clasue from a simple list of inputs, using
#' all inputs to construct WHERE
#'
#' @param entity_type Most often dh_feature or dh_properties. Indicates which 
#'   table to retrieve data from. This will be the target table for the query
#'   constructed from user inputs input list
#' @param pk Primary key column name, often hydroid or pid. See Readme for
#'   additional information or contact OWSPA Data Coordinatory if you do not
#'   know the appropriate primary key for the table of interest
#' @param inputs contents of record to post in list(pid=X, propname='nom',
#'   propvalue, ...). The pk column may (optional) be one of the entries in this
#'   list and this should serve as the primary key ID that the user is
#'   interested in identifying. If a primary key is found, it is the only item
#'   added to the WHERE clause. Otherwise, all contents of inputs will be added
#'   into the WHERE clause constructed from this query
#' @param alias assign an alias to a give table/view relation?
#' @export fn_guess_sql_where
#' @return A WHERE clasue to be added to an SQL query, perhaps generated via
#'   fn_guess_sql
#' @examples NA
fn_guess_sql_where <- function(entity_type, pk, inputs, alias="") {
  get_where = ""
  #If an alias has been provided, append with a literal period e.g "alias."
  if (alias != "") {
    alias = paste0(alias,".")
  }
  #Convert the primary key to an integer
  pkid <- as.integer(as.character(inputs[pk]))
  #If no primary key ID was provided, set to NULL
  if (is.na(pkid) || is.null(pkid)) {
    pkid = NULL
  }
  # remove special things that are not relevant to this function or are
  # referenced in futrue SQL constructors
  inputs$limit <- NULL
  inputs$page <- NULL
  #If a primary key has been provided, set WHERE clause on SQL query to be the
  #primary key field equal the primary key ID
  if (!is.null(pkid)) {
    # Simple PK retrieval
    if (!is.na(pkid)) {
      get_where = paste0(alias, pk," = ",pkid)
    }
  } else {
    get_where_glue = ""
    #message("inputs:")
    #print(inputs)
    #For each name in the inputs list, create a WHERE statement that is the name
    #of that input (adjusted for alias as needed) is equal to the value of that
    #input
    for (col_name in names(inputs)) {
      #Skip if the input value is NA
      if (is.na(inputs[[col_name]])) {
        inputs[[col_name]] <- NULL
        next
      }
      #Value associated with this list entry
      col_val = inputs[[col_name]]
      #message(paste(col_name,'=',typeof(col_val)))
      if (is.character(col_val)) {
        col_val = paste0("'",col_val,"'")
      }
      #If the value is set, paste into get_where, pasting onto the previous loop
      if (!is.null(col_val) && !is.na(col_val)) {
        get_where = paste(
          get_where, 
          get_where_glue, 
          paste0(alias, col_name)," = ", col_val
        )
        #For all future iterations, set the "glue" to AND to allow for the
        #construction of a single WHERE clause
        get_where_glue = "AND"
      }
    }
  }
  #Return the SQL WHERE clause
  return(get_where)
}

#If user has provided any limits, return a LIMIT SQL clause that can be added to
#the bottom of any query. Returns either an empty charactor or the limit clause
fn_guess_limits <- function(entity_type, pk, inputs) {
  if (is.null(inputs$limit)) {
    limit = ""
  } else {
    limit = paste ("limit",inputs$limit)
  }
  return(limit)
}


fn_guess_insert <- function(entity_type, pk, inputs) {
  col_sql = ""
  val_sql = ""
  in_sql = ""
  in_sep = ""
  for (col_name in names(inputs)) {
    if (is.na(inputs[[col_name]])) {
      inputs[[col_name]] <- NULL
      next
    }
    col_val = inputs[[col_name]]
    col_sql <- paste(col_sql, in_sep, col_name)
    if (is.character(col_val)) {
      col_val = paste0("'",col_val,"'")
    }
    val_sql <- paste(val_sql, in_sep, col_val)
    in_sep = ","
  }
  in_sql = paste(
    "INSERT INTO", entity_type, "(", col_sql, ")", 
    "VALUES", "(", val_sql, ")"
  )
  if (!is.null(pk)) {
    if (!is.na(pk)) {
      in_sql = paste(
        in_sql, "RETURNING", pk
      )
    }
  }
  return(in_sql)
}

fn_guess_update <- function(entity_type, pk, inputs) {
  val_sql = ""
  up_sql = ""
  up_sep = ""
  pk_val = NULL
  for (col_name in names(inputs)) {
    #print(paste("handling ", col_name))
    if (is.na(inputs[[col_name]])) {
      inputs[[col_name]] <- NULL
      next
    }
    col_val = inputs[[col_name]]
    if (is.character(col_val)) {
      col_val = paste0("'",col_val,"'")
    }
    if (col_name == pk) {
      #print(paste("Found pk col", pk))
      pk_val = col_val
      next
    }
    val_sql <- paste(val_sql, up_sep, col_name, "=", col_val)
    up_sep = ","
  }
  up_sql = paste(
    "UPDATE", entity_type, 
    "SET", val_sql, 
    "WHERE", pk, "=", pk_val
  )
  return(up_sql)
}

fn_pk_clause <- function(pk, inputs) {
  pk_clause = ""
  if (typeof(pk) == "character") {
    if (!is.null(inputs[[pk]])) {
      pk_clause = paste(pk, "=", inputs[[pk]])
    }
  } else {
    if (typeof(pk) == "list") {
      pk_glue = ""
      for (i in pk) {
        if (!is.null(inputs[[i]])) {
          ival = inputs[[i]]
          if (typeof(ival) == 'character') {
            ival = paste0("'",ival,"'")
          }
          pk_clause = paste(pk_clause, pk_glue, i, "=", ival)
          pk_glue = "AND"
        }
      }
    }
  }
  return(pk_clause)
}
