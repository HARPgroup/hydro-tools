#' Post any entity to via an ODBC connection. This allows users to insert or
#' update entities within database.
#'
#' @param entity_type = dh_feature, dh_properties, ...
#' @param pk = primary key column name, e.g. hydroid, pid, ...
#' @param inputs contents of record to post in list (pid, propname, propvalue,
#'   ...). If missing essential values, post will fail. Required information may
#'   be found in each table description on the postgres database. Contact OWS
#'   WSPA data coordinator for more information
#' @param con connection to ODBC server
#' @param obj optional class with extra query info
#' @seealso fn_get_odbc, fn_post_rest
#' @export fn_post_odbc
#' @examples 
#' \dontrun{
#' fn_post_odbc('dh_properties','pid',
#' inputs = list(pid = NA,bundle = NA, featureid = NA, etc.),
#' con = datasource$connection, obj = FALSE)
#' }
fn_post_odbc <- function(entity_type, pk, inputs, con, obj=FALSE){
  #Search for existing ts matching supplied varkey, featureid, entity_type 
  #print(inputs)
  if (!is.na(pk)) {
    pkid <- as.integer(as.character(inputs[[pk]]))
    if (!is.null(pkid)) {
      message(paste("Final na/null check for pk",pk))
      if (is.na(pkid)) {
        pkid = NULL
      }
    }
  } else {
    pkid = NULL
  }
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
  #message(odbc_sql)
  pkid <- sqldf(as.character(odbc_sql), connection = con)
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
#' @seealso NA
#' @export fn_post_odbc
#' @examples NA
fn_delete_odbc <- function(entity_type, pk, inputs, con, obj=FALSE){
  #Search for existing ts matching supplied varkey, featureid, entity_type 
  #print(inputs)
  # note: we do not currently support non-integer pk columns
  pkid <- as.integer(as.character(inputs[pk]))
  if ( is.na(pkid) | is.null(pkid) ) {
    message(paste0("----- Warning: cannot delete entity", entity_type, "without ", pk))
    return(FALSE)
  } else {
    #message(paste0("----- deleting ", entity_type, "..."))
    odbc_sql = paste("DELETE from", entity_type, "WHERE", pk, "=", pkid)
  }
  #print(odbc_sql)
  result <- sqldf(as.character(odbc_sql), connection = con)
  #message(paste("ODBC returned", result))
  return(result)
}

#' Get any entity from a RESTful web service
#'
#' @param entity_type = dh_feature, dh_properties, ...
#' @param pk = primary key column name, e.g. hydroid, pid, ...
#' @param inputs contents of record to post in list(pid, propname, propvalue, ...)
#' @param con connection to ODBC server
#' @param obj optional class with extra query info
#' @export fn_get_odbc
#' @examples NA
fn_get_odbc <- function(entity_type, pk, inputs, con, obj=FALSE){
  #Search for existing ts matching supplied varkey, featureid, entity_type 
  #message(entity_type)
  #message(paste(inputs))
  get_sql = FALSE
  #print(obj)
  if (!is.logical(obj)) {
    if ("sql_select_from" %in% names(obj) 
        & (length(obj[["sql_select_from"]]) > 0)
    ) {
      get_sql = obj$sql_select_from
    }
  }
  if (is.logical(get_sql)) {
    sql_stuff <- fn_guess_sql(entity_type, pk, inputs)
    get_sql = sql_stuff$get_sql
  }
  get_where = fn_guess_sql_where(entity_type, pk, inputs)
  limits = fn_guess_limits(entity_type, pk, inputs)
  # put it all together
  if (nchar(trimws(get_where)) == 0) {
    where_pre = ""
  } else {
    where_pre = "WHERE"
  }
  get_sql = paste(get_sql, where_pre, get_where, limits)
  #message(get_sql)
  entities = sqldf(get_sql, connection = con, method = "raw")
  if (is.logical(entities)) {
    message("----- This entity does not exist")
    entities = FALSE
  } else {
    #message(paste("Total =", nrow(entities)))
  }
  return(entities)
}

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

#' Guess an SQL query from a simple list of inputs
#'
#' @param entity_type = dh_feature, dh_properties, ...
#' @param pk = primary key column name, e.g. hydroid, pid, ...
#' @param inputs contents of record to post in list(pid=X, propname='nom', propvalue, ...)
#' @param alias assign an alias to a give table/view relation?
#' @export fn_guess_sql_where
#' @examples NA
fn_guess_sql_where <- function(entity_type, pk, inputs, alias="") {
  get_where = ""
  if (alias != "") {
    alias = paste0(alias,".")
  }
  pkid <- as.integer(as.character(inputs[pk]))
  if (is.na(pkid) | is.null(pkid)) {
    pkid = NULL
  }
  # remove special things that are not part of the columns
  inputs$limit <- NULL
  inputs$page <- NULL
  if (!is.null(pkid)) {
    # Simple PK retrieval
    if (!is.na(pkid)) {
      get_where = paste0(alias, pk," = ",pkid)
    }
  } else {
    get_where_glue = ""
    #message("inputs:")
    #print(inputs)
    for (col_name in names(inputs)) {
      if (is.na(inputs[col_name])) {
        inputs[col_name] <- NULL
        next
      }
      col_val = inputs[[col_name]]
      #message(paste(col_name,'=',typeof(col_val)))
      if (is.character(col_val)) {
        col_val = paste0("'",col_val,"'")
      }
      if (!is.null(inputs[col_name]) & !is.na(inputs[col_name])) {
        get_where = paste(
          get_where, 
          get_where_glue, 
          paste0(alias, col_name)," = ", col_val
        )
        get_where_glue = "AND"
      }
    }
  }
  return(get_where)
}


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
    if (is.na(inputs[col_name])) {
      inputs[col_name] <- NULL
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
    if (is.na(inputs[col_name])) {
      inputs[col_name] <- NULL
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
