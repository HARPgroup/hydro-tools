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
fn_post_odbc <- function(entity_type, pk, inputs, con, obj=FALSE){
  #Search for existing ts matching supplied varkey, featureid, entity_type 
  #print(inputs)
  pkid <- as.integer(as.character(inputs[pk]))
  #message(paste("Called fn_post_odbc(", entity_type, ",", pk, ", pkid=", pkid, ")"))
  
  for (j in 1:length(inputs)) {
    if (is.na(inputs[j])) {
      inputs[j] <- NULL
    }
  }
  #message(inputs)
  #message(paste("pk= ", pkid))
  this_result <- list(
    status = FALSE
  )
  if (!is.na(match(pk, names(inputs)) )) {
    #remove this for saving if it is not null
    iix <- which(names(inputs) == pk)
    inputs  <- inputs[-iix]
    # the reason we remove this is that the REST service bombs if we 
    # send it a pk, like tid or pid, and this is unnecessary because 
    # the pkid is part of the URL so no worries.
    #message(paste("removed ", pk, " from inputs"))
  }
  if ( is.na(pkid) | is.null(pkid) ) {
    message(paste0("----- Creating ", entity_type, "..."))
    this_result <- httr::POST(
      paste0(site, "/",entity_type, "/"), 
      httr::add_headers(HTTP_X_CSRF_TOKEN = token),
      body = inputs,
      encode = "json"
    )
    # need to harvest the id col since this is an insert
    rest_parts = httr::content(this_result)
    #print(paste("Parts:", rest_parts))
    pkid = as.integer(rest_parts$id)
  } else {
    message(paste0("----- Updating ", entity_type, "..."))
    #message(paste("PUT URL: ", paste0(site, "/",entity_type, "/", pkid)))
    this_result <- httr::PUT(
      paste0(site, "/",entity_type, "/", pkid), 
      httr::add_headers(HTTP_X_CSRF_TOKEN = token),
      body = inputs,
      encode = "json"
    );
    #print(this_result)
  }
  #rest_parts = strsplit(this_result$url, '/', fixed = TRUE)
  #print(paste("Rest Parts:", rest_parts))
  #pkid = as.integer(rest_parts[[1]][length(rest_parts[[1]])])
  
  if (!is.logical(this_result$status )) {
    return_id <- switch(
      this_result$status,
      "200" = pkid,
      "201" = pkid,
      "400" = FALSE,
      "500" = FALSE
    )
  } else {
    pkid = FALSE
  }
  message(paste("REST returned", pkid))
  return(pkid)
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
  message(entity_type)
  message(paste(inputs))
  get_sql = FALSE
  print(obj)
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
  get_sql = paste(get_sql, "WHERE", get_where, limits)
  message(get_sql)
  entities = sqldf(get_sql, connection = con, method = "raw")
  if (is.logical(entities)) {
    message("----- This entity does not exist")
    entities = FALSE
  } else {
    message(paste("Total =", nrow(entities)))
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

fn_guess_sql_where <- function(entity_type, pk, inputs) {
  get_where = ""
  pkid <- as.integer(as.character(inputs[pk]))
  if (is.na(pkid)) {
    pkid = NULL
  }
  # remove special things that are not part of the columns
  inputs$limit <- NULL
  inputs$page <- NULL
  if (!is.null(pkid)) {
    # Simple PK retrieval
    get_where = paste(pk,"=",pkid)
  } else {
    get_where_glue = ""
    #message(paste("inputs:", inputs))
    for (col_name in names(inputs)) {
      col_val = inputs[[col_name]]
      if (is.na(inputs[col_name])) {
        inputs[col_name] <- NULL
      }
      #message(paste(col_name,'=',typeof(col_val)))
      if (is.character(col_val)) {
        col_val = paste0("'",col_val,"'")
      }
      if (!is.null(inputs[col_name])) {
        get_where = paste(
          get_where, 
          get_where_glue, 
          col_name,"=", col_val
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