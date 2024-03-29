#' Retrieve a token from vaydro REST service.  TBD: replace with object to store token and dn connection
#'
#' @param uri character deprecated to be replaced by global datasource
#' @param token for CSRF autentication use
#' @param ctype content-type
#' @param delim delimiter to expect in final data set
#' @param enc encoding
#' @return data frame from delimited file
#' @seealso NA
#' @export om_auth_read
#' @examples NA
om_auth_read <- function(uri, token, ctype = "text/csv", delim=',', enc="xml") {
  # New method with httr
  # specifically used with csrf token authentication
  # Helps to allow any view to be retrieved with full system authentication
  rawdat <- httr::GET(
    uri,
    httr::add_headers(HTTP_X_CSRF_TOKEN = token),
    encode = enc, httr::content_type(ctype)
  )
  if (httr::http_error(rawdat)) {
    return(FALSE)
  }
  cdat <- httr::content(rawdat, type = ctype)
  return(cdat)
}