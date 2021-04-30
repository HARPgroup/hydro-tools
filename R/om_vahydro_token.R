#' Retrieve a token from vaydro REST service.  TBD: replace with object to store token and dn connection
#'
#' @param base_url character deprecated to be replaced by global datasource
#' @param rest_uname username to authenticate (if omitted will prompt)
#' @return token character from VAHydro REST web service
#' @seealso NA
#' @export om_vahydro_token
#' @examples NA
om_vahydro_token <- function(base_url='http://deq2.bse.vt.edu/d.dh', rest_uname = NULL) {
  if (is.null(rest_uname)) {
    # readline does *not* wait for input when run inside a fn
    # so we use getPass which does.
    #rest_uname <- readline("REST Username: ")
    rest_uname <- getPass::getPass("REST User Name: ")
  }
  rest_pw <- getPass::getPass("REST Password: ")
  message(paste("reading from", base_url))
  # do something
  #Cross-site Request Forgery Protection (Token required for POST and PUT operations)
  csrf_url <- paste(base_url,"restws/session/token/",sep="/");
  print(paste("REST AUTH INFO HAS BEEN SUPPLIED",sep=""))
  print(paste("RETRIEVING REST TOKEN",sep=""))
  csrf <- GET(url=csrf_url,authenticate(rest_uname,rest_pw));
  print(csrf)
  token <- content(csrf);
  
  if (length(token)==1){
    print("Login attempt successful")
    print(paste("token = ",token,sep=""))
  } else {
    print("Login attempt unsuccessful")
  }
  return(token)
}