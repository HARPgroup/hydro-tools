#' Retrieve a token from vaydro REST service.  TBD: replace with object to store token and dn connection
#'
#' @param base_url character deprecated to be replaced by global datasource
#' @return token from VAHydro REST web service
#' @seealso NA
#' @export om_vahydro_get_token
#' @examples NA
om_vahydro_get_token <- function(base_url='http://deq2.bse.vt.edu/d.dh') {
  rest_uname <- readline("REST Username: ")
  rest_pw <- getPass::getPass("REST Password: ")
  # do something
  #Cross-site Request Forgery Protection (Token required for POST and PUT operations)
  csrf_url <- paste(base_url,"restws/session/token/",sep="/");
  
  #IF THE OBJECTS 'rest_uname' or 'rest_pw' DONT EXIST, USER INPUT REQUIRED
  if (!is.character(rest_uname) | !(is.character(rest_pw))){
    
    rest_uname <- c() #initialize username object
    rest_pw <- c()    #initialize password object
    
    #currently set up to allow infinite login attempts, but this can easily be restricted to a set # of attempts
    token <- c("rest_uname","rest_pw") #used in while loop below, "length of 2"
    login_attempts <- 1
    if (!is.character(rest_uname)) {
      print(paste("REST AUTH INFO MUST BE SUPPLIED",sep=""))
      while(length(token) == 2  && login_attempts <= 5){
        print(paste("login attempt #",login_attempts,sep=""))
        
        rest_uname <- readline(prompt="Enter REST user name: ")
        rest_pw <- readline(prompt="Password: ")
        csrf <- GET(url=csrf_url,authenticate(rest_uname,rest_pw));
        token <- content(csrf);
        #print(token)
        
        if (length(token)==2){
          print("Sorry, unrecognized username or password")
        }
        login_attempts <- login_attempts + 1
      }
      if (login_attempts > 5){print(paste("ALLOWABLE NUMBER OF LOGIN ATTEMPTS EXCEEDED"))}
    }
    
  } else {
    print(paste("REST AUTH INFO HAS BEEN SUPPLIED",sep=""))
    print(paste("RETRIEVING REST TOKEN",sep=""))
    csrf <- GET(url=csrf_url,authenticate(rest_uname,rest_pw));
    token <- content(csrf);
  }
  
  if (length(token)==1){
    print("Login attempt successful")
    print(paste("token = ",token,sep=""))
  } else {
    print("Login attempt unsuccessful")
  }
  return(token)
}