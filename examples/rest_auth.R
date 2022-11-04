library(httr);

#----------------------------------------------
#site <- "http://deq2.bse.vt.edu/d.dh"    #WORKS
site <- "http://deq2.bse.vt.edu/d.bet"
#----------------------------------------------
# Load Libraries
basepath="C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitHub\\hydro-tools";
#source(paste(basepath,'config.local.private',sep='/'));
source(paste(basepath,"VAHydro-2.0/rest_functions.R", sep = "/"));

#retrieve rest token
source(paste(basepath,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
token <- rest_token(site, token, rest_uname, rest_pw)
