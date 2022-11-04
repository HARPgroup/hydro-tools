library(pander);
library(httr);
library(zoo);
library(hydrotools);

site <- "http://deq1.bse.vt.edu/d.alpha"    #Specify the site of interest, either d.bet OR d.dh
omsite = site

# Load Libraries
basepath='/var/www/R'
source('/var/www/R/config.R')
options(timeout=1200) # set timeout to twice default level to avoid abort due to high traffic


################################################################################################
source(paste(hydro_tools_location,'/VAHydro-2.0/rest_functions.R',sep=""))

??vahydro_auth_read()

################################################################################################
################################################################################################

