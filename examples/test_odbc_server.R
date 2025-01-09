options(scipen=999)
library('hydrotools')
library('zoo')
library("knitr")
library("stringr")
basepath='/var/www/R';
source("/var/www/R/config.R")
source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/R/fac_utils.R")
dso <- RomDataSource$new(site, rest_uname = odbc_uname, connection_type = 'odbc', dbname = 'drupal.dh03')
dso$get_token(rest_pw = odbc_pw)

dso <- RomDataSource$new("http://192.168.0.21", rest_uname = odbc_uname, connection_type = 'odbc', dbname = 'drupal.dh03')
dso$get_token(rest_pw = odbc_pw, odbc_port=5432) 

model_pid = 4824696

prev <- RomProperty$new(
  dso, list(
    propname='test_variable', 
    featureid=model_pid, 
    entity_type='dh_properties'
  ), TRUE
)

prev$propvalue = 14900
prev$save(TRUE)


prev2 <- RomProperty$new(
  dso, list(
    propname='test_variable2', 
    featureid=model_pid, 
    varkey = 'om_class_Equation',
    entity_type='dh_properties',
    propcode = '5.5 * 1000'
  ), TRUE
)
prev2$save(TRUE)
prev2$delete(TRUE)

prev$propvalue = 14000
prev$save(TRUE)


upsql = "UPDATE dh_properties SET   vid = 8332550 , entity_type = 'dh_properties' , varid = 1 , bundle = 'dh_properties' , featureid = 4824696 , propname = 'test_variable' , propvalue = 13000 WHERE pid = 7685242"
sqldf(upsql, connection=dso$connection)
  