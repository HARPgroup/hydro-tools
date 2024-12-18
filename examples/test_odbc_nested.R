options(scipen=999)
library('hydrotools')
library('zoo')
library("knitr")
basepath='/var/www/R';
source("/var/www/R/config.R")
source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/R/fac_utils.R")
dso <- RomDataSource$new(site, rest_uname = odbc_uname, connection_type = 'odbc', dbname = 'drupal.dh03')
dso$get_token(rest_pw = odbc_pw)
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw = rest_pw)

model_pid = 4824696
model <- FALSE
model <- RomPropertyTree$new(dso, list(root_pid=model_pid), TRUE)
model$prop_list # this might be temporary 

vardef <- FALSE
vardef <- RomVariableDefinition$new(dso, list(varkey='wd_mgy'), TRUE)
