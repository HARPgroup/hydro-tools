options(scipen=999)
library('hydrotools')
library('zoo')
library("knitr")
library("stringr")
basepath='/var/www/R';
source("/var/www/R/config.R")
source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/R/fac_utils.R")
ds <- RomDataSource$new(site, rest_uname = rest_uname)
ds$get_token(rest_pw = rest_pw)
dso <- RomDataSource$new(site, rest_uname = odbc_uname, connection_type = 'odbc', dbname = 'drupal.dh03')
dso$get_token(rest_pw = odbc_pw)

model_pid = 4824696
model <- FALSE
model <- RomPropertyTree$new(dso, list(root_pid=model_pid), TRUE)
model$prop_list # this might be temporary 
nested = list()

fac_demand_prop <- RomProperty$new(
  dso, list(
    propname='fac_demand_mgy', 
    featureid=model_pid, 
    entity_type='dh_properties'
  ), TRUE
)
fac_demand_prop$data_matrix

vardef <- FALSE
vardef <- RomVariableDefinition$new(dso, list(varkey='wd_mgy'), TRUE)
