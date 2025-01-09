options(scipen=999)
library('hydrotools')
library('zoo')
library("knitr")
library("stringr")
basepath='/var/www/R';
source("/var/www/R/config.R")
source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/R/fac_utils.R")
#ds <- RomDataSource$new(site, rest_uname = rest_uname)
#ds$get_token(rest_pw = rest_pw)
dso <- RomDataSource$new(site, rest_uname = odbc_uname, connection_type = 'odbc', dbname = 'drupal.dh03')
dso$get_token(rest_pw = odbc_pw)
## If testing on internal network http://192.168.0.21
#dso <- RomDataSource$new("http://192.168.0.21", rest_uname = odbc_uname, connection_type = 'odbc', dbname = 'drupal.dh03')
#dso$get_token(rest_pw = odbc_pw, odbc_port=5432) 

fac_hydroid <- 72017
model_version = "vahydro-1.0"
model <- RomProperty$new(
  dso, list(
    propcode=model_version,
    featureid=fac_hydroid,
    entity_type='dh_feature'
  ), TRUE
)
# this loads a single property under this model
ps_enabled <- RomProperty$new(
  dso, list(
    propname='ps_enabled', 
    featureid=model_pid, 
    entity_type='dh_properties'
  ), TRUE
)
ps_enabled$data_matrix

# now, we can do an export to json, but it will only get local
# properties that have already been retrieved/created
model_export_local <- dso$get_nested_export(dso, model_pid, dso$propvalues)
# this goes to the database and insure that all properties under the parent
# property have been downloaded, then exports:
model_tree <- RomPropertyTree$new(dso, list(root_pid=model_pid), TRUE)
model_export <- dso$get_json_prop(model_pid)

blank_prop <- RomProperty$new(
  dso, list(featureid=model$pid, varkey='wd_mgy', propname='test_var', TRUE)
)

        
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


fn_guess_insert(
  "dh_properties", 
  "pid",
  list(
    propname='fac_demand_mgy', 
    featureid=model_pid, 
    entity_type='dh_properties'
  )
)

fn_post_odbc(
  "dh_properties", 
  "pid",
  list(
    propname='test_variable', 
    featureid=model_pid, 
    entity_type='dh_properties',
    propvalue = 12050
  ),
  con = dso$connection
)
fn_post_odbc(
  "dh_properties", 
  "pid",
  list(
    propname='test_variable', 
    featureid=model_pid, 
    entity_type='dh_properties',
    propvalue = 12050
  ),
  con = dso$connection
) 
prev <- RomProperty$new(
  dso, list(
    propname='test_variable', 
    featureid=model_pid, 
    entity_type='dh_properties',
    propvalue = 12050
  ), TRUE
)
prev <- RomProperty$new(
  dso, list(
    propname='test_variable', 
    featureid=model_pid, 
    entity_type='dh_properties'
  ), TRUE
)

prev$propvalue = 13000
prev$save(TRUE)

model$to_list()
