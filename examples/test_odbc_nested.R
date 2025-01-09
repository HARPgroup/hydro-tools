options(scipen=999)
library('hydrotools')
library('zoo')
library("knitr")
library("stringr")
basepath='/var/www/R';
source("/var/www/R/config.R")
source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/R/fac_utils.R")

fac_hydroid <- 72017
feature <- RomFeature$new(ds, list(hydroid=fac_hydroid), TRUE)
mps <- feature$get_mps()
mp <- RomFeature$new(ds, list(hydroid=mps[1,]$hydroid), TRUE)
riverseg = mp$find_spatial_relations(
  inputs=list(bundle='watershed', ftype='vahydro'), 
  operator='st_within',
  return_geoms=TRUE
)

model_version = "vahydro-1.0"

# get the river segment model
riverseg_feature <- RomFeature$new(ds, list(hydroid=as.integer(riverseg$hydroid)), TRUE)
rseg_model <- RomProperty$new(
  ds, list(
    propcode=model_version,
    featureid=riverseg_feature$hydroid,
    entity_type='dh_feature'
  ), TRUE
)
rseg_model_export <- dso$get_json_prop(rseg_model$pid)
# remove the outer name on the list to make access easier
# TBD: this should be done in the datasource. 
rseg_model_export <- rseg_model_export[[(as.character(names(rseg_model_export)))]]

# get the facility model
model <- RomProperty$new(
  ds, list(
    propcode=model_version,
    featureid=feature$hydroid,
    entity_type='dh_feature'
  ), TRUE
)


# this loads a single property under this model
ps_enabled <- RomProperty$new(
  dso, list(
    propname='ps_enabled', 
    featureid=model$pid, 
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
model_export <- dso$get_json_prop(model$pid)

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
