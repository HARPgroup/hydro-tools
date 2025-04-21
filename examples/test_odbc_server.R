options(scipen=999)
library('hydrotools')
library('zoo')
library("knitr")
library("stringr")
basepath='/var/www/R';
source("/var/www/R/config.R")
source("https://raw.githubusercontent.com/HARPgroup/hydro-tools/master/R/fac_utils.R")

model_pid = 4824696

prev <- RomProperty$new(
  ds, list(
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

prev$tsvalues()
ts2 <- RomTS$new(
  ds, list(
    featureid=prev$pid, 
    varkey = 'om_class_Constant',
    tstime = as.numeric(as.POSIXct("2025-01-01", tz="America/New_York")),
    entity_type='dh_properties',
    tsvalue = 100.0
  ), TRUE
)
ts2$save(TRUE)
prev2$delete(TRUE)
ds$debug = TRUE
ts2retrieve <- RomTS$new(
  ds, list(
    featureid=prev$pid, 
    varkey = 'om_class_Constant',
    tstime = as.numeric(as.POSIXct("2025-01-01", tz="America/New_York")),
    entity_type='dh_properties'
  ), TRUE
)
ts2retrieve$tsvalue = 101.5
ts2retrieve$save(TRUE)
ts2rtid = ts2retrieve$tid
fn$sqldf("select tsvalue from dh_timeseries where tid = $ts2rtid", connection=ds$connection)

