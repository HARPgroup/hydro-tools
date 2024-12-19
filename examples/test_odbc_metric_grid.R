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

# GET VAHydro 1.0 RIVERSEG l90_Qout DATA
mdf <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0','vahydro-1.0',  'vahydro-1.0'),
  'runid' = c('runid_1000', 'runid_0', 'runid_11', 'runid_0', 'runid_11', 'runid_0'),
  'metric' = c('Qout','Qout', 'wd_cumulative_mgd', 'wd_cumulative_mgd','l30_Qout', 'l30_Qout'),
  'runlabel' = c('Qout_1000', 'Qout_0', 'wd_11', 'wd_0', 'l30_11', 'l30_0')
)
wshed_data_odbc <- om_vahydro_metric_grid(
  metric = FALSE, runids = mdf,
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = dso
)
wshed_data <- om_vahydro_metric_grid(
  metric = FALSE, runids = mdf,
  base_url = paste(site,'entity-model-prop-level-export',sep="/"),
  ds = ds
)

tn_odbcdata = fn_extract_basin(wshed_data_odbc,'TU4_9260_0000')
tn_data = fn_extract_basin(wshed_data,'TU4_9260_0000')
