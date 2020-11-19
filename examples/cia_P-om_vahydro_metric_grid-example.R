# Upper and Middle Potomac cia table for model debugging
# where is the extra water comingfrom in 2040 baseline flows?

library("sqldf")
library("stringr") #for str_remove()

# Load Libraries
basepath='/var/www/R';
site <- "http://deq2.bse.vt.edu/d.dh"    #Specify the site of interest, either d.bet OR d.dh
source("/var/www/R/config.local.private"); 
source(paste(basepath,'config.R',sep='/'))
source(paste(hydro_tools_location,'/R/om_vahydro_metric_grid.R', sep = ''));
folder <- "C:/Workspace/tmp/"

# get the DA, need to grab a model output first in order to insure segments with a channel subcomp
# are included, hence the first column is to look for prop runid_11, Qbaseline, then the next 2 get 
df <- data.frame(
  'model_version' = c('vahydro-1.0',  'vahydro-1.0',  'vahydro-1.0'),
  'runid' = c('runid_11', '0.%20River%20Channel', 'local_channel'),
  'runlabel' = c('QBaseline_2020', 'comp_da', 'subcomp_da'),
  'metric' = c('Qbaseline', 'drainage_area', 'drainage_area')
)
da_data <- om_vahydro_metric_grid(metric, df)
da_data <- sqldf(
  "select pid, comp_da, subcomp_da,
   CASE
    WHEN comp_da is null then subcomp_da
    ELSE comp_da
    END as da
   from da_data
  ")

df <- data.frame(
  'model_version' = c('vahydro-1.0',  'vahydro-1.0',  'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c('runid_11', 'runid_13', 'runid_11', 'runid_13', 'runid_11', 'runid_13', 'runid_11', 'runid_13', 'runid_11', 'runid_13'),
  'metric' = c('Qbaseline', 'Qbaseline','l90_Qout','l90_Qout','wd_cumulative_mgd','wd_cumulative_mgd','ps_cumulative_mgd','ps_cumulative_mgd','ps_nextdown_mgd','ps_nextdown_mgd'),
  'runlabel' = c('Qbaseline_2020', 'QBaseline_2040', 'L90_2020', 'L90_2040', 'WD_2020', 'WD_2040', 'PS_2020', 'PS_2040', 'PSNX_2020', 'PSNX_2040')
)
wshed_data <- om_vahydro_metric_grid(metric, df)

wshed_data <- sqldf(
  "select a.*, b.da 
   from wshed_data as a 
  left outer join da_data as b 
  on (a.pid = b.pid)
  where hydrocode like 'vahydrosw_wshed_P%'
  and hydrocode not like 'vahydrosw_wshed_PL%'
  order by da
  ")

wshed_case <- sqldf(
  "select * from 
  wshed_data 
  where 
    (abs(1.0 - (QBaseline_2020/QBaseline_2040)) > 0.001)
    or riverseg = 'PM7_4200_4410' 
    or riverseg = 'PM7_4410_4620'
  ")

elid = 229119
pordat <- fn_get_runfile(elid, 201)
pordf <- as.data.frame(pordat)

# you can still run it very quickly with the old nomenclature from the foundation4 script if needed,
#  but you need to transform the runid array to a data frame, see below:
oldskool_data <- om_vahydro_metric_grid('l90_Qout', as.data.frame(c('runid_11', 'runid_13', 'runid_17')))

