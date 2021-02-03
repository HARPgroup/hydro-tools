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

# df <- data.frame(
#   'model_version' = c('vahydro-1.0',  'vahydro-1.0',  'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
#   'runid' = c('runid_11', 'runid_13', 'runid_11', 'runid_13', 'runid_11', 'runid_13', 'runid_11', 'runid_13', 'runid_11', 'runid_13'),
#   'metric' = c('Qbaseline', 'Qbaseline','l90_Qout','l90_Qout','wd_cumulative_mgd','wd_cumulative_mgd','ps_cumulative_mgd','ps_cumulative_mgd','ps_nextdown_mgd','ps_nextdown_mgd'),
#   'runlabel' = c('Qbaseline_2020', 'QBaseline_2040', 'L90_2020', 'L90_2040', 'WD_2020', 'WD_2040', 'PS_2020', 'PS_2040', 'PSNX_2020', 'PSNX_2040')
# )

df <- data.frame(
  'model_version' = c('vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'usgs-1.0', 'vahydro-1.0', 'usgs-1.0'),
  'runid' = c('runid_11', 'runid_11', 'runid_11', 'runid_11', 'runid_11', 'runid_11'),
  'metric' = c('wd_cumulative_mgd', 'ps_cumulative_mgd', 'l90_Qout','l90_Qout','l90_year','l90_year'),
  'runlabel' = c('wd2020', 'ps2020', 'L90_2020', 'L90_usgs', 'L90_year_2020', 'L90_year_usgs')
)

wshed_data <- om_vahydro_metric_grid(metric, df)

wshed_data <- sqldf(
  "select a.*, b.da 
   from wshed_data as a 
  left outer join da_data as b 
  on (a.pid = b.pid)
  order by da
  ")


wshed_case <- sqldf(
  "select * from 
  wshed_data 
  where 
    riverseg like 'P%' 
  ")

elid = 229119
pordat <- fn_get_runfile(elid, 201)
pordf <- as.data.frame(pordat)

# you can still run it very quickly with the old nomenclature from the foundation4 script if needed,
#  but you need to transform the runid array to a data frame, see below:
#oldskool_data <- om_vahydro_metric_grid('l90_Qout', as.data.frame(c('runid_11', 'runid_13', 'runid_17')))

minor_basin_list <- c('PS', 'NR', 'YP', 'TU', 'RL', 'OR', 'EL', 'ES', 'PU', 'RU', 'YM', 'JA', 'MN', 'PM', 'YL', 'BS', 'PL', 'OD', 'JU', 'JB', 'JL')
minor_basin_list = c('YP', 'YM')
table_name = '11_13_17'
# Save the metric specific file
for (minor_basin in minor_basin_list) {
  mbdata <- sqldf(
    paste0(
      "select * from wshed_data where hydrocode like 'vahydrosw_wshed_",
      minor_basin,
      "%'
      AND hydrocode not like '%0000'
      ORDER BY da"
    )
  )
  filename <- paste0(folder,"cia_", minor_basin, '_', table_name, ".csv")
  write.csv(mbdata,filename)
}

df <- data.frame(
  'model_version' = c('vahydro-1.0',  'vahydro-1.0',  'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c('runid_11', 'runid_17', 'runid_11', 'runid_13', 'runid_17', 'runid_11', 'runid_11', 'runid_13', 'runid_13'),
  'metric' = c('l90_year', 'l90_cc_year','l90_Qout','l90_Qout','l90_Qout','wd_cumulative_mgd','ps_cumulative_mgd','wd_cumulative_mgd','ps_cumulative_mgd'),
  'runlabel' = c('l90_year', 'l90_cc_year', 'L90_2020', 'L90_2040', 'L90_CCdry', 'Total_WD_2020', 'Total_PS_2020', 'Total_WD_2040', 'Total_PS_2040')
)
wshed_data <- om_vahydro_metric_grid(metric, df)

wshed_data <- sqldf(
  "select a.*, b.da 
   from wshed_data as a 
  left outer join da_data as b 
  on (a.pid = b.pid)
  WHERE a.hydrocode not like '%0000'
  order by da
  ")

minor_basin_list = c('YP', 'YM')
table_name = 'cc_year_compare'
# Save the metric specific file
for (minor_basin in minor_basin_list) {
  mbdata <- sqldf(
    paste0(
      "select * from wshed_data where hydrocode like 'vahydrosw_wshed_",
      minor_basin,
      "%'
      AND hydrocode not like '%0000'
      ORDER BY da"
    )
  )
  filename <- paste0(folder,"cia_", minor_basin, '_', table_name, ".csv")
  write.csv(mbdata,filename)
}