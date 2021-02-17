library('hydrotools')

df <- data.frame(
  'model_version' = c('vahydro-1.0',  'vahydro-1.0',  'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0', 'vahydro-1.0'),
  'runid' = c('runid_11', 'runid_13', 'runid_11', 'runid_13', 'runid_11', 'runid_13', 'runid_11', 'runid_13', 'runid_11', 'runid_13'),
  'runlabel' = c('Qbaseline_2020', 'QBaseline_2040', 'L90_2020', 'L90_2040', 'WD_2020', 'WD_2040', 'PS_2020', 'PS_2040', 'PSNX_2020', 'PSNX_2040'),
  'metric' = c('Qbaseline', 'Qbaseline','l90_Qout','l90_Qout','wd_cumulative_mgd','wd_cumulative_mgd','ps_cumulative_mgd','ps_cumulative_mgd','ps_nextdown_mgd','ps_nextdown_mgd')
)
wshed_data <- om_vahydro_metric_grid(metric, df)
