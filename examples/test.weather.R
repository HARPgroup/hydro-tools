# REST and dh_timeseries_weather
# Not yet working?
dhw <- hydrotools::fn_get_rest(
  'dh_timeseries_weather', 
  'tid', 
  list(featureid = 378700, tsendtime = 1628568000, entity_type = 'dh_'), 
  site = site, 
  token
)

