# install_github("HARPGroup/hydro-tools", force=TRUE)
library("hydrotools")
ds <- RomDataSource$new("http://deq2.bse.vt.edu/d.alpha")
ds$get_token()
hid <- 72575

ts <- RomTS$new(
  ds, 
  list(
    varkey="geologic_map", 
    featureid=hid, 
    entity_type="dh_feature", 
    tsvalue=1.0, 
    tscode="test" 
  ), 
  TRUE
)

ts1 <- ds$get_ts(list(
  varkey="geologic_map", 
  featureid=hid, 
  entity_type="dh_feature", 
  tsvalue=1.0, 
  tscode="test" 
), 'object', TRUE)

# should work the same as 
ts2 <- getTimeseries(
  list(
    varkey="geologic_map", 
    featureid=hid, 
    entity_type="dh_feature", 
    tsvalue=1.0, 
    tscode="test" 
  ), 
  "http://deq2.bse.vt.edu/d.alpha",
  t
)

ts$save()
# Should work the same
#ss <- postTimeseries(ts$to_list(), site,tt)
