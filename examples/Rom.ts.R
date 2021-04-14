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
  ) 
)
# Should work the same
#ss <- postTimeseries(ts$to_list(), site,tt)
ts$save()

