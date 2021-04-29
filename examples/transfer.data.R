# install_github("HARPGroup/hydro-tools", force=TRUE)
library("hydrotools")
ds1 <- RomDataSource$new("http://deq2.bse.vt.edu/d.alpha")
ds1$get_token()
hid <- 61565

tsinfo = list(
  featureid=hid, 
  entity_type="dh_feature"
)
# which returns the exact same object as
ts1 <- ds1$get_ts(tsinfo, 'data.frame')
# new school manual without datasource object
ts1_new_man <- fn_get_timeseries(tsinfo, "http://deq2.bse.vt.edu/d.dh", token)
# new school search manually FROM the datasource already retrieved
ts1_new_search <- fn_search_tsvalues(config, ds$tsvalues)

# old school
ts1_old <- getTimeseries(tsinfo, "http://deq2.bse.vt.edu/d.alpha")

## Now set up the destination 
ds2 <- RomDataSource$new("http://deq2.bse.vt.edu/d.dh")
ds2$get_token()

hid2 <- 473590

for (tsrow in ts1) {
  # tbd: note, we should find the varkey for the original data source
  #      and then set varkey instead of varid when we create this
  #      so that it will query the ds2 to get the proper varid in case
  #      they are different.  For this case, since d.alpha is a clone 
  #      of d.dh, all the varids should be the same.
  tsl <- as.list(tsrow)
  tsl$tid = NULL
  tsl$featureid = hid2
  t2 <- RomTS$new(ds2, tsl)
  t2$save()
}
# Putting it back
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