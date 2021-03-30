library("hydrotools")
ds <- RomDataSource$new("http://deq2.bse.vt.edu/d.alpha/")
hid <- 72575

ts <- RomTS$new(
  ds, 
  list(
    varkey="geologic_map", 
    featureid=hid, 
    entity_type="dh_feature", 
    tscode="mount alto quad"
  )
)

inputs <- list(
  varid <- ds$get_vardef(),
  featureid = hid,
  tsvalue
)