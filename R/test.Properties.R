# install_github("HARPGroup/hydro-tools", force=TRUE)
library("hydrotools")
ds <- RomDataSource$new("http://deq2.bse.vt.edu/d.dh")
ds$get_token()
hid <- 74551 

map1 <- RomProperty$new(ds, list(propname="map1", entity_type="dh_feature",featureid=hid),TRUE)
map1 <- RomProperty$new(ds, list(propname="extent", entity_type="dh_properties",featureid=map1$pid),TRUE)
ds$get_prop(config = list(featureid = extent$pid, entity_type='dh_properties'))

# If using new version 
extent$propvalues()
extent$propvalues()[c('propname','propvalue')]


