# install_github("HARPGroup/hydro-tools", force=TRUE)
library("hydrotools")
ds <- RomDataSource$new("http://deq1.bse.vt.edu/d.dh", 'restws_admin')
ds$get_token()
hid <- 74551 

map1 <- RomProperty$new(ds, list(propname="map1", entity_type="dh_feature",featureid=hid),TRUE)
extent <- RomProperty$new(ds, list(propname="extent", entity_type="dh_properties",featureid=map1$pid),TRUE)

# Using old method (pre 5/27/2021)
pvals <- ds$get_prop(config = list(featureid = extent$pid, entity_type='dh_properties'))
pvals[c('propname','propvalue')]

# Or, If using new version 
extent$propvalues()[c('propname','propvalue')]
# Show all:
# extent$propvalues()
# show only props named x1
extent$propvalues('x1')[c('propname','propvalue')]


