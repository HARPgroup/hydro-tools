library("hydrotools")
library("sqldf")

ds <- RomDataSource$new(NULL)
ds$tsvalues
ds$props
ds$features
ts <- RomTS$new(ds)
ts$tid = 5
ts$tsvalue = 1.4
ts$save()
#ts$to_list()
ds$tsvalues

ts2 <- RomTS$new(ds)
ts2$tid = 10
ts2$tsvalue = 1.9
ts2$save()
ds$tsvalues

tsvalues <- ds$tsvalues
sqldf("select * from tsvalues where tid = 5")
fn_search_tsvalues(list(tid=5), ds$tsvalues)


# Now try with REST

site = "http://deq2.bse.vt.edu/d.dh"
ds <- RomDataSource$new(site)
ds$get_token()
ts <- RomTS$new(ds)
ts$tid = NA
ts$tsvalue = 1.4
ts$save()

#ts2 <- RomTS$new(ds, list(tid = 112953, varid = 46, featureid = 523, entity_type = 'dh_feature', limit = 1), TRUE)
ts2 <- RomTS$new(ds, list(tid = 112953), TRUE)

ds$tsvalues

