library("hydrotools")
ds <- RomDataSource$new("http://deq1.bse.vt.edu/d.alpha", 'restws_admin')
ds$get_token()

feat <- RomFeature$new(ds,list(hydroid=1096),TRUE)
ts <- feat$tsvalues(varkey='wd_mgy')[c('tstime', 'tsvalue')]
tsz <- zoo::as.zoo(tsv$tsvalue, order.by = as.POSIXct(tsv$tstime, origin="1970-01-01"))
barplot(tsz)
