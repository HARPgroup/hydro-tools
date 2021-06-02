library("hydrotools")
ds <- RomDataSource$new("https://deq1.bse.vt.edu/d.dh", 'restws_admin')
ds$get_token()

feat <- RomFeature$new(ds,list(hydroid=62173),TRUE)
tsv <- feat$tsvalues(varkey='wd_mgy')[c('tstime', 'tsvalue')]
tsv <- as.zoo(tsv$tsvalue, order.by = as.POSIXct(tsv$tstime, origin="1970-01-01"))
barplot(tsv)