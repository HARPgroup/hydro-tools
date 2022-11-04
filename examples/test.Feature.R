library("hydrotools")
library("httr")

ds <- RomDataSource$new("http://deq1.bse.vt.edu/d.dh", 'rest_uname')
ds$get_token()
# Load a feature (measuring point or facility or other)
feat <- RomFeature$new(ds,list(hydroid=1096),TRUE)

# Historic annual withdrawals
ts <- feat$tsvalues(varkey='wd_mgy')[c('tstime', 'tsvalue')]
tsz <- zoo::as.zoo(ts$tsvalue, order.by = as.POSIXct(ts$tstime, origin="1970-01-01"))
barplot(tsz)

# Historic monthly withdrawals
ts <- feat$tsvalues(varkey='wd_mgm')[c('tstime', 'tsvalue')]
tsz <- zoo::as.zoo(ts$tsvalue, order.by = as.POSIXct(ts$tstime, origin="1970-01-01"))
barplot(tsz)

# box plot of monthly variation in withdrawal
boxplot(ts$tsvalue ~ months(as.POSIXct(ts$tstime, origin="1970-01-01")))

# cross tab months - old school VWUDS format
ts$year <- format(strptime(ts$tstime,'%s'), '%Y')
ymtabs <- xtabs(
  tsvalue ~ year + months(tstime),
  data = ts)
ymtabs <- cbind(row.names(ymtabs), as.data.frame.matrix(ftable(ymtabs)))
names(ymtabs) <- c(c('Year'), month.abb)

# Bar Chart of Average Monthly Withdrawals
ts$month <- months(ts$tstime)
tsmon <- sqldf("select month, avg(tsvalue) as tsvalue from ts group by month")
barplot(
  tsmon$tsvalue ~ tsmon$month,
  ylab = paste(ds$get_vardef('wd_mgm')[c('varname','varunits')]),
  xlab = "Month"
)
