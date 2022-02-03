library("hydrotools")
library("httr")

ds <- RomDataSource$new("http://deq1.bse.vt.edu/d.dh", rest_uname)
ds$get_token(rest_pw)
# Load a feature (measuring point or facility or other)
# 58868
# 1096
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
  tsvalue ~ year + months(as.POSIXct(ts$tstime, origin="1970-01-01")),
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

list_of_cs <- ds$get( "dh_adminreg_contact", "contactid", list(bundle = 'dh_adminreg_contact'))

# load a list of drought region features
list_of_fs <- ds$get( "dh_feature", "hydroid",
  list(bundle = "landunit", ftype="drought_region")
)

# load a list of County/City features
list_of_fips <- ds$get(  "dh_feature", "hydroid",
  list(bundle = "usafips", fstatus="active")
)

# load a list of drought statuses
list_of_ds <- ds$get( "dh_properties", "pid",
  list(entity_type = "dh_feature", propname="drought_status")
)

region_status <- sqldf(
  "select a.hydroid, a.name, b.propcode as drought_status
    from list_of_fs as a 
    left outer join list_of_ds as b
    on (a.hydroid = b.featureid)
  "
)

# load a list of features
fips_status <- sqldf(
  "select a.hydroid, a.name, b.propcode as drought_status
    from list_of_fips as a 
    left outer join list_of_ds as b
    on (a.hydroid = b.featureid)
    where b.propcode >= 0
  "
)

all_status = sqldf(
  "select * from region_status
  UNION
  select * from fips_status"
)