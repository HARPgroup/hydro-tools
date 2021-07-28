# install_github("HARPGroup/hydro-tools", force=TRUE)
library("hydrotools")
ds <- RomDataSource$new("http://deq1.bse.vt.edu/d.dh", 'restws_admin')
ds$get_token()
hid <- 68123 

map1 <- RomProperty$new(
  ds, list(
    propcode="vahydro-1.0", 
    entity_type="dh_feature",
    featureid=hid
  ),
  TRUE
)
model <- ds$get_prop(
  config = list(
    featureid = 68123, 
    entity_type='dh_feature', 
    propcode = 'vahydro-1.0'
  ),
  return_type = 'object'
)

# use new json
wshed_obj_url <- paste(json_obj_url, model$pid, sep="/")
wshed_model_info <- om_auth_read(wshed_obj_url, token,  "text/json", "")

# All channel data
# use model parameter retrieval routine
# Facility analysis
df <- data.frame(
  'model_version' = c('vahydro-1.0',  'vahydro-1.0'),
  'runid' = c('local_channel','local_channel'),
  'metric' = c('drainage_area', 'province'),
  'runlabel' = c('drainage_area', 'province')
)
channel_data <- om_vahydro_metric_grid(
  metric, df, 'all', 'dh_feature', 'watershed','all',
  "vahydro-1.0","http://deq1.bse.vt.edu/d.dh/entity-model-prop-level-export"
)

# 1 = appalachian plateau
# 2 = Valley and Ridge
# 3 = Piedmont 
# 4 = Coastal Plain

coeffs <- rbind(
  # appalachian Plateau
  list(
    hc = 2.030,he = 0.2310,bfc = 12.175,bfe = 0.4711,bc = 5.389,be = 0.5349,n = 0.036
  ),
  # valley & Ridge
  list(
    hc = 1.43,he = 0.2830,bfc = 13.216,fe = 0.4532,bc = 4.667,be = 0.5489,n = 0.038
  ),
  # Piedmont
  list(
    hc = 2.137,he = 0.2561,bfc = 14.135,bfe = 0.4111,bc = 6.393,be = 0.4604,n = 0.095
  ),
  # Coastal Plain
  list(
    hc = 2.820, he = 0.2000, bfc = 15.791, bfe = 0.3758, bc = 6.440, be = 0.4442, n = 0.040
  )
)

channel_all = FALSE
for (i in 1:nrow(channel_data)) {
  channel <- channel_data[i,]
  prov <- channel$province
  cco <- coeffs[prov,]
  channel_atts <- list(
    name = channel_data$name,
    riverseg = channel$riverseg,
    province = prov,
    da = channel$drainage_area,
    h = cco$hc * channel$drainage_area ^ cco$he,
    bf = cco$bfc * channel$drainage_area  ^ cco$bfe,
    b = cco$bc *channel$drainage_area ^ cco$be
  )
  channel_atts$z = 0.5 * (channel_atts$bf - channel_atts$b) / channel_atts$h
  print(i)
  if (is.logical(channel_all)){
    channel_all <- rbind(channel_atts)
  } else {
    channel_all <- rbind(channel_all, channel_atts)
  }
}
channel_all <- as.data.frame(channel_all)
