# install_github("HARPGroup/hydro-tools", force=TRUE)
library("hydrotools")
ds <- RomDataSource$new(site, 'restws_admin')
ds$get_token()

hid <- 68123 
hydrocode = 'vahydrosw_wshed_JL1_6560_6440_beaver_creek'
feature <- RomFeature$new(ds,list(hydroid=hid),TRUE)
feature <- RomFeature$new(ds,list(hydrocode=hydrocode, bundle="watershed",ftype="vahydro"),TRUE)

model <- RomProperty$new(ds,list(featureid=hid, propcode="vahydro-1.0"),TRUE)


# use new json
model_obj_url <- paste(json_obj_url, model$pid, sep="/")
model_info <- om_auth_read(model_obj_url, token,  "text/json", "")
model <- fromJSON(model_info)

