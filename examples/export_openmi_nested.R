
get_export <- function(ds, featureid, props, depth=0) {
  #message(paste("Handling pid=",featureid))
  #thisobject <- sqldf(paste("select * from props where pid =", featureid), method="raw")
  #thisobject = props[which(props$pid == featureid),]
  propatts <- as.list(props[which(props$pid == featureid),])
  thisobject = RomProperty$new(ds, propatts, FALSE )
  #print(paste("handling", thisobject$propname))
  plugin <- get_plugin(thisobject)
  export[[thisobject$propname]] = plugin$exportOpenMI(thisobject)
  #export[[thisobject$propname]] = list(propname=thisobject$propname, code=thisobject$propcode)
  children = props[which(props$featureid == featureid),]
  # note: this sqldf below (and above) is a version that uses sqldf to recursively trace the 
  #       property tree.  This is hugely inefficient, like 3,000% increase in execution time.
  #       This is kept for posterity and as an example of what not to do.
  #children = sqldf(paste("select * from props where featureid =", featureid), method="raw")
  if (nrow(children) > 0) {
    for (i in 1:nrow(children)) {
      thischild <- children[i,]
      sub_export <- get_export(ds, thischild$pid, props, depth)
      export[[thisobject$propname]][[thischild$propname]] <- sub_export[[thischild$propname]]
    }
  }
  return(export)
}


startTime <- Sys.time() 
export = list()
export <- get_export(dso, model$prop_list[1,]$pid, model$prop_list)
#export <- get_export_sql(dso, model$prop_list[1,]$pid, model$prop_list)
endTime <- Sys.time() 
# prints recorded time 
print(endTime - startTime)
r400 = find_name(export,"runid_400")
Smin_L90_mg = find_name(r400, "Smin_L90_mg")
riverseg = find_name(export, "riverseg")
fac_demand_mgy = find_name(export,"fac_demand_mgy")
adj_demand_mgd = find_name(export,"adj_demand_mgd")

# test with dso object
model_pid = 4824696
model <- dso$get_json_prop(model_pid)
model_tree <- RomPropertyTree$new(dso, list(root_pid=model_pid), TRUE)
base_demand_mgd = find_name(model,"base_demand_mgd")
adj_demand_mgd = find_name(model,"adj_demand_mgd")
send_to_parent = find_name(model,"Send to Parent")
kable(send_to_parent$broadcast_params)
jsonlite::toJSON(send_to_parent$broadcast_params)
stp_bcp = send_to_parent$broadcast_params$value
stp_bcp$weight <- NULL
names(stp_bcp) <- NULL

jsonlite::toJSON(as.matrix(send_to_parent$broadcast_params))
stp_tree <- RomPropertyTree$new(dso, list(root_pid=send_to_parent$id), TRUE)

# list all plugins
dso$var_defs$plugin
