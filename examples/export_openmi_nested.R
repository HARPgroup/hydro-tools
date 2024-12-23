get_plugin <- function(ds, thisobject) {
  vardef = ds$get_vardef(as.integer(thisobject$varid))
  plugin = get_plugin_class(vardef$plugin, thisobject)
  return(plugin)
}

get_export <- function(ds, featureid, props, depth=0) {
  #message(paste("Handling pid=",featureid))
  #thisobject <- sqldf(paste("select * from props where pid =", featureid), method="raw")
  thisobject = props[which(props$pid == featureid),]
  plugin <- get_plugin(ds,thisobject)
  export[[thisobject$propname]] = plugin$exportOpenMI(thisobject)
  #export[[thisobject$propname]] = list(propname=thisobject$propname, code=thisobject$propcode)
  children = props[which(props$featureid == featureid),]
  if (nrow(children) > 0) {
    for (i in 1:nrow(children)) {
      thischild <- children[i,]
      sub_export <- get_export(ds, thischild$pid, props, depth)
      export[[thisobject$propname]][[thischild$propname]] <- sub_export[[thischild$propname]]
    }
  }
  return(export)
}

get_export_sql <- function(ds, featureid, props, depth=0) {
  #message(paste("Handling pid=",featureid))
  thisobject <- sqldf(paste("select * from props where pid =", featureid), method="raw")
  #plugin <- get_plugin(ds,thisobject)
  export = list()
  export[[thisobject$propname]] = list(propname=thisobject$propname, code=thisobject$propcode)
  children = sqldf(paste("select * from props where featureid =", featureid), method="raw")
  #children = props[which(props$featureid == featureid),]
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
wd_mgd = find_name(r400, "Smin_L90_mg")
