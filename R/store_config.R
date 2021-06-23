
run_text <- function (runid = "runid_600",site = "https://deq1.bse.vt.edu/d.dh",token = NULL) {

library("rjson")
library("hydrotools")

# Create datasource
#private = list(token = token)
ds <- RomDataSource$new(site)
RomDataSource$private$token <- token
scen_var <- ds$get_vardef('om_scenario')
scen_config <- ds$get_prop(list(featureid = scen_var$varid,entity_type = 'dh_variabledefinition',propname = 'variants'))
src_json_node <- paste(site,'/node/62', scen_config$pid, sep="/")
load_txt <- ds$auth_read(src_json_node, "text/json", "")
load_objects <- fromJSON(load_txt)
#load_objects$reports$cia

# Show the infofor runid 600
run_text <- load_objects$variants[runid]

return(run_text)
}


find_name <- function(haystack, needle) {
  # this fn came from https://stackoverflow.com/questions/58400176/r-find-object-by-name-in-deeply-nested-list
  if (hasName(haystack, needle)) {
    haystack[[needle]]
  } else if (is.list(haystack)) {
    for (obj in haystack) {
      ret <- Recall(obj, needle)
      if (!is.null(ret)) return(ret)
    }
  } else {
    NULL
  }
}




