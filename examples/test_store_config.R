# install.packages('https://github.com/HARPgroup/openmi-om/raw/master/R/openmi.om_0.0.0.9105.tar.gz', repos = NULL, type="source")

library("rjson")
library("hydrotools")

# Create datasource
ds <- RomDataSource$new("https://deq1.bse.vt.edu/d.dh", 'restws_admin')
ds$get_token()
scen_var <- ds$get_vardef('om_scenario')
scen_config <- ds$get_prop(
  list(
    featureid = scen_var$varid, 
    entity_type = 'dh_variabledefinition', 
    propname = 'variants'
  )
)
src_json_node <- paste('https://deq1.bse.vt.edu/d.dh/node/62', scen_config$pid, sep="/")
load_txt <- ds$auth_read(src_json_node, "text/json", "")
load_objects <- fromJSON(load_txt)
load_objects$reports$cia
# Show the infofor runid 600
load_objects$variants$runid_600


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

find_name(load_objects, 'reports')
find_name(load_objects, 'cia')
info_txt <- find_name(load_objects, 'runid_600')

