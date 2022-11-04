# fn_get_prop - will retrieve properties using standard REST. TO BE MOVED TO utils.R in next version
#' @param config = list(entity_type, featureid, tid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tscode = NULL, tlid = NULL) timeline ID (not yet used)
#' @param site URL of om server
#' @param token show debugging info
#' @return nothing sets internal private token
fn_get_prop = function(config, site, token) {
  propvalues <- fn_get_rest('dh_properties', 'pid', config)
  return(propvalues)
}