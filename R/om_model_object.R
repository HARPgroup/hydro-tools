# 
#' om_model_object Function: Retrieves a model object, creates if needed
#' @description Retrieves a model object, creates if needed
#' @param ds RomDataSource
#' @param feature Feature object RomFeature
#' @param model_version Model version
#' @param model_name Model name, defaults to feature name + model_Version
#' @param model_varkey Variable key to use for model if one needs to be created, default=om_model_element
#' @return model object as RomProperty
#' @export om_model_object
om_model_object <- function(
    ds, feature, model_version, model_name = FALSE,
    model_varkey='om_model_element'
) {
  model <- RomProperty$new(
    ds,
    list(
      featureid=feature$hydroid, 
      entity_type="dh_feature", 
      propcode=model_version
    ), 
    TRUE
  )
  
  if (is.na(model$pid)) {
    if (is.logical(model_name)) {
      model_name <- paste(feature$name, model_version)
    }
    model$propname = model_name
    model$varid = ds$get_vardef(model_varkey)$hydroid
    message(paste("Creating new feature model", model$propname, model$varid, model$featureid, model$propcode))
    model$save(TRUE)
  }
  
  return(model)
}