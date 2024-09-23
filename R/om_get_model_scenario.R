# 
#' fn_plot_impoundment_flux Function: Plot water fluxes and balance info for an impoundment
#' @description Generates a plot of Qin, Qout, percent storage remaining and demand
#' @param feature_code Feature code (hydrocode)
#' @param feature_bundle Feature bundle
#' @param feature_ftype Feature ftype
#' @param model_version Model version
#' @param scenario_name Name of scenario to create/retrieve 
#' @param model_varkey Variable key to use for model if one needs to be created, default=om_model_element
#' @return scenario object as RomProperty
#' @export om_get_model_scenario
om_get_model_scenario <- function(
    feature_code, feature_bundle, feature_ftype, model_version, 
    scenario_name, model_varkey='om_model_element'
  ) {
  feature<- RomFeature$new(
    ds,
    list(
      hydrocode=feature_code, 
      ftype=feature_ftype,
      bundle=feature_bundle
    ), 
    TRUE
  )
  
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
    model$propname = feature$name
    model$varid = ds$get_vardef(model_varkey)$varid
    message(paste("Creating new feature model", model$propname, model$varid, model$featureid, model$propcode))
    model$save(TRUE)
  }
  
  #Re-ordered scenario to be within the model element and the land use within the scenario
  model_scenario <- RomProperty$new( 
    ds,
    list(
      varkey="om_scenario", 
      featureid=model$pid, 
      entity_type="dh_properties", 
      propname=scenario_name, 
      propcode=scenario_name 
    ), 
    TRUE
  )
  if (is.na(model$pid)) {
    model_scenario$save(TRUE)
  }
  return(model_scenario)
}