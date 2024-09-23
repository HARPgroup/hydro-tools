# 
#' om_get_model_scenario Function: Retrieves a scenario for saving model data, creates scenario if needed
#' @description Retrieves a model scenario
#' @param model object as RomProperty
#' @param scenario_name Name of scenario to create/retrieve 
#' @param model_varkey Variable key to use for model if one needs to be created, default=om_model_element
#' @return scenario object as RomProperty
#' @export om_get_model_scenario
om_get_model_scenario <- function(
    model, 
    scenario_name, model_varkey='om_model_element'
  ) {
  
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
  if (is.na(model_scenario$pid)) {
    model_scenario$save(TRUE)
  }
  return(model_scenario)
}