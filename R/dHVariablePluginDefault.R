#' Base entity data object
#' @description Handler class for property entities (and timeseries if needed)
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param datasource optional RomDataSource for remote and local storage
#' @param config list of attributes to set
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHVariablePluginDefault
dHVariablePluginDefault <- R6Class(
  "dHVariablePluginDefault",
  public = list(
    #' @field name what is it called
    name = NA,
    #' @field datasource used to retrieve local and remote properties
    datasource = NA,
    #' @param datasource RESTful repository (optional)
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return object instance
    initialize = function(datasource = NULL, config = list()) {
      self$datasource = datasource
    },
    #' @param entity the local object to work on 
    #' @param load_remote automatically query REST data source for matches?
    #' @returns an updated config if necessary or FALSE if it fails
    exportOpenMI = function(entity) {
      # creates an array that can later be serialized as json, xml, or whatever
      # export = self$exportOpenMIBase(entity);
      # # load subComponents 
      # # @todo: figure this out so that other chains work better.
      # children <- self$datasource$propvalues()
      #   foreach (thischild in children) {
      #     if (exists(export[thischild$propname])) {
      #       # should we override this as the base class has already set a value ?
      #     }
      #     sub_entity = is_object(entity[[thisname]]) 
      #     ? $entity->{$thisname} 
      #     : om_load_dh_property($entity, $thisname, TRUE);
      #     $plugin = dh_variables_getPlugins($sub_entity);
      #     #dpm($plugin,'plugin');
      #     if (is_object($plugin) and method_exists($plugin, 'exportOpenMI')) {
      #       $sub_export = $plugin->exportOpenMI($sub_entity);
      #       $has_plug = TRUE;
      #     } else {
      #       $has_plug = FALSE;
      #       $sub_export = array(
      #         $sub_entity->propname => array(
      #           'host' => $_SERVER['HTTP_HOST'], 
      #           'id' => $sub_entity->pid, 
      #           'name' => $sub_entity->propname, 
      #           'value' => $sub_entity->propvalue, 
      #           'code' => $sub_entity->propcode, 
      #         )
      #       );
      #     }
      #     $export[$entity->propname][$thisname] = $sub_export[$sub_entity->propname];
      #   }
      #   return $export;
    },
    #' @param entity the local object to work on 
    #' @param load_remote automatically query REST data source for matches?
    #' @returns an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      
    }
  )
)


