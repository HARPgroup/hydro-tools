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
    object_class = FALSE,
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return object instance
    initialize = function(config = list()) {
      #message("Created plugin")
    },
    #' @param entity the local object to work on 
    #' @param load_remote automatically query REST data source for matches?
    #' @returns an updated config if necessary or FALSE if it fails
    exportOpenMI = function(entity) {
      # creates an array that can later be serialized as json, xml, or whatever
      export = self$exportOpenMIBase(entity);
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
      return(export)
    },
    #' @param entity the local object to work on 
    #' @param load_remote automatically query REST data source for matches?
    #' @returns an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      export = list(
        id=entity$pid,
        name=entity$propname,
        value=entity$propvalue
      )
      return(export)
    }
  )
)


#' Equation meta-model object
#' @description Handler class for property entities (and timeseries if needed)
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMEquation
dHOMEquation <- R6Class(
  "dHOMEquation",
  inherit = dHVariablePluginDefault,
  public = list(
    #' @field name what is it called
    name = NA,
    object_class = 'Equation',
    
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return object instance
    initialize = function(config = list()) {
      #message("Created plugin")
    },
    #' @param entity the local object to work on 
    #' @param load_remote automatically query REST data source for matches?
    #' @returns an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      export = list(
        id=entity$pid,
        name=entity$propname,
        value=entity$propcode
      )
      return(export)
    }
  )
)


#' Numeric Constant meta-model object
#' @description Simple class to hold numeric values
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMEquation
dHOMConstant <- R6Class(
  "dHOMConstant",
  inherit = dHVariablePluginDefault,
  public = list(
    #' @field name what is it called
    name = NA,
    object_class = FALSE,
    
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return object instance
    initialize = function(config = list()) {
      #message("Created plugin")
    },
    #' @param entity the local object to work on 
    #' @param load_remote automatically query REST data source for matches?
    #' @returns an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      export = list(
        id=entity$pid,
        name=entity$propname,
        value=entity$propvalue
      )
      return(export)
    }
  )
)

#' Text Constant meta-model object
#' @description Simple class to hold string values
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMAlphanumericConstant
dHOMAlphanumericConstant <- R6Class(
  "dHOMAlphanumericConstant",
  inherit = dHVariablePluginDefault,
  public = list(
    #' @field name what is it called
    name = NA,
    object_class = FALSE,
    
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return object instance
    initialize = function(config = list()) {
      #message("Created plugin")
    },
    #' @param entity the local object to work on 
    #' @param load_remote automatically query REST data source for matches?
    #' @returns an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      export = list(
        id=entity$pid,
        name=entity$propname,
        value=entity$propcode
      )
      return(export)
    }
  )
)

#' Object class of meta-model object
#' @description Simple class to hold text values
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMObjectClass
dHOMObjectClass <- R6Class(
  "dHOMObjectClass",
  inherit = dHOMAlphanumericConstant,
  public = list(
  )
)

#' Matrix meta-model object
#' @description Simple class to hold tabular values
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMDataMatrix
dHOMDataMatrix <- R6Class(
  "dHOMDataMatrix",
  inherit = dHVariablePluginDefault,
  public = list(
    #' @field name what is it called
    name = NA,
    object_class = 'dataMatrix',
    #' @param entity the local object to work on 
    #' @param load_remote automatically query REST data source for matches?
    #' @returns an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      #print(paste("Entity matrix:", entity$propname))
      export = list(
        id=entity$pid,
        name=entity$propname,
        value=entity$data_matrix
      )
      return(export)
    }
  )
)

# 'This is heare because there is no way to instantiate a dynamic class using 
# 'a string for a class name, so we have to have logic to expose allowed classes
#' Retrieve Plugin object for a variable entity
#'
#' @param plugin_name the actual class name
#' @param entity the object to apply the plugin to
#' @return class matching plugin or default generic plugin
#' @seealso NA
#' @export fn_search_vardefs
#' @examples NA
get_plugin_class <- function(plugin_name, entity) {
  if (is.na(plugin_name) ) {
    plugin = dHVariablePluginDefault$new(entity)
  } else if (plugin_name == "dHOMEquation") {
    plugin = dHOMEquation$new(entity)
  } else if (plugin_name == "dHOMAlphanumericConstant") {
    plugin = dHOMAlphanumericConstant$new(entity)
  } else if (plugin_name == "dHOMObjectClass") {
    plugin = dHOMObjectClass$new(entity)
  } else if (plugin_name == "dHOMDataMatrix") {
    plugin = dHOMDataMatrix$new(entity)
  } else {
    plugin = dHVariablePluginDefault$new(entity)
  }
  return(plugin)
}
