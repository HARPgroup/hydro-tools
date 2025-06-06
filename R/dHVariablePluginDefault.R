#' Base entity data object
#' @title dHVariablePluginDefault
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
    #' @field object_class model object type
    object_class = FALSE,
    #' @field entity_bundle model object type
    entity_bundle = FALSE,
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return object instance
    initialize = function(config = list()) {
      #message("Created plugin")
    },
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
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
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      export = list(
        id=entity$pid,
        name=entity$propname,
        object_class=self$object_class, 
        plugin=class(self)[1],
        value=entity$propvalue,
        code=entity$propcode
      )
      return(export)
    },
    #' @param entity the local object to work on 
    #' @param export list of properties and subproperties to export
    exportOpenMIpost = function(entity, export){
      #Allows modification of exported property lists prior to JSON conversion
      return(export)
    },
    #' @param om_list the open MI export array/list to work on
    #' @return a Rom importable config or FALSE if it fails
    fromOpenMIBase = function(om_list) {
      rom_list = list(
        pid=om_list$id,
        propname=om_list$name,
        propvalue=om_list$value,
        propcode=om_list$code
      )
      return(rom_list)
    },
    #' @return detailed information about this object in markdown list format
    param_info = function() {
      info = "Information about object and it's parameters
      - propname: name of the object
      - propvalue: numeric value "
      return(info)
    }
  )
)


#' Equation meta-model object
#' @title dHOMEquation
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
    #' @field object_class model object type
    object_class = 'Equation',
    
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return object instance
    initialize = function(config = list()) {
      #message("Created plugin")
    },
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      export = super$exportOpenMIBase(entity)
      export$value = entity$propcode
      export$equation = list(
        name='equation',
        value=entity$propcode
      )
      return(export)
    }
  )
)


#' Numeric Constant meta-model object
#' @title dHOMConstant
#' @description Simple class to hold numeric values
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMConstant
dHOMConstant <- R6Class(
  "dHOMConstant",
  inherit = dHVariablePluginDefault,
  public = list(
    #' @field name what is it called
    name = NA,
    #' @field object_class model object type
    object_class = FALSE,
    
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return object instance
    initialize = function(config = list()) {
      #message("Created plugin")
    },
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      export = list(
        id=entity$pid,
        name=entity$propname,
        value=entity$propvalue
      )
      if (is.na(export$value)) {
        # NA is not always handled outside R
        export$value = 0.0
      }
      return(export)
    }
  )
)

#' Text Constant meta-model object
#' @title dHOMAlphanumericConstant
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
    #' @field object_class model object type
    object_class = FALSE,
    
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return object instance
    initialize = function(config = list()) {
      #message("Created plugin")
    },
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      # NOTE: this one does not use the parent, as it is handled oddly by OM if we do
      export = list(
        id=entity$pid,
        name=entity$propname,
        value=entity$propcode
      )
      return(export)
    }
  )
)


#' @title dHOMPublicVars
#' @description Text that can be any of public vars list
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMAlphanumericConstant
dHOMPublicVars <- R6Class(
  "dHVarImage",
  inherit = dHOMAlphanumericConstant
)

#' Image URL pointer
#' @title dHVarImage
#' @description Simple class to hold string values
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHVarImage
dHVarImage <- R6Class(
  "dHVarImage",
  inherit = dHVariablePluginDefault,
  public = list(
    #' @field name what is it called
    name = NA,
    #' @field object_class model object type
    object_class = "dHVarImage",
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return object instance
    initialize = function(config = list()) {
      #message("Created plugin")
    },
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      # note: we export 'code' attribute but should deprecate in favor of 'value'
      export = super$exportOpenMIBase(entity)
      export$code = entity$propcode
      export$value = entity$propcode
      return(export)
    }
  )
)
#' Object class of meta-model object
#' @title dHOMObjectClass
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
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      if ('propcode' %in% names(entity)) {
        export = entity$propcode
      }
      return(export)
    }
  )
)

#' Matrix meta-model object
#' @title dHOMDataMatrix
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
    #' @field object_class model object type
    object_class = 'dataMatrix',
    #' @field entity_bundle model object type
    entity_bundle = 'om_data_matrix',
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      export = super$exportOpenMIBase(entity)
      export$matrix=list(
        name="matrix",
        object_class="array",
        value=I(entity$data_matrix)
      )
      return(export)
    },
    #' @return info regarding the needs and capabilities of this object
    param_info = function() {
      info = "- eval_type = 'auto'; // auto, numeric, string, reference 
      - valuetype = 1; // 0 - returns entire array (normal), 1 - single column lookup (col), 2 - 2 column lookup (col & row)
      - value_dbcolumntype = ''; // can be a db type, or an equation, which resolves to numeric in db storage
      - keycol1 = ''; // key for 1st lookup variable
      - lutype1 = 0; // lookup type for first lookup variable: 0 - exact match; 1 - interpolate values; 2 - stair step
      - keycol2 = ''; // key for 2nd lookup variable
      - lutype2 = 0; // lookup type for second lookup variable"
      return(info)
    }
  )
)

#' Annotation meta-model object
#' @title dHVarAnnotation
#' @description Simple class to hold tabular values
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHVarAnnotation
dHVarAnnotation <- R6Class(
  "dHVarAnnotation",
  inherit = dHVariablePluginDefault,
  public = list(
    #' @field name what is it called
    name = NA,
    #' @field object_class model object type
    object_class = 'textField',
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      export = super$exportOpenMIBase(entity)
      export$value = entity$proptext
      return(export)
    }
  )
)

#' dHOMtextField meta-model object
#' @title dHOMtextField
#' @description Simple class to hold tabular values
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMtextField
dHOMtextField <- R6Class(
  "dHOMtextField",
  inherit = dHVarAnnotation,
  public = list(
    #' @field name what is it called
    name = NA,
    #' @field object_class model object type
    object_class = 'textField',
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      export = super$exportOpenMIBase(entity)
      export$value = entity$propcode
      return(export)
    }
  )
)


#' dHOMConsumptiveUseFractionsPWS meta-model object
#' @title dHOMConsumptiveUseFractionsPWS
#' @description Simple class to hold tabular values
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMConsumptiveUseFractionsPWS
dHOMConsumptiveUseFractionsPWS <- R6Class(
  "dHOMConsumptiveUseFractionsPWS",
  inherit = dHOMDataMatrix,
  public = list(
    #' @field name what is it called
    name = NA,
    #' @field object_class model object type
    object_class = 'textField',
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      export = super$exportOpenMIBase(entity)
      export$value = entity$propcode
      return(export)
    }
  )
)

#' Broadcast meta-model object
#' @title dHOMbroadCastObject
#' @description Simple class to hold tabular values
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMbroadCastObject
dHOMbroadCastObject <- R6Class(
  "dHOMbroadCastObject",
  inherit = dHVariablePluginDefault,
  public = list(
    #' @field name what is it called
    name = NA,
    #' @field object_class model object type
    object_class = 'broadCastObject',
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      #print(paste("Entity matrix:", entity$propname))
      export = super$exportOpenMIBase(entity)
      export$value=NULL
      export$broadcast_params = list(
        name='broadcast_params',
        object_class='array',
        value=I(entity$data_matrix)
      )
      return(export)
    }
  )
)


#' Tiered flowby meta-model object
#' @title dHOMWaterSystemTieredFlowBy
#' @description Simple class to hold tabular flow by values
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMWaterSystemTieredFlowBy
dHOMWaterSystemTieredFlowBy <- R6Class(
  "dHOMWaterSystemTieredFlowBy",
  inherit = dHOMDataMatrix,
  public = list(
    #' @field name what is it called
    name = NA,
    #' @field object_class model object type
    object_class = 'wsp_1tierflowby',
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      #print(paste("Entity matrix:", entity$propname))
      export = super$exportOpenMIBase(entity)
      
      return(export)
    },
    list2matrix = function(l) {
      matrix_vals = data.frame(matrix(unlist(l), nrow = length(l), byrow = TRUE)) |>
        setNames(names(l[[1]]))
      return(matrix_vals)
    },
    translateOMtoDH = function(entity, om_json) {
      matrix_vals = self$list2matrix(om_json$processors$flowby$rule_matrix$matrix_rowcol)
      entity$set_matrix(matrix_vals)
    }
  )
)


#' VA Hydro model impoundment plugin
#' @title dHOMHydroImpoundment
#' @description Simple class to deal with hydro impoundment class model objects
#' @details Has standard methods for exporting impoundment properties and subproperties
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMHydroImpoundment
dHOMHydroImpoundment <- R6Class(
  "dHOMHydroImpoundment",
  inherit = dHOMDataMatrix,
  public = list(
    #' @field name what is it called
    name = NA,
    #' @field object_class model object type
    object_class = 'hydroImpoundment',
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      #print(paste("Entity matrix:", entity$propname))
      export = super$exportOpenMIBase(entity)
      
      return(export)
    }
  )
)

#' hydroimp
#' @title dHOMHydroImpoundmentSmall
#' @description Simple class to hold tabular flow by values
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param entity list or object with entity info
#' @return reference class of type openmi.om.base.
#' @seealso NA
#' @examples NA
#' @export dHOMHydroImpoundmentSmall
dHOMHydroImpoundmentSmall <- R6Class(
  "dHOMHydroImpoundmentSmall",
  inherit = dHOMHydroImpoundment,
  public = list(
    #' @field name what is it called
    name = NA,
    #' @field object_class model object type
    object_class = 'hydroImpSmall',
    #' @param entity the local object to work on 
    #' @return an updated config if necessary or FALSE if it fails
    exportOpenMIBase = function(entity) {
      #print(paste("Entity matrix:", entity$propname))
      export = super$exportOpenMIBase(entity)
      
      return(export)
    },
    #' @param entity the local object to work on 
    #' @param export current export object
    #' @return Returns a modified list of property and children
    exportOpenMIpost = function(entity, export = list()) {
      #Copy the storage_stage_area matrix and properties over to a new list
      #matrix that is expected in the php import to OM
      message("Copying impoundment storage_stage_area to matrix")
      export[['matrix']] <- export[['storage_stage_area']][['matrix']]
      #Remove the original list
      export[['storage_stage_area']] <- NULL
      return(export)
    }
  )
)



# 'This is here because there is no way to instantiate a dynamic class using 
# 'a string for a class name, so we have to have logic to expose allowed classes
#' Retrieve Plugin object for a variable entity
#'
#' @title get_plugin_class
#' @param plugin_name the actual class name
#' @param entity the object to apply the plugin to
#' @return class matching plugin or default generic plugin
#' @seealso NA
#' @export get_plugin_class
#' @examples NA
get_plugin_class <- function(plugin_name, entity) {
  if (is.na(plugin_name) ) {
    plugin = dHVariablePluginDefault$new(entity)
  } else if (plugin_name == "dHOMConstant") {
    plugin = dHOMConstant$new(entity)
  } else if (plugin_name == "dHOMHydroImpoundmentSmall") {
    plugin = dHOMHydroImpoundmentSmall$new(entity)
  } else if (plugin_name == "dHOMHydroImpoundment") {
    plugin = dHOMHydroImpoundment$new(entity)
  } else if (plugin_name == "dHOMEquation") {
    plugin = dHOMEquation$new(entity)
  } else if (plugin_name == "dHOMAlphanumericConstant") {
    plugin = dHOMAlphanumericConstant$new(entity)
  } else if (plugin_name == "dHOMPublicVars") {
    plugin = dHOMPublicVars$new(entity)
  } else if (plugin_name == "dHOMObjectClass") {
    plugin = dHOMObjectClass$new(entity)
  } else if (plugin_name == "dHOMDataMatrix") {
    plugin = dHOMDataMatrix$new(entity)
  } else if (plugin_name == "dHVarImage") {
    plugin = dHVarImage$new(entity)
  } else if (plugin_name == "dHVarAnnotation") {
    plugin = dHVarAnnotation$new(entity)
  } else if (plugin_name == "dHOMtextField") {
    plugin = dHOMtextField$new(entity)
  } else if (plugin_name == "dHOMWaterSystemPermitStatus") {
    # note dHOMWaterSystemPermitStatus is currently handled by dHOMAlphanumericConstant
    plugin = dHOMAlphanumericConstant$new(entity)
  } else if (plugin_name == "dHOMbroadCastObject") {
    plugin = dHOMbroadCastObject$new(entity)
  } else if (plugin_name == "dHOMWaterSystemTieredFlowBy") {
    plugin = dHOMWaterSystemTieredFlowBy$new(entity)
  } else {
    plugin = dHVariablePluginDefault$new(entity)
  }
  return(plugin)
}

# Is this useful?
# give an element and it loads it from the plugin?
# can't we just use the id field which is pid, to load it instead?
# however, if we have modified the tree element in place, then
# this would effectively preseve those changes/sending them to RomProperty
# also, if we were loading a new element that was created in the tree
# but this is hypothetical use case
load_tree_prop <- function(ds, tree_element) {
  omid_plugin <- get_plugin_class(tree_element$plugin, tree_element)
  prop <- RomProperty$new(ds, omid_plugin$fromOpenMIBase(tree_element))
  return(prop)
}