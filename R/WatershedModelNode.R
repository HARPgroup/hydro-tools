

#' @export WatershedModelNode
WatershedModelNode <- R6Class(
  "watershed_model_node",
  public = list(
    #' @field ds RomDataSource
    ds = NA,
    #' @field ds RomDataSource points to legacy model db
    ds_om = NA,
    #' @field hydroid
    hydroid = NA,
    #' @field hydrocode
    hydrocode = NA,
    #' @field bundle
    bundle = NA,
    #' @field ftype
    ftype = NA,
    #' @field RomFeature
    feature = NA,
    #' @field pid integer
    pid = NA,
    #' @field version descriptive code for model
    version = NA,
    #' @field prop as RomProperty
    prop = NA,
    #' @field json as list
    json = NA,
    #' @field elementid as integer
    elementid = NA,
    #' @field rocode describes runoff type default = 'cbp6_lrseg'
    rocode = 'cbp6_lrseg',
    #' @field name what is it called
    name = NA,
    #' @field riverseg we using code names
    riverseg = NA,
    #' @param config list of attributes to set, see also: to_list() for format
    #' @return object instance
    initialize = function(ds, config = list(), ds_om=NA) {
      #message("Created plugin")
      self$ds = ds
      self$ds_om = ds_om
      self$handle_config(config)
      self$get_feature()
      self$get_model()
    },
    #' @return json of model from dH
    handle_config = function(config) {
      
      if ("pid" %in% names(config)) {
        self$pid = config[['pid']]
      }
      if ("hydroid" %in% names(config)) {
        self$hydroid = config[['hydroid']]
      }
      if ("bundle" %in% names(config)) {
        self$bundle = config[['bundle']]
      }
      if ("ftype" %in% names(config)) {
        self$ftype = config[['ftype']]
      }
      if ("hydrocode" %in% names(config)) {
        self$hydrocode = config[['hydrocode']]
      }
      if ("version" %in% names(config)) {
        self$version = config[['version']]
      }
      if ("rocode" %in% names(config)) {
        self$rocode = config[['rocode']]
      }
      return(TRUE)
    },
    #' @return json of model from dH
    get_feature = function() {
      if (!is.na(self$hydroid)) {
        self$feature = RomFeature$new(self$ds, list(hydroid=self$hydroid), TRUE)
      } else {
        self$feature = RomFeature$new(ds, list(hydrocode=self$hydrocode, bundle=self$bundle, ftype=self$ftype), TRUE)
      }
      return(self$feature)
    },
    #' @return json of model from dH
    get_model_pid = function() {
      if (is.na(self$pid)) {
        return(FALSE)
      }
      return(self$pid)
    },
    #' @return json of model from dH
    get_model = function() {
      if (!is.na(self$pid)) {
        self$prop = RomProperty$new(self$ds,list(pid=self$pid), TRUE)
        self$pid = self$prop$pid
      } else {
        # check for feature and version
        if (typeof(self$feature)=="environment") {
          if (!is.na(self$version)) {
            self$prop = self$feature$get_prop(propcode=self$version)
            self$pid = self$prop$pid
          }
        }
      }
      return(self$prop)
    },
    #' @return TRUE or FALSE
    load_json_model = function() {
      self$json = self$ds$get_json_prop(self$get_model_pid())
      if (typeof(self$json) == "list") {
        self$elementid = self$json$om_element_connection$value
      }
    },
    #' @return json of model from dH
    get_json_model = function() {
      if (typeof(self$json) != "list") {
        self$load_json_model()
      }
      return(self$json)
    },
    #' @param elementid target container to search
    #' @param include_children recurse through children? default = 1
    #' @param max_recursion_level integer how far to search if include_children = 1
    #' @param exclude_custom1 custom1 to exclude (use cova_upstream prevent upstream recursion)
    #' @parawm target_custom1 custom1 to find
    #' @return dataframe of elementids
    om_element_tree = function(
      elementid, include_children=1, max_recursion_level=-1, 
      exclude_custom1=-1, target_custom1=-1) {
      
      sql=paste0(
        "WITH RECURSIVE element_tree AS (
              SELECT p.*, 0 AS level, e.custom1
              FROM map_model_linkages as p
              left outer join scen_model_element as e
              on (
                e.elementid = p.src_id
              )
              WHERE p.dest_id = ", elementid, "
                AND linktype = 1
              UNION
              SELECT c.*, p.level + 1 as level, e.custom1
              FROM map_model_linkages as c
              left outer join scen_model_element as e
              on (
                e.elementid = c.src_id
              )
              inner join element_tree as p
              on (c.dest_id = p.src_id and c.linktype = 1 and ",
        include_children, " = 1)
              WHERE ( (e.custom1 <> '", exclude_custom1, "') 
                OR ('", exclude_custom1, "' = '-1') )
            )
          SELECT src_id from element_tree
          WHERE ((level <= ", max_recursion_level, ") OR (", max_recursion_level, " = -1))
              AND ( (custom1 = '", target_custom1, "') 
                OR ('", target_custom1, "' = '-1') );"
      )
      message(sql)
      elids = sqldf(sql, connection=self$ds_om$connection)
      return(elids)
    },
    #' @return list of elementids corresponding to runoff props uses rocode prop to filter
    get_runoff_elids = function() {
      # TBD
      # try to load dH linkages
      #self$json = self$get_json_model()
      #if ('1. Local Runoff Inflows' %in% names(self$json)) {
      #  ro = find_name(self$json, '1. Local Runoff Inflows')
      #}
      # query ds_model for
      if (!is.logical(self$ds_om)) {
        exclude_custom1 = "cova_upstream"
        include_children = 1
        max_recursion_level=4
        target_custom1 = self$rocode
        elementid = self$elementid
        elids = self$om_element_tree(elementid, include_children, max_recursion_level, exclude_custom1, target_custom1)
      }
      return(elids)
    },
    #' @return data frame with the closest gage
    nearest_gage = function() {
      usgs_nearest_gage(self$feature, self$get_json_model())
    }
  )
)

