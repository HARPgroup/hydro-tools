#' Watershed Model Node data object
#' @title ModelElementBase
#' @description Utility class for interacting with a watershed feature/model combo
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param ds RomDataSource for remote and local storage (required)
#' @param config list of attributes to set/query
#' @param ds_om RomDataSource for legacy model connection (optional)
#' @return R6Class of type ModelElementBase
#' @seealso NA
#' @examples NA
#' @export ModelElementBase
ModelElementBase <- R6Class(
  "ModelElementBase",
  public = list(
    #' @field ds RomDataSource
    ds = NA,
    #' @field ds_om RomDataSource points to legacy model db
    ds_om = NA,
    #' @field hydroid unique ID from dh_feature database
    hydroid = NA,
    #' @field hydrocode non-numeric identifier may reference ext source db
    hydrocode = NA,
    #' @field bundle type of feature
    bundle = NA,
    #' @field ftype sub-type of feature
    ftype = NA,
    #' @field feature a RomFeature entity
    feature = NA,
    #' @field pid integer identifier
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
    #' @field site URI base for the legacy model scripts assumes global var omsite exists for default
    site = NA,
    #' @param ds RomDataSource object mandatory
    #' @param config list of attributes to set, see also: to_list() for format
    #' @param ds_om RomDataSource object pointing to OM (optional)
    #' @param site URI base for the legacy model scripts assumes global var omsite exists for default
    #' @return object instance
    initialize = function(ds, config = list(), ds_om=NA, site=omsite) {
      #message("Created plugin")
      self$ds = ds
      self$ds_om = ds_om
      self$site = site
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
      if (is.na(self$pid)) {
        # check for feature and version
        if (typeof(self$feature)=="environment") {
          if (!is.na(self$version)) {
            self$prop = self$feature$get_prop(propcode=self$version)
            self$pid = self$prop$pid
          }
        }
      }
      if (!is.na(self$pid)) {
        self$prop = RomProperty$new(self$ds,list(pid=self$pid), TRUE)
      }
      return(self$prop)
    },
    #' @param runid run scenario/id to retrieve
    #' @param cached - use local cache? default FALSE
    #' @param hydrowarmup truncate first 2-9 months?
    #' @param cleanup Should the function delete the log file create for the cached argument?
    #' @param outaszoo zoo is default, if FALSE returns dataframe
    #' @return timeseries of run file
    get_run_data = function(runid, cached = FALSE, hydrowarmup = TRUE, cleanup = FALSE, outaszoo = TRUE) {
      if (!is.na(self$elementid)) {
        self$prop = RomProperty$new(self$ds,list(pid=self$pid), TRUE)
        self$pid = self$prop$pid
      }
      dat = om_get_rundata(self$elementid, runid, site = self$site)
      return(dat)
    },
    #' @return TRUE or FALSE
    load_json_model = function() {
      self$json = self$ds$get_json_prop(self$get_model_pid())
      if (typeof(self$json) == "list") {
        if ("om_element_connection" %in% names(model$json)) {
          self$elementid = self$json$om_element_connection$value
        }
        if ("riverseg" %in% names(model$json)) {
          self$riverseg = self$json$riverseg$value
        }
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
    #' @param target_custom1 custom1 to find
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
    #' @param runid runid to summarize
    #' @param export_path where to store the file
    #' @param github_location library to load rendering code
    #' @param cu_pre_var variable for "BEFORE" flow values
    #' @param cu_post_var variable for "AFTER"
    #' @param doc_title document title page text, default generated from feature
    #' @param cu_threshold thresholds of change to code yellow, orange, red
    #' @param include_appendices options 'hydropower', ...
    #' @return file path of rendered output
    render_cia_brief = function(
    runid, export_path=export_path, github_location=github_location, 
    cu_pre_var = "Qout", cu_post_var="Qout", doc_title = 'auto',
    cu_threshold = c(-10, -20, -30), include_appendices = c()) {
      if (doc_title == 'auto') {
        doc_title = paste('Instream Flows Analysis', self$feature$name)
      }
      rmarkdown::render(
        paste0(github_location,'/vahydro/R/OWS_summaries/model_run_brief.Rmd'), 
        output_file = paste0(export_path, paste0('te_', self$feature$hydrocode, '_', runid,'.docx')), 
        params = list( 
          doc_title = doc_title,
          model_feature = self$feature$hydroid, 
          scenario = runid, model_version = self$version, 
          cu_pre_var = cu_pre_var, cu_post_var=cu_post_var, table_cols = 1, 
          model_pid = self$pid, cu_threshold = cu_threshold,
          image_names = c(), image_descriptions = c(),
          include_appendices = include_appendices
        )
      )
    }
  )
)



#' Watershed Model Node data object
#' @title WatershedModelNode
#' @description Utility class for interacting with a watershed feature/model combo
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param ds RomDataSource for remote and local storage (required)
#' @param config list of attributes to set/query
#' @param ds_om RomDataSource for legacy model connection (optional)
#' @return R6Class of type WatershedModelNode
#' @seealso NA
#' @examples NA
#' @export WatershedModelNode
WatershedModelNode <- R6Class(
  "WatershedModelNode",
  inherit = ModelElementBase,
  public = list(
    #' @field ds RomDataSource
    ds = NA,
    #' @field ds_om RomDataSource points to legacy model db
    ds_om = NA,
    #' @field hydroid unique ID from dh_feature database
    hydroid = NA,
    #' @field hydrocode non-numeric identifier may reference ext source db
    hydrocode = NA,
    #' @field bundle type of feature
    bundle = NA,
    #' @field ftype sub-type of feature
    ftype = NA,
    #' @field feature a RomFeature entity
    feature = NA,
    #' @field pid integer identifier
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
    #' @param ds RomDataSource object mandatory
    #' @param config list of attributes to set, see also: to_list() for format
    #' @param ds_om RomDataSource object pointing to OM (optional)
    #' @param site URI base for the legacy model scripts assumes global var omsite exists for default
    #' @return object instance
    initialize = function(ds, config = list(), ds_om=NA, site=omsite) {
      super$initialize(ds, config, ds_om, site=omsite)
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
    },
    #' @param runids runids to summarize
    #' @param export_path where to store the file
    #' @param github_location library to load rendering code
    #' @return file path of rendered output
    render_cia_detailed = function(runids, export_path, github_location) {
      # TBD
      return(FALSE)
    },
    #' @param runid runid to summarize
    #' @param export_path where to store the file
    #' @param github_location library to load rendering code
    #' @param cu_pre_var variable for "BEFORE" flow values
    #' @param cu_post_var variable for "AFTER"
    #' @param doc_title document title page text, default generated from feature
    #' @param cu_threshold thresholds of change to code yellow, orange, red
    #' @param include_appendices options 'hydropower', ...
    #' @return file path of rendered output
    render_cia_brief = function(
      runid, export_path=export_path, github_location=github_location, 
      cu_pre_var = "Qout", cu_post_var="Qout", doc_title = 'auto',
      cu_threshold = c(-10, -20, -30), include_appendices = c()) {
      if (doc_title == 'auto') {
        doc_title = paste('Instream Flows Analysis', self$feature$name)
      }
      super$render_cia_brief(
        runid, export_path, github_location, 
        cu_pre_var, cu_post_var, doc_title,
        cu_threshold, include_appendices
      )
    },
    gage_vs_model = function(runid, gage='auto', export_path=export_path, github_location = github_location) {
      if (gage == 'auto') {
        gage = self$nearest_gage()
      } 
      gage_info = dataRetrieval::readNWISsite(gage)
      render_params <- usgs_calib_rarray(self$json, gage_info, runid)
      rmarkdown::render(
        paste(github_location,'/hydro-tools/USGS/gage_vs_model.Rmd',sep="/"),
        output_file = paste0(
          export_path, 
          gage_info$site_no, '_',
          self$riverseg
        ),
        params = render_params
      )
    }
  )
)



#' Watershed Model Node data object
#' @title WaterSupplyElement
#' @description Utility class for interacting with a facility feature/model combo
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param ds RomDataSource for remote and local storage (required)
#' @param config list of attributes to set/query
#' @param ds_om RomDataSource for legacy model connection (optional)
#' @return R6Class of type WaterSupplyElement
#' @seealso NA
#' @examples NA
#' @export WaterSupplyElement
WaterSupplyElement <- R6Class(
  "WaterSupplyElement",
  inherit = ModelElementBase,
  public = list(
    #' @param ds RomDataSource object mandatory
    #' @param config list of attributes to set, see also: to_list() for format
    #' @param ds_om RomDataSource object pointing to OM (optional)
    #' @param site URI base for the legacy model scripts assumes global var omsite exists for default
    #' @return object instance
    initialize = function(ds, config = list(), ds_om=NA, site=omsite) {
      super$initialize(ds, config, ds_om, site=omsite)
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
    },
    #' @param runids runids to summarize
    #' @param export_path where to store the file
    #' @param github_location library to load rendering code
    #' @return file path of rendered output
    render_cia_detailed = function(runids, export_path, github_location) {
      # TBD
      return(FALSE)
    },
    #' @param runid n to summarize
    #' @param export_path where to store the file
    #' @param github_location library to load rendering code
    #' @param cu_pre_var variable for "BEFORE" flow values
    #' @param cu_post_var variable for "AFTER"
    #' @param doc_title document title page text, default generated from feature
    #' @param cu_threshold thresholds of change to code yellow, orange, red
    #' @param include_appendices options 'hydropower', ...
    #' @return file path of rendered output
    render_cia_brief = function(
    runid, export_path=export_path, github_location=github_location, 
    cu_pre_var = "Qintake", cu_post_var="Qintake", doc_title = 'auto',
    cu_threshold = c(-10, -20, -30), include_appendices = c()) {
      if (doc_title == 'auto') {
        doc_title = paste('Instream Flows Analysis', self$feature$name)
      }
      super$render_cia_brief(
        runid, export_path, github_location, 
        cu_pre_var, cu_post_var, doc_title,
        cu_threshold, include_appendices
      )
    }
  )
)



#' Watershed Model Node data object
#' @title HydroImpoundment
#' @description Utility class for interacting with a facility feature/model combo
#' @details Has standard methods for managing data and meta data
#' @importFrom R6 R6Class  
#' @param ds RomDataSource for remote and local storage (required)
#' @param config list of attributes to set/query
#' @param ds_om RomDataSource for legacy model connection (optional)
#' @return R6Class of type HydroImpoundment
#' @seealso NA
#' @examples NA
#' @export HydroImpoundment
HydroImpoundment <- R6Class(
  "HydroImpoundment",
  inherit = ModelElementBase,
  public = list(
    #' @param ds RomDataSource object mandatory
    #' @param config list of attributes to set, see also: to_list() for format
    #' @param ds_om RomDataSource object pointing to OM (optional)
    #' @param site URI base for the legacy model scripts assumes global var omsite exists for default
    #' @return object instance
    initialize = function(ds, config = list(), ds_om=NA, site=omsite) {
      super$initialize(ds, config, ds_om, site=omsite)
    },
    #' @param storage lookup/interpolate by storage
    #' @param stage lookup/interpolate by stage
    #' @param surface_area lookup/interpolate by surface area
    #' @return file path of rendered output
    ssa_lookup = function(storage=FALSE, stage=FALSE, surface_area=FALSE) {
      # Will return a lookup based on the first matching non-FALSE argument
      svals = FALSE
      json = self$get_json_model()
      if ( !("storage_stage_area" %in% names(json)) ){
        message("Cannot locate storage_stage_area property on", self$feature$name)
        return(FALSE)
      }
      # TODO: migrate this into the dHDataMatrix plugin?
      raw_table = self$json$storage_stage_area$matrix$value
      if (as.character(raw_table[1,1]) == "storage") {
        raw_table = raw_table[-1,]
      }
      rownames(raw_table) <- NULL # insure these are indexed beginning at 1
      colnames(raw_table) <- c('storage', 'stage', 'surface_area')
      ssa = openmi.om.matrix$new()
      ssa$datamatrix <- as.matrix(
        raw_table
      )
      ssa$datamatrix = apply(ssa$datamatrix, 2, FUN=as.numeric)
      ssa$rowtype = as.integer(2)
      ssa$colindex = 'val'
      
      if (!is.logical(storage)) {
        svals = ssa$findMatch(ssa$datamatrix, ssa$datamatrix[,1], storage, 1)
      } else if (!is.logical(stage)) {
        svals = ssa$findMatch(ssa$datamatrix, ssa$datamatrix[,2], stage, 1)
      } else if (!is.logical(surface_area)) {
        svals = ssa$findMatch(ssa$datamatrix, ssa$datamatrix[,3], surface_area, 1)
      }
      return(svals)
    }
  )
)
