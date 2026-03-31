# ModelElementBase ####
#' Watershed Model Node data object
#' @title ModelElementBase
#' @description Utility class for interacting with a watershed feature/model combo
#' @details . This R6 object has standard methods for managing the data and meta
#'   data related to OWS model data. These objects may be initialized with OWS
#'   model data. The objects will store relevant model data and has methods to
#'   load related feautre or model data.
#' @importFrom R6 R6Class  
#' @import openmi.om
#' @param ds RomDataSource for remote and local storage (required; often
#'   provided in DEQ config files)
#' @param config list of attributes to set/query and used to identify model
#'   elements in datasource
#' @param ds_om RomDataSource for legacy model connection (optional)
#' @return R6Class of type ModelElementBase
#' @seealso NA
#' @examples NA
#' @export ModelElementBase
ModelElementBase <- R6Class(
  "ModelElementBase",
  public = list(
    #' @field ds RomDataSource often provided in DEQ config files and features a
    #'   local data storage or a connection to a database/RESTful service
    ds = NA,
    #' @field ds_om RomDataSource points to legacy model database and may also
    #'   be provided in DEQ config file upon request to OWS modelling staff
    ds_om = NA,
    #' @field hydroid unique ID from dh_feature database to identify the
    #'   physical feature that is or has been modelled by OWS
    hydroid = NA,
    #' @field hydrocode non-numeric identifier in dh_feature that often defines
    #'   external data source connections or identifiers
    hydrocode = NA,
    #' @field bundle type of feature in dh_feature. See ReadMe for more details.
    bundle = NA,
    #' @field ftype sub-type of feature in dh_feature. See ReadMe for more details.
    ftype = NA,
    #' @field feature a \code{RomFeature} entity that has found a valid feature in dh_feature
    feature = NA,
    #' @field pid integer identifier of the model as found in dh_properties
    pid = NA,
    #' @field version descriptive code for the model version. Often 'vahydro-1.0'
    version = NA,
    #' @field prop a RomProperty that is defined by the model base property
    prop = NA,
    #' @field json Modelling data (inputs) JSON
    json = NA,
    #' @field elementid An integer identifier of the OM model element
    elementid = NA,
    #' @field rocode describes runoff type default = 'cbp6_lrseg'
    rocode = 'cbp6_lrseg',
    #' @field name The model name
    name = NA,
    #' @field riverseg The riverseg code defined for the watershed node in VA
    #'   Hydro OM model (often derived or related to the river segment codes
    #'   used in the CB Watershed Model)
    riverseg = NA,
    #' @field site URI base for the legacy model scripts assumes global var omsite exists for default
    site = NA,
    #' @description
    #' Initialize a ModelElementBase instance, returning an R6 object that is
    #' now populated with model data in its fields through queries of the
    #' provided ds using the \code{handle_config()}, \code{get_feature()} and
    #' \code{get_model()} methods. This object will also have numerous methods
    #' that are described in \code{?ModelElementBase}.
    #' @param ds RomDataSource object mandatory. This is most often created in
    #'   DEQ config files and will feature connections to local or OWS data
    #'   bases
    #' @param config list of attributes to set, see also: to_list() for format
    #' @param ds_om RomDataSource object pointing to OM (optional)
    #' @param site URI base for the legacy model scripts assumes global var
    #'   omsite exists for default (as often created in DEQ config files)
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
    #' @description
    #' Parse the user provided config list to define essential fields to be used
    #' in ds queries to identify relevant model and feature
    #' @param config list of attributes to set, see also: to_list() for format,
    #' often provided in initialize call of the instance
    #' @return TRUE, to indicate successful config parsing. Populates pid,
    #'   hydroid, bundle, ftype, hydrocode, version, and rocode fields on instance
    #'   depending on user config.
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
    #' @description
    #' Initialize a \code{RomFeature()} using the data source and either the hydroid
    #' field or the hydrocode, bundle, and ftype on this object instance. Some
    #' of these latter fields may be NULL per RomFeature.
    #' @return The R6 RomFeature found. This will also populate the feature
    #'   field of this object instance.
    get_feature = function() {
      if (!is.na(self$hydroid)) {
        self$feature = RomFeature$new(self$ds, list(hydroid=self$hydroid), TRUE)
      } else {
        self$feature = RomFeature$new(ds, list(hydrocode=self$hydrocode, bundle=self$bundle, ftype=self$ftype), TRUE)
      }
      return(self$feature)
    },
    #' @description
    #' A simple function that gets the self pid or returns FALSE if not
    #' populated. A placeholder for future development that serves as QC to
    #' ensure a PID has been populated.
    #' @return Self PID field if present or FALSE if not.
    get_model_pid = function() {
      if (is.na(self$pid)) {
        return(FALSE)
      }
      return(self$pid)
    },
    #' @description
    #' Populates the prop field of this object instance by either querying the
    #' datasource field (if the PID field of this object is populated) or
    #' otherwise using the \code{get_prop(propcode = self$version)} method on
    #' the feature field to find the appropriate property by querying off the
    #' feature
    #' @return The R6 RomProperty found. This will also populate the prop field of
    #'   this object instance.
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
    #' @description Gets and returns the model run data for a given model
    #' elementID and scenario runID. Model data will be returned as a zoo or a
    #' data frame depending on user request
    #' @param runid integer run id representing the scenario. Ask the modeling team
    #'   for scenario IDs if you are unsure otherwise see the WSPA Shiny Dashboard
    #'   for more information
    #' @param cached boolean - if TRUE will use recently stored local copy
    #' @param hydrowarmup boolean - if TRUE will trim beginning of model time frame
    #'   to account for potential model warm up as water routes downstream from the
    #'   headwaters and operational rules engage
    #' @param cleanup Logical. Should the function delete the log file create for
    #'   the cached argument? If this is TRUE, the .log files will be deleted after
    #'   download from OM server
    #' @param outaszoo boolean return as a zoo timeseries with numeric mode if TRUE,
    #'   or as data frame if FALSE. Default is TRUE.
    #' @return timeseries of run file as either data frame or zoo depending on
    #'   user input
    get_run_data = function(runid, cached = FALSE, hydrowarmup = TRUE, cleanup = FALSE, outaszoo = TRUE) {
      if (!is.na(self$elementid)) {
        self$prop <- RomProperty$new(self$ds,list(pid=self$pid), TRUE)
      }
      dat <- om_get_rundata(self$elementid, runid, site = self$site,
                           cached = cached, hydrowarmup = hydrowarmup,
                           cleanup = cleanup,  outaszoo = outaszoo)
      return(dat)
    },
    
    #' @description Uses the \code{get_json_prop()} method on this objects data
    #'   source to get model json from this object's PID field. Stores the json
    #'   as a field on this object. This method will then try to populate the
    #'   elementid and riverseg fields on this object based on values in the
    #'   JSON, if they exist
    #' @return Nothing
    load_json_model = function() {
      self$json = self$ds$get_json_prop(self$get_model_pid())
      if (typeof(self$json) == "list") {
        if ("om_element_connection" %in% names(model$json)) {
          self$elementid <- self$json$om_element_connection$value
        }
        if ("riverseg" %in% names(model$json)) {
          self$riverseg <- self$json$riverseg$value
        }
      }
    },
    #' @description Calls the \code{load_json_model()} method to load the model
    #'   json and returns it. If the json field on this object is already
    #'   populated, will return what's available
    #' @return json of model from dh_properties as set in
    get_json_model = function() {
      if (typeof(self$json) != "list") {
        self$load_json_model()
      }
      return(self$json)
    },
    #' @description Grabs the original model XML file and converts it to a
    #'   readible list to return to the use
    #' @param elementid target model container to search (OM model ID)
    #' @param oborops target container to search
    #' @return elemxml converted to list
    get_elem_xml= function(elementid = FALSE, oborops='object') {
      # NOTE: this is not yet working.
      # the xml comes in, and it appears to parse yielding a list-like
      # vawriable attributes, but those attributes cannot be viewed
      if (elementid == FALSE) {
        elementid = self$elementid
      }
      raw_xml <- sqldf::sqldf(
        paste(
          "select elem_xml, elemoperators 
           from scen_model_element 
           where elementid =", elementid
        ), 
        connection = self$ds_om$connection
      )
      if (oborops == 'operators') {
        rxx = as.character(raw_xml[['elemoperators']])
        trim_xml = substr(rxx, 3,stringr::str_length(rxx) -2)
      } else {
        trim_xml = raw_xml[['elem_xml']]
      }
      expdoc <- xml2::read_xml(trim_xml)
      exp <- xml2::as_list(expdoc)
      return(exp)
    },
    #' @description Returns all model elementids that are upstream and
    #'   optionally downstream of the target model. These VAHydro OM elementids
    #'   can be used to identify related models within a modelElementBase()
    #'   object
    #' @param elementid target OM elementid to build the tree off of (e.g. the container to search)
    #' @param include_children recurse through children? default = 1
    #' @param max_recursion_level integer how far to search if include_children = 1
    #' @param exclude_custom1 custom1 to exclude (use cova_upstream prevent upstream recursion)
    #' @param target_custom1 custom1 to find
    #' @return dataframe of elementids
    om_element_tree = function(
    elementid=FALSE, include_children=1, max_recursion_level=-1, 
    exclude_custom1=-1, target_custom1=-1) {
      if (elementid == FALSE) {
        elementid = self$elementid
      }
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
    #' @description Render the CIA brief with basic parameters set by user for
    #'   this model
    #' @param runid runid to summarize
    #' @param export_path path to render the word document
    #' @param github_location Path to github folder that should contain the
    #'   vahydro OWS repository
    #' @param cu_pre_var variable for "BEFORE" flow values e.g. before permit
    #'   implementation
    #' @param cu_post_var variable for "AFTER" flow values e.g. after permit
    #'   implementation
    #' @param doc_title document title page text. The default value, auto, will
    #'   create a title based on the feature name
    #' @param cu_threshold thresholds of change to code yellow, orange, red in
    #'   display CIA tables drawn from \code{om_cu_table()}
    #' @param include_appendices options for appendices like 'hydropower', ...
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


# WatershedModelNode ####
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
    #' @param runid runid to summarize
    #' @param export_path where to store the file
    #' @param gage what to compare to
    #' @param github_location library to load rendering code
    gage_vs_model = function(
    runid, gage='auto', export_path=export_path, github_location = github_location) {
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


# WaterSupplyElement ####
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


# HydroImpoundment ####
#' Reservoir object
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
      ssa = openmi.om::openmi.om.matrix$new()
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
