#'@name om_vahydro_metric_grid
#'@title Query Model Metrics
#'@description Get scenario model metrics from runids
#'@details This function queries a table in the datasource (default dh_feature)
#'  for models of \code{model_version} metrics. These metrics are specific to
#'  the \code{metric} and \code{runid} provided by user. Each combination of
#'  metric and runid will be a separate column in the resulting long-style data
#'  frame
#' @param metric character vector of requested metrics
#' @param runids Either a character vector of model run ids (in the form
#'   'runid_600', for example) or a dataframe with model_version, runid, metric,
#'   and runlabel specified. The data frame is generally deprecated in favor of
#'   using the individual arguments on this function
#' @param featureid integer or 'all', representing which specific features to
#'   query for models
#' @param entity_type character (single-length) to represent the table to query
#'   for models, defaults to \code{dh_feature}
#' @param bundle character vector representing feature types. Only these 
#'  features will be queried for models of \code{model_version}. May be 'all' to
#'  query all bundle types. Defaults to 'watershed'
#' @param ftype character vector representing feature types, i.e. the bundle
#'   subclass. Only these features will be queried for models of
#'   \code{model_version}. May be 'all' to query all ftypes. Defaults to 'vahydro'
#' @param model_version character vector with default 'vahydro-1.0'. This will
#'   be used to determine which models to find on \code{entity_type}
#' @param base_url character, deprecated replaced by global datasource
#' @param ds datasource object of class \code{RomDataSource} often created in DEQ
#'   config files
#' @param debug Logical. If TRUE, this function will message out the SQL query
#'   used to find model scenario metrics
#'@return A long-style data frame in which each row represents a model and
#'  columns are present for each combination of runid and metric input by user,
#'  populated by the model scenario metric value
#'@examples 
#'\dontrun{
#'library("hydrotools")
#'basepath='/var/www/R'
#'source(paste(basepath,'config.R',sep='/'))
#'m_version = c('vahydro-1.0')
#'runids <- c('runid_600','runid_400')
#'metrics <- c("WA_90_mgd", "Qavailable_90_mgd","Smin_L90_mg"
#'om_vahydro_metric_grid(
#'  metric = metrics,
#'  runids = runid,
#'  bundle = "watershed",
#'  ftype = "vahydro",
#'  ds = ds)
#'}
#'@export om_vahydro_metric_grid
om_vahydro_metric_grid <- function (
    metric,
    runids,
    featureid = 'all',
    entity_type = 'dh_feature',
    bundle = 'watershed',
    ftype = 'vahydro',
    model_version = 'vahydro-1.0',
    base_url = "http://deq1.bse.vt.edu/d.dh/entity-model-prop-level-export",
    ds = FALSE,
    debug = FALSE
) {
  #Parse out inputs if user submits a data frame as runinfo (legacy version
  #mandated this)
  if (is.data.frame((runids))) {
    message("Found info")
    message(runinfo)
    # user is passing in other params in data frame format
    runid <- as.character(runids$runid)
    if (!is.null(runids$model_version)) model_version <- unique(as.character(runids$model_version))
    if (!is.null(runids$metric)) metric <- unique(as.character(runids$metric))
    #We don't need unique runlabels as we will match these later on
    if (!is.null(runids$runlabel)) runlabel <- as.character(runids$runlabel)
  } else {
    # All arguments passed as vectors, set runid
    runid <- runids
  }
  
  # Run either odbc query to get model metric data or try auth_read on a REST
  # service depending on user request
  if (is.logical(ds)) {
    # using old method with global expected token.
    # warn that this is deprecated.
    message("om_vahydro_metric_grid() called without RomDataSource ")
    message("Global token is deprecated, please call with parameter ds = [RomDataSource]")
    params <- paste(featureid,entity_type,bundle,ftype,model_version, runid, metric,sep="/")
    url <- paste(base_url,params,sep="/")
    rawdat <- httr::GET(
      url,
      httr::add_headers(HTTP_X_CSRF_TOKEN = token),
      encode = "xml", httr::content_type("text/csv")
    )
    dat <- httr::content(rawdat)
  } else {
    if (ds$connection_type == 'odbc') {
      #If odbc, devleop SQL query to send to data base
      prop_sql <- om_vahydro_metric_grid_sql(featureid,entity_type,bundle,
                                              ftype,model_version, runid, metric) 
      #If debug flag is TRUE, show query in message for debugging:
      if (debug) {
        message(prop_sql)
      }
      message(paste("retrieving via ODBC"))
      #Query ds for metrics
      dat <- DBI::dbGetQuery(conn = ds$connection, prop_sql)
    } else {
      #Deprecated VA Hydro REST service - preserved in case REST is requried
      #in the future by OIS
      message(paste("retrieving ", url))
      dat <- ds$auth_read(url, content_type = "text/csv", delim=',')
    }
    
  }
  #Pivot wider and simplify so that each name/run is a column with the
  #corresponding metric value
  alldata <- dat |> 
    dplyr::select(pid, propname, hydrocode, featureid, riverseg,
                  run_name, attribute_name, attribute_value) |> 
    tidyr::pivot_wider(names_from = c(attribute_name, run_name), 
                       values_from = attribute_value,
                       names_sep = "_") |> 
    as.data.frame()
  
  #Older versions of VA Hydro metric grid allowed for runlabel specification. In
  #this case, we need to rename the appropriate columns with the user provided
  #value
  if(is.data.frame((runids)) && !is.null(runlabel)){
    #Create a data frame with the runids and metrics from the names of alldata. 
    #This preserves the order of the data and ensures proper name replacement
    df_runid <- gsub(".*_(runid.*)","\\1",names(alldata))
    df_metric <- gsub("(.*)_runid.*","\\1",names(alldata))
    df_data <- data.frame(dataname = names(alldata), datarunid = df_runid,
                          datametric = df_metric)
    #Join on the user provided runlabels by joining in the runids data frame by
    #runid and metric
    join_data <- sqldf::sqldf(
      "SELECT *
      FROM df_data as d
      LEFT JOIN runids as r
      ON r.runid = d.datarunid
      AND r.metric = d.datametric"
    )
    #Columns like pid and propname will not have a run label, so just set these
    #equivalent to the base column name stored in dataname
    join_data$runlabel[is.na(join_data$runlabel)] <- join_data$dataname[is.na(join_data$runlabel)]
    #Replace all the appropriate column names in order with the user provided
    #label
    names(alldata)[match(join_data$dataname, names(alldata))] <- 
      join_data$runlabel[match(names(alldata), join_data$dataname)]
  }
  #Return the long-style data frame
  return(alldata)
}

 
#'@name om_vahydro_metric_grid_sql
#'@title Build model metric SQL
#'@description Helper function to generate a metric grid via SQL
#'@details This function builds a character vector of executable SQL to query
#'  model \code{metric} of version \code{model_version} based on user input.
#'  Used in \code{hydrotools::om_vahydro_metric_grid()}
#' @param metric character vector of requested metrics
#' @param runid Either a character vector of model run ids or a dataframe with
#'   model_version, runid, metric, and runlabel specified. The data frame is
#'   generally deprecated in favor of using the individual arguments on this
#'   function
#' @param featureid integer or 'all', representing which specific features to
#'   query for models
#' @param entity_type character (single-length) to represent the table to query
#'   for models, e.g. \code{dh_feature}
#' @param bundle character vector representing feature types. Only these 
#'  features will be queried for models of \code{model_version}. May be 'all' to
#'  query all bundle types.
#' @param ftype character vector representing feature types, i.e. the bundle
#'   subclass. Only these features will be queried for models of
#'   \code{model_version}. May be 'all' to query all ftypes. 
#' @param model_version character vector. This will
#'   be used to determine which models to find on \code{entity_type}
#' @return Character with executable SQL query
#'@examples 
#'entity_type <- 'dh_feature'
#'featureid <- 'all'
#'bundle <- 'all'
#'ftype <- 'all'
#'model_version <- 'vahydro-1.0'
#'runid <- c('runid_600','runid_400')
#'metric <- c("WA_90_mgd", "Qavailable_90_mgd","Smin_L90_mg")
#'om_vahydro_metric_grid_sql(featureid,entity_type,bundle,
#'   ftype,model_version, runid, metric) 
#' @export om_vahydro_metric_grid_sql
om_vahydro_metric_grid_sql <- function(featureid,entity_type,bundle,
                                        ftype,model_version, runid, metric) {
  prop_sql <- "
    SELECT model.pid AS pid, model.propname as propname, 
    model.propcode AS propcode, 
    model.propvalue AS propvalue, 
    model.startdate AS startdate, model.enddate AS enddate, 
    dh_variabledefinition.varkey AS varkey, 
    dh_variabledefinition.varname AS varname, 
    scenario.propname AS run_name, 
    metric.propname AS attribute_name, metric.propvalue AS attribute_value, 
    metric.propcode AS attribute_code, 
    model.featureid AS featureid, 
    base_entity.hydrocode AS hydrocode, 
    elementid.propvalue AS om_elementid, 
    riverseg.propcode AS riverseg
    FROM dh_properties as model
    LEFT JOIN [entity_type] as base_entity 
    ON model.featureid = base_entity.hydroid AND model.entity_type = '[entity_type]'
    LEFT JOIN dh_properties as scenario 
    ON model.pid = scenario.featureid AND scenario.entity_type = 'dh_properties'
    LEFT JOIN dh_properties as metric 
    ON scenario.pid = metric.featureid AND metric.entity_type = 'dh_properties'
    LEFT JOIN dh_properties as elementid 
    ON model.pid = elementid.featureid AND (elementid.entity_type = 'dh_properties' AND elementid.propname = 'om_element_connection')
    LEFT JOIN dh_properties as riverseg 
    ON model.pid = riverseg.featureid AND (riverseg.entity_type = 'dh_properties' AND riverseg.propname = 'riverseg')
    LEFT JOIN dh_variabledefinition 
    ON model.varid = dh_variabledefinition.hydroid
    WHERE model.entity_type = '[entity_type]'
      AND model.propcode IN ([model_version])
      AND scenario.propname IN ([runid]) 
      AND metric.propname IN ([metric])
  "
  if ( all(bundle != '') & all(bundle != 'all')) {
    prop_sql <- paste(prop_sql, " AND base_entity.bundle IN ([bundle]) ")
  }
  if ( all(ftype != '') & all(ftype != 'all')) {
    prop_sql <- paste(prop_sql, " AND base_entity.ftype IN ([ftype]) ")
  }
  prop_sql <- gsub("\\[entity_type\\]", entity_type, prop_sql)
  prop_sql <- gsub("\\[bundle\\]", paste0("'",bundle,"'",collapse = ","), prop_sql)
  prop_sql <- gsub("\\[ftype\\]", paste0("'",ftype,"'",collapse = ","), prop_sql)
  prop_sql <- gsub("\\[model_version\\]", paste0("'",model_version,"'",collapse = ","), prop_sql)
  prop_sql <- gsub("\\[runid\\]", paste0("'",runid,"'",collapse = ","), prop_sql)
  prop_sql <- gsub("\\[metric\\]", paste0("'",metric,"'",collapse = ", "), prop_sql)
  return(prop_sql)
}


#'@name om_metric_matrix
#'@title Build om_vahydro_metric_grid data frame input
#'@description Helper function to generate old-style inputs for
#'  om_vahydro_metric_grid
#'@details Builds a data frame of runids, metrics, and runlabels to provide as
#'  the runids argument in \code{om_vahydro_metric_grid()}
#'@param metrics character vector of requested metrics
#'@param runids Either a character vector of model run ids or a dataframe with
#'   model_version, runid, metric, and runlabel specified. The data frame is
#'   generally deprecated in favor of using the individual arguments on this
#'   function
#'@param m_version character vector. This will
#'   be used to determine which models to find on \code{entity_type}
#'@return data frame to provide to \code{om_vahydro_metric_grid()}
#'@examples 
#'m_version <- c('vahydro-1.0')
#'runids <- c('runid_600','runid_400')
#'metrics <- c("WA_90_mgd", "Qavailable_90_mgd","Smin_L90_mg")
#'omgrid_df <- om_metric_matrix(m_version, runids, metrics) 
#'@export
om_metric_matrix <- function(m_version, runids, metrics) {
  df <- data.frame(
    model_version = rep(m_version, each = length(runids) * length(metrics)),
    runid = rep(runids, length(metrics) * length(m_version)),
    metric = rep(rep(metrics,each = length(runids)), length(m_version))
  )
  df$runlabel <- paste0(df$metric,"_",df$runid)
  return(df)
}
