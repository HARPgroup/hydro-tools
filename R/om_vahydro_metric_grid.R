#' The base class for executable equation based meta-model components.
#'
#' @param metric character deprecated in favor of runids array
#' @param runids dataframe with model and run info
#' @param featureid integer or 'all' 
#' @param entity_type character
#' @param bundle character 
#' @param ftype character feature type
#' @param model_version character default 'vahydro-1.0'
#' @param base_url character deprecated to be replaced by global datasource
#' @param ds datasource object of class RomDataSource
#' @return reference class of type openmi.om.equation
#' @seealso NA
#' @export om_vahydro_metric_grid
#' @examples NA
om_vahydro_metric_grid <- function (
  metric,
  runids,
  featureid = 'all',
  entity_type = 'dh_feature',
  bundle = 'watershed',
  ftype = 'vahydro',
  model_version = 'vahydro-1.0',
  base_url = "http://deq1.bse.vt.edu/d.dh/entity-model-prop-level-export",
  ds = FALSE
) {
  alldata = NULL
  mv_base = NULL
  for (i in 1:nrow(runids)) {
    runinfo = runids[i,]
    if (is.data.frame((runinfo))) {
      message("Found info")
      message(runinfo)
      # user is passing in other params in data frame format
      runid = as.character(runinfo$runid)
      if (!is.null(runinfo$model_version)) model_version = as.character(runinfo$model_version)
      if (!is.null(runinfo$metric)) metric = as.character(runinfo$metric)
      if (!is.null(runinfo$runlabel)) runlabel = as.character(runinfo$runlabel)
    } else {
      # only runid is passed in
      runid = runinfo
      runlabel = runid
    }
    if (is.null(mv_base)) {
      # Annotate the first model version here in order to prevent
      # redundant joins if we are comparing models from 
      # features that have multiple models in the same version domain
      # as happens with some facility/riverseg models
      mv_base <- model_version
    }
    runlabel <- stringr::str_replace_all(runlabel, '-', '_')
    runlabel <- stringr::str_replace_all(runlabel, ' ', '_')
    params <- paste(featureid,entity_type,bundle,ftype,model_version, runid, metric,sep="/")
    url <- paste(base_url,params,sep="/")
    if (is.logical(ds)) {
      # using old method with global expected token.
      # warn that this is deprecated.
      message("om_vahydro_metric_grid() called without RomDataSource ")
      message("Global token is deprecated, please call with parameter ds = [RomDataSource]")
      rawdat <- httr::GET(
        url,
        httr::add_headers(HTTP_X_CSRF_TOKEN = token),
        encode = "xml", httr::content_type("text/csv")
      );
      dat <- content(rawdat)
    } else {
      if (ds$connection_type == 'odbc') {
        #message("om_vahydro_metric_grid() called using ODBC ")
        prop_sql = om_vahydro_metric_grid_sql(featureid,entity_type,bundle,ftype,model_version, runid, metric) 
        message(prop_sql)
        message(paste("retrieving via ODBC"))
        dat <- sqldf(prop_sql, connection = ds$connection)
        #message(paste("returned", nrow(dat),"rows"))
      } else {
        message(paste("retrieving ", url))
        dat <- ds$auth_read(url, content_type = "text/csv", delim=',')
        #message("om_vahydro_metric_grid() called using http views ")
      }
      
    }
    meta_cols <- c('pid', 'propname', 'hydrocode', 'featureid', 'riverseg')
    rawdata <- as.data.frame(dat)
    if (is.null(alldata) ) {
      alldata = sqldf(
        paste(
          "select a.pid, a.propname, a.hydrocode, a.featureid, a.riverseg, a.attribute_value as ",
          runlabel, 
          "from rawdata as a "
        ), method = "raw"
      )
    } else {
      data_cols <- paste(names(alldata)[ !names(alldata) %in% meta_cols],collapse=' ,')
      mergeq = paste(
        "select CASE WHEN a.pid is NULL THEN b.pid ELSE a.pid END as pid,
            CASE WHEN a.propname is null then b.propname ELSE a.propname END AS propname,
            CASE WHEN a.hydrocode is null THEN b.hydrocode ELSE a.hydrocode END as hydrocode,
            CASE WHEN a.featureid is null THEN b.featureid ELSE a.featureid END AS featureid,
            CASE WHEN a.riverseg is null THEN b.riverseg ELSE a.riverseg END AS riverseg,",
            data_cols, ",",
            "b.attribute_value as ",
        runlabel, 
        "from alldata as a 
        full join rawdata as b 
        on (
          a.featureid = b.featureid
        "
      )
      onclause <- ""
      if (model_version == mv_base) {
        mergeq <- paste(mergeq, " and a.pid = b.pid")
      }
      mergeq <- paste(mergeq, ")")
      message(mergeq)
      alldata = sqldf(
        mergeq
      )
    }
  }
  return(alldata)
}

#' Helper function to generate a metric grid via SQL
#' featureid,entity_type,bundle,ftype,model_version, runid, metric
#' @param featureid integer or 'all' 
#' @param entity_type character
#' @param bundle character 
#' @param ftype character feature type
#' @param model_version character default 'vahydro-1.0'
#' @param runid run info
#' @param metric character deprecated in favor of runids array
#' @return string with executable SQL query
#' @seealso NA
#' @export om_vahydro_metric_grid_sql
#' @examples NA
om_vahydro_metric_grid_sql <- function(featureid,entity_type,bundle,ftype,model_version, runid, metric) {
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
    WHERE (( (model.entity_type = '[entity_type]') 
      AND (base_entity.bundle = '[bundle]') 
      AND (base_entity.ftype = '[ftype]') 
      AND (model.propcode = '[model_version]') 
      AND (scenario.propname = '[runid]') 
      AND (metric.propname = '[metric]') 
    ))
  "
  prop_sql <- str_replace_all(prop_sql, "\\[bundle\\]", bundle)
  prop_sql <- str_replace_all(prop_sql, "\\[entity_type\\]", entity_type)
  prop_sql <- str_replace_all(prop_sql, "\\[ftype\\]", ftype)
  prop_sql <- str_replace_all(prop_sql, "\\[model_version\\]", model_version)
  prop_sql <- str_replace_all(prop_sql, "\\[runid\\]", runid)
  prop_sql <- str_replace_all(prop_sql, "\\[metric\\]", metric)
  return(prop_sql)
}