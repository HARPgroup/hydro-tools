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
    message(paste("retrieving ", url))
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
      dat <- ds$auth_read(url, content_type = "text/csv", delim=',')
    }
    meta_cols <- c('pid', 'propname', 'hydrocode', 'featureid', 'riverseg')
    rawdata <- as.data.frame(dat)
    if (is.null(alldata) ) {
      alldata = sqldf(
        paste(
          "select a.pid, a.propname, a.hydrocode, a.featureid, a.riverseg, a.attribute_value as ",
          runlabel, 
          "from rawdata as a "
        )
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