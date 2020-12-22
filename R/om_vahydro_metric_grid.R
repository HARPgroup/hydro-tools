library('stringr')
om_vahydro_metric_grid <- function (
  metric,
  runids,
  featureid = 'all',
  entity_type = 'dh_feature',
  bundle = 'watershed',
  ftype = 'vahydro',
  model_version = 'vahydro-1.0',
  base_url = "http://deq2.bse.vt.edu/d.dh/entity-model-prop-level-export"
) {
  alldata = NULL
  mv_base = NULL
  for (i in 1:nrow(runids)) {
    runinfo = runids[i,]
    if (is.data.frame((runinfo))) {
      print("Found info")
      print(runinfo)
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
    runlabel <- str_replace_all(runlabel, '-', '_')
    runlabel <- str_replace_all(runlabel, ' ', '_')
    params <- paste(featureid,entity_type,bundle,ftype,model_version, runid, metric,sep="/")
    url <- paste(base_url,params,sep="/")
    print(paste("retrieving ", url))
    rawdat <- GET(
      url,
      add_headers(HTTP_X_CSRF_TOKEN = token),
      encode = "xml", content_type("text/csv")
    );
    dat <- content(rawdat)
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
      mergeq = paste(
        "select a.*, b.attribute_value as ",
        runlabel, 
        "from alldata as a 
        left outer join rawdata as b 
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