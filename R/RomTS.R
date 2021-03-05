

RomTS <- R6Class(
  "RomTS",
  public = list(
    tid = NULL,
    featureid = NULL,
    entity_type = NULL,
    bundle = NULL,
    tstime = NULL,
    tsendtime = NULL,
    tsvalue = NULL,
    tlid = NULL,
    varid = NULL,
    uid = NULL,
    status = NULL,
    datasource = NULL,
    initialize = function(datasource = NULL, config) {
      # todo: some of this can be handled by the RomDataSource?
      self$datasource <- datasource 
      config_cols <- names(config)
      if (is.element("varkey", config_cols)) {
        if (!is.null(self$datasource)) {
          config$varid = self$datasource$get_vardef(config$varkey)
        }
      }
      self$from_list(config)
    },
    from_list = function(config) {
      for (i in names(config)) {
        if (i == "tid") {
          self$tid = config$tid
        } else if (i == "varid") {
          self$varid = config$varid
        } else if (i == "entity_type") {
          self$entity_type = config$entity_type
        } else if (i == "featureid") {
          self$featureid = config$featureid
        } else if (i == "tstime") {
          self$tstime = config$tstime
        } else if (i == "tsendtime") {
          self$tsendtime = config$tsendtime
        } else if (i == "tsvalue") {
          self$tsvalue = config$tsvalue
        } else if (i == "tscode") {
          self$tscode = config$tscode
        } else if (i == "tlid") {
          self$tlid = config$tlid
        } else if (i == "bundle") {
          self$bundle = config$bundle
        }
      }
    },
    from_args = function(tid = NULL, entity_type = NULL, featureid = NULL, varid = NULL, tstime = NULL, tsendtime = NULL, tsvalue = NULL, bundle = NULL) {
      self$tid=tid
      self$varid=varid
      self$entity_type=entity_type
      self$featureid=featureid
      self$tstime=tstime
      self$tsendtime=tsendtime
      self$tsvalue=tsvalue
      self$tscode=tscode
      self$tlid=tlid
      self$bundle=bundle
    },
    to_list = function() {
      t_list <- list(
        tid = self$tid,
        varid = self$varid,
        entity_type = self$entity_type,
        featureid = self$featureid,
        tstime = self$tstime,
        tsendtime = self$tsendtime,
        tsvalue = self$tsvalue,
        tscode = self$tscode,
        tlid = self$tlid,
        bundle = self$bundle
      )
      return(t_list)
    },
    set_prop = function(name, value, format='default') {
      # this should be at the base class, should not have to subclass
      # @tbd: can this even be done on a local object field? 
      # it may not be necessary, we may only implement 
      # calls to set RomProperties linked to it?
      # self$hair <- val
    }
  )
)


