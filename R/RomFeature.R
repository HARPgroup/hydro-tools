# Global object store holds references to all objects of a given domain
# and pointers to remote data source if applicable.
# if remote data source is set, all data can be synched back to it
# no mixing of data sources is allowed
RomDataSource <- R6Class(
  "RomDataSource",
  private = list(
    token = NULL # rest token if remote rest source is used
  ),
  public = list(
    base_url = NULL,
    initialize = function(base_url, prop_db = NULL, ts_db = NULL) {
      self$base_url = base_url
    },
    get_token = function() {
      private$token <- om_vahydro_get_token(self$base_url)
    },
    timeline = NULL,
    tsvalues = data.frame(
      tid=character(),
      tsvalue=character(),
      tscode=character(),
      tstime=character(),
      tsendtime=character(),
      featureid=character(),
      modified=character(),
      entity_type=character(),
      varid=character(),
      uid=character(),
      status=character(),
      stringsAsFactors=FALSE
    ),
    props = data.frame(
      proptext=character(),
      pid=character(),
      propname=character(), 
      propvalue=character(),
      propcode=character(),
      startdate=character(),
      enddate=character(),
      featureid=character(),
      modified=character(),
      entity_type=character(),
      bundle=character(),
      varid=character(),
      uid=character(),
      vid=character(),
      status=character(),
      module=character(),
      field_dh_matrix=character(),
      stringsAsFactors=FALSE
    ) 
  )
)

RomFeature <- R6Class(
  "RomFeature",
  public = list(
    name = NULL,
    code = NULL,
    ftype = NULL,
    id = NULL,
    bundle = NULL,
    fstatus = NULL,
    properties = data.frame(),
    description = NULL,
    mps = list(),
    propdb = NULL,
    tsdb = NULL,
    initialize = function(propdb, tsdb, name = NA, bundle = NA) {
      self$name <- name
      self$bundle <- bundle
      stopifnot(is.character(name), length(name) == 1)
      stopifnot(is.character(bundle), length(bundle) == 1)
      #col.names(self$properties <-
    },
    set_prop = function(name, value, format='default') {
      # @tbd: can this even be done on a local object field? 
      # it may not be necessary, we may only implement 
      # calls to set RomProperties linked to it?
      # self$hair <- val
    },
    add_mp = function (thiscomp) {
      j = length(self$mps) + 1
      #self$mps[j] <- thiscomp
      self$mps[j] <- list('obj' = thiscomp)
    }
  )
)


