

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


