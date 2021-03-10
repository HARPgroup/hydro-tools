

RomFeature <- R6Class(
  "RomFeature",
  public = list(
    name = NULL,
    code = NULL,
    ftype = NULL,
    id = NULL,
    bundle = NULL,
    fstatus = NULL,
    # list of object references?  or list format name, value, ...
    properties = list(), 
    description = NULL,
    mps = list(),
    datasource = NULL,
    initialize = function(datasource = NULL, config) {
      self$name <- name
      self$bundle <- bundle
      stopifnot(is.character(name), length(name) == 1)
      stopifnot(is.character(bundle), length(bundle) == 1)
      #col.names(self$properties <-
    },
    to_list = function() {
      # returns as a list, which can be set and fed back to 
      # from_list() or new(config)
      t_list <- list(
        id = self$tid,
        name = self$name,
        code = self$code,
        ftype = self$ftype,
        fstatus = self$fstatus,
        bundle = self$bundle
      )
      return(t_list)
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


