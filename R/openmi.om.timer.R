#' The base time-keeping class for simulation control
#' @description Class providing the minimum attributes and methods for the model
#'   time keeping object
#' @details Has standard methods for iterating through timesteps and connecting
#'   with other components
#' @importFrom R6 R6Class
#' @return R6 class of type openmi.om.timer
#' @import lubridate
#' @export openmi.om.timer
openmi.om.timer <- R6Class(
  "openmi.om.timer",
  public = list(
    #' @field starttime beginning of simulation
    starttime = NA,
    #' @field endtime end of simulation
    endtime = NA,
    #' @field thistime current simulation time
    thistime = NA,
    #' @field status is this timer running, paused, finished
    status = NA,
    #' @field mo current simulation month
    mo = NA,
    #' @field da current simulation day
    da = NA,
    #' @field yr current simulation year
    yr = NA,
    #' @field tz current timezone
    tz = NA,
    #' @field dt time step
    dt = NA, # time step increment in seconds,
    #' @description advance timer one timestep based on dt field and update data
    #' @return NULL
    update = function() {
      if (length(self$thistime) == 0) {
        self$thistime <- self$starttime
        self$status <- 'running'
      }
      #thistime <- thistime + duration(dt, "seconds")
      self$thistime <- self$thistime + lubridate::seconds(self$dt)
      self$mo <- as.integer(format(self$thistime,'%m'))
      self$da <- as.integer(format(self$thistime,'%d'))
      self$yr <- as.integer(format(self$thistime,'%Y'))
      if (self$thistime > self$endtime) {
        self$status <- 'finished'
      }
    },
    #' @description initialize for a model run setting fields based on thistime
    #'   and dt fields, likely passed by parent objects
    #' @return NULL
    init = function() {
      if (length(self$dt) == 0) {
        self$dt <- 86400
      }
      self$thistime <- self$starttime
      self$mo <- as.integer(format(self$thistime,'%m'))
      self$da <- as.integer(format(self$thistime,'%d'))
      self$yr <- as.integer(format(self$thistime,'%Y'))
      self$status <- 'initialized'
    }
  )
)
