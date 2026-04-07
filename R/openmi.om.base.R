#' The base object class for meta-model components, utilizing a openmi to
#' leverage the VA Hydro Operation Model components
#'
#' @description Class providing the minimum attributes and methods for a model
#'   component of the VA Hydro OM
#' @details Has standard methods for iterating through timesteps and connecting
#'   with other model components
#' @importFrom R6 R6Class
#' @seealso https://doi.org/10.1016/j.envsoft.2018.07.015
#' @export openmi.om.base
#' @examples NA
#' @include openmi.om.timer.R
#' @return R6 class of type openmi.om.base
openmi.om.base <- R6Class(
  'openmi.om.base',
  public = list(
    #' @field name A unique identifier for this controller
    name = NA,
    #' @field code A unique identifier for this controller
    code = NA,
    #' @field debug mode on/off
    debug = FALSE,
    #' @field value The value at the current instance
    value = NA,
    #' @field data Data from the larger context, including the parent. was arData in original om
    data = NA,
    #' @field state This objects local state. was state in om
    state = list(),
    #' @field inputs Inputs linked to this object
    inputs = list(),
    #' @field components Components contained by this object often equations,
    #'   matrices, etc.
    components = NA,
    #' @field host Host database name
    host = NA,
    #' @field type Type of component
    type = NA,
    #' @field compid compid is a unique identifier in this simulation domain
    compid = NA,
    #' @field vars vars is an array of variables that this requires for solving, determines op order
    vars = NA,
    #' @field timer Timer is the object keepign time in simulation (set by parent controller)
    timer = NA,
    #' @field id id is identifier (compid, name duplicates?)
    id = NA,
    #' @description Create an openmi.om.base property with name or host
    #' @param elem_list Which properties to set on creation?
    #' @param format Format of elem_list
    #' @return R6 class object with populated name or host fields
    initialize = function(elem_list = list(), format = 'raw'){
      for (i in names(elem_list)) {
        self$set_prop(as.character(i), elem_list[[i]], format)
      }
    },
    #' @description What fields may be set on this object?
    #' @return array of settable properties
    settable = function() {
      return(c('name', 'host'))
    },
    #' @description Sets a property value on this object
    #' @param propname which attribute
    #' @param propvalue what value
    #' @param format The intended format of propvalue
    #' @return Nothing, but sets host or name via \code{openmi.om.base$set_sub_prop()}
    set_prop = function(propname, propvalue, format = 'raw') {
      if (format == 'openmi') {
        propvalue = self$parse_openmi(propvalue)
      }
      # is it allowed to be set?
      if (propname == 'name') {
        self$name <- propvalue
      }
      if (propname == 'host') {
        self$host <- propvalue
      }
      self$set_sub_prop(propname, propvalue)
    },
    #' @description
        #' Derives the value from an openmi formatted property list to find
        #' relevant modelling info based on model object type
    #' @param propvalue openmi formatted property list
    #' @return a settable data value from openmi json type data description
    parse_openmi = function(propvalue) {
      # check for special handlers needed
      # all openMI formatted will have a object_class and value field
      # however, this format can include both flat values and arrays,
      # so detect flat value and return if found
      if (is.null(names(propvalue))) {
        return(propvalue)
      }
      if (!is.na(match('object_class', names(propvalue)))) {
        object_class = as.character(propvalue[['object_class']])
      } else {
        # assume it's textField
        object_class = 'textField'
      }
      if (is.na(match('value', names(propvalue)))) {
        propvalue = NULL
      } else {
        if (object_class == 'textField') {
          propvalue <- as.character(propvalue[['value']])
        } else {
          # TBD: handle other types like data matrix in child classes
          propvalue <- self$parse_class_specific(propvalue)
        }
      }
      return(propvalue)
    },
    #' @description Method to allow special parsing of model property values
    #' @param propvalue from some custom classformat reading implementation
    #' @return a settable data value
    parse_class_specific = function(propvalue) {
      # special handlers for each class go here
      propvalue <- as.character(propvalue[['value']])
      return(propvalue)
    },
    #' @description set the value of a contained component (not a local class attribute)
    #' @param propname which attribute
    #' @param propvalue what value
    #' @param format of propvalue
    #' @return a settable data value
    set_sub_prop = function(propname, propvalue, format = 'raw') {
      if (!is.na(match(propname, names(self$components)))) {
        # pass to the component
        obj <- self$components[propname]
        obj$set_prop(propname, propvalue, format)
      }
    },
    #' @description initialize this component after first object creation
    #' @return Nothing, but will run \code{self$set_vars()} to populate fields
    #'   and will initialize each component (using the \code{init()} method on
    #'   each component)
    init = function(){
      if (length(self$debug) == 0) {
        self$debug <- FALSE
      }
      # init() in OM php
      # if we have no components, this will be NA,
      # if we *do* have them, then it becomes a list so we iterate thru them
      if (typeof(self$components) == 'list') {
        if (length(self$components) > 0) {
          for (i in 1:length(self$components)) {
            self$components[[i]]$init()
          }
        }
      }
      self$set_vars()
    },
    #' @description set_vars finds all the input var names for this function
    #' @return NULL
    set_vars = function() {
      # populates the vars array
      # may be subclassed
      # TBD: get any inputs used by this for better stand0alone object ordering
      #      note: standalone ordering is NOT available in original OM php
      #      maybe this is better handled separately since there are object refs tied to inputs?
      self$vars <- c()
    },
    #' @description log_debug handles debug info
    #' @param debug_mesg message to add to logger
    #' @return NULL
    log_debug = function(debug_mesg) {
      # TBD
    },
    #' @description get all input values from linked components and internal
    #'   timeseried feeds
    #' @return Nothing, but sets data fields
    # added to base specification
    getInputs = function () {
      # get data from related objects or internal timeseries feeds
      # @todo: determine if we should clear data array at time begin
      # store in internal "data" list
      self$data$mo <- self$timer$mo
      self$data$da <- self$timer$da
      self$data$yr <- self$timer$yr
      self$data$thistime <- self$timer$thistime
      if (length(names(self$inputs)) > 0) {
        nms = names(self$inputs)
        for (i in 1:length(nms)) {
          i_name = nms[i]
          for (j in 1:length(self$inputs[i_name])) {
            input = self$inputs[[i_name]][[j]]
            #print(input)
            i_object = input$object
            r_name = input$remote_name
            i_type = input$input_type
            if (length(r_name) > 0) {
              i_value = i_object$getValue(r_name)
            } else {
              i_value = i_object$getValue()
            }
            if (i_type == 'numeric') {
              if (j == 1) {
                # nullify on initial
                self$data[i_name] <- 0
              }
              self$data[i_name] <- self$data[[i_name]] + as.numeric(i_value)
              #data[i_name] <- i_value
              if (self$debug) {
                print(paste("obtained and converted input", i_name, i_value,sep='='))
              }
            }
          }
        }
      }
    },
    #' @description execute things to do before model timestep execution
    #' @return Nothing, but runs \code{self$getInputs()}
    prepare = function(){
      # preStep() in OM php
      self$getInputs()
    },
    #' @description perform model timestep execution via \code{self$prepare()},
    #' \code{self$stepChildren()}, \code{self$update()}, \code{self$finish()},
    #'  \code{self$logState()}
    #' @return Nothing, but runs various state methods
    step = function(){
      # prepare = preStep() in OM php
      self$prepare()
      self$stepChildren()
      self$update()
      self$finish()
      self$logState()
    },
    #' @description execute child model timestep code by calling \code{step()}
    #'   for each component opn this object
    #' @return NULL
    stepChildren = function(){
      # evaluate() in OM php
      if (is.list(self$components)) {
        for (i in 1:length(self$components)) {
          #message(paste("Calling update() on ", i))
          self$components[[i]]$step()
        }
      } else {
        #message(paste("components on object", self$name, "is.na"))
      }
    },
    #' @description execute model timestep code
    #' @return NULL
    update = function(){
      # evaluate() in OM php
    },
    #' @description do things at end of model step and run for each component
    #' @return NULL
    finish = function(){
      # postStep() in OM php
      if (is.list(self$components)) {
        if (length(self$components) > 0) {
          for (i in 1:length(self$components)) {
            self$components[[i]]$finish()
          }
        }
      }
    },
    #' @description Is this object valid?
    #' @return logical TRUE/FALSE
    validate = function(){
      
    },
    #' @description log data at end of timestep
    #' @return NULL
    logState = function () {
      # logState() in OM php
      
    },
    #' @description get value of this object currently
    #' @param name a specific name from the state array, not just the default. TBD
    #' @return value
    getValue = function(name = "value"){
      # Check data array first
      if (name %in% names(self$data)) {
        return(self$data[name])
      }
      # Defaults to simple case where object only has one possible value
      return(self$value)
    },
    #' @description connect an input to this component
    #' @param local_name name that will be referred to in local contest
    #' @param object is the actual R6 class to connect to
    #' @param remote_name is what property on the remote object are we accessing
    #' @param input_type is this is a number (most common), text or other?
    #' @return Sets inputs field
    addInput = function(
    local_name = character(),
    object = openmi.om.base,
    remote_name = '',
    input_type = 'numeric'
    ){
      # adds inputs named as "localname"
      # a given localname input may have multiple inputs
      # so they are stored as a nested list
      if (is.null(self$inputs[[local_name]])) {
        self$inputs[[local_name]] <- list()
        print(paste("adding", local_name, self$inputs[local_name]))
      }
      iid = length(self$inputs[[local_name]]) + 1
      self$inputs[[local_name]][iid] <- list(
        input = list(
          local_name = local_name,
          object = object,
          remote_name = remote_name,
          input_type = input_type
        )
      )
    },
    #' @description add a contained sub-component (i.e. not linked)
    #' @param thiscomp an R6 classof open.mi type
    #' @return value
    addComponent = function (thiscomp = openmi.om.base) {
      # this is deprecated in favor of the naming convention _
      return(self$add_component(thiscomp))
    },
    #' @description add a contained sub-component (i.e. not linked)
    #' @param thiscomp an R6 classof open.mi type
    #' @details components are small objects that reside inside this object
    #'these *were* called processors in the old version of the model other
    #'models are handled exclusively by the runtime controller/container and
    #'they *may be* known as model_entities -- or they may *also* be known as
    #'components: is there any reason to have a separate concept of components
    #'and sub_components? run hierarchy?
    #' @return NULL
    add_component = function (thiscomp = openmi.om.base) {
      compid = self$get_component_id(thiscomp)
      message(paste("Created compid =", compid))
      thiscomp$compid <- compid
      thiscomp$timer = self$timer
      if (!is.list(self$components)) {
        self$components = list()
      }
      self$components[compid] <- list('object' = thiscomp)
    },
    #' @description return unique ID of component
    #' @param thiscomp an R6 classof open.mi type
    #' @return integer
    get_component_id = function(thiscomp) {
      
      # we can add this with numeric indices if we like
      # however, we must have a way of linking objects, which
      # requires a persistent name, i.e. a sort of DOI
      # format: host:type:id, examples:
      #   localhost:feature:647 (well 647),
      #   localhost:component:1991 (the withdrawal amt)
      # Input/Link format:
      #   localname:host:type:id:[remote name]
      #   - if property name is null then just use getValue() without parameter
      #print(thiscomp$compid)
      
      # we may need to use this for model controllers, but for now we just return the name
      return(as.character(thiscomp$name))
      
      # tbd: do we need this for *regular* models, not containers?
      if (length(thiscomp$host) == 0) {
        thiscomp$host = 'localhost'
      }
      if (length(thiscomp$type) == 0) {
        thiscomp$type = 'unknown'
      }
      if (length(thiscomp$id) == 0) {
        if (!is.list(self$components)) {
          cid = 1
        } else {
          cid = length(self$components) + 1
        }
        thiscomp$id = paste('local', cid, sep='');
      }
      thiscomp$compid = paste(thiscomp$name, thiscomp$host,thiscomp$type,thiscomp$id, sep=":")
      return(thiscomp)
    },
    #' @description order contained sub-components
    #' @return NULL
    orderOperations = function () {
      # sets basic hierarchy of execution by re-ordering the components list
    },
    #' @description format this object as openmi json
    #' @return json text
    asJSON = function () {
      #TBD
    }
  )
)

