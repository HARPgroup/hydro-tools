#' The base class for matrix/lokup table meta-model components.
#' @return R6 class of type openmi.om.matrix
#' @seealso openmi.om.base
#' @export openmi.om.matrix
#' @include openmi.om.linkableComponent.R
openmi.om.matrix <- R6Class(
  "openmi.om.matrix",
  inherit = openmi.om.linkableComponent,
  public = list(
    #' @field datamatrix holds the actual table
    datamatrix = NA,
    #' @field colindex holds the column lookup variable name (ustabe keycol1)
    colindex = NA,
    #' @field rowindex holds the column lookup variable name (ustabe keycol2)
    rowindex = NA,
    #' @field coltype holds the column lookup type (ustabe lutype1)
    coltype = NA,
    #' @field rowtype holds the row lookup type (ustabe lutype2)
    rowtype = NA,
    #' @description Set propvalues and parse matrix into datamatrix field, if
    #'  applicable
    #' @param propname which attribute
    #' @param propvalue what value
    #' @param format of propvalue
    #' @return NA
    set_prop = function(propname, propvalue, format = 'raw') {
      super$set_prop(propname, propvalue, format)
      if (format == 'openmi') {
        propvalue = self$parse_openmi(propvalue)
      }
      if (propname == 'matrix') {
        self$datamatrix <- propvalue
      }
      self$set_sub_prop(propname, propvalue, format)
    },
    #' @description Custom parser
    #' @param propvalue from some custom classformat reading implementation
    #' @return a settable data value
    parse_class_specific = function(propvalue) {
      # special handlers for each class go here
      message("Called parse_class_specific on matrix()")
      print(propvalue)
      
      propvalue <- as.character(propvalue[['value']])
      return(propvalue)
    },
    #' @description update does the lookup QC
    #' @return NULL
    update = function () {
      super$update()
      # @todo: currently only returns exact match row/col lookup
      #   - add types: 1-d (row only)
      #   - add interpolation methods: interpolate, stair-step (prev value)
      # stairstep
      # Note: when using switch, the matching arg must be a string
      #       it seems that R auto converts these to make correct matches
      valmatrix <- self$datamatrix
      # @todo: evaluate all the cells in valmatrix, for now it assumes
      #        that these are all numeric
      rowmatch <- self$findMatch(valmatrix, self$data$rowindex, self$rowtype)
      # @todo:
      #   this does not yet function.  It should first find:
      #   - a full row match (or interpolation of multiple rows if app)
      #   - then derive a value from the retrieved row
      mval <- self$findMatch(rowmatch, self$data$colindex, self$coltype)
      value <- as.numeric(mval)
      code <- as.character(mval)
      if (debug) {
        print(paste(
          "Found",
          self$data$rowindex,
          self$data$colindex,
          'vaue:',
          value,
          'code',
          code,
          sep = ' '
        ))
      }
      
    },
    #' @description findMatch looks into a single dimension table using the
    #'   method ixtype provided by user
    #' @param dm array to search
    #' @param ixval key value to search for matches
    #' @param ixcol Column to search for
    #' @param ixtype what kind of lookup to perform?
    #' @return matching value (with interpolation if ixtype allows it)
    findMatch = function (dm, ixval, ixcol, ixtype = 0) {
      foundmatch = switch(
        ixtype,
        '0' = self$exactMatch(dm, ixval, ixcol),
        '1' = self$interpolate(dm, ixval, ixcol),
        '2' = self$stairStep(dm, ixval, ixcol),
        '3' = self$closest(dm, ixval, ixcol),
        # default
        self$exactMatch(dm, ixval, ixcol)
      )
      return(foundmatch)
    },
    #' @description exactMatch looks for keys and returns the appropriate row of
    #'   the matrix
    #' @param dm matrix to search
    #' @param ixval key value to search for matches
    #' @param ixcol Column to search for
    #' @return matching value
    exactMatch = function(dm, ixval, ixcol) {
      # match row & col exactly
      if(is.null(ncol(dm))){
        rval <- dm[dm == ixval]
      }else{
        rval <- dm[dm[,ixcol] == ixval]
      }
      if(length(rval) == 0){
        message(paste("No exact match is present in matrix for value", ixval))
        rval <- NULL
      }
      return(rval)
    },
    #' @description interpolate searches by key and calculates if no exact match
    #' @param dm matrix to search
    #' @param ixval key value to search for matches
    #' @param ixcol Column to search for
    #' @return interpolated value
    interpolate = function(dm, ixval, ixcol) {
      # Linearly inerpolate if no exact match is found. Repeat on each non-key
      # column of the array
      #Does an exact match already exist?
      rval <- self$exactMatch(dm, ixval, ixcol)
      
      #No exact match found, interpolate:
      if(is.null(rval)){
        if (is.null(ncol(dm))) {
          # given only a 1-column entity
          rval <- ixval
        } else {
          #Linearly interpolate each row based on the key column and value
          #provided by user
          rval <- mapply(FUN = approx, 
                         #Get a list with each column of the matrix as a vector
                         y = lapply(X = seq_len(ncol(dm)), dm = dm, FUN = function(X,dm) dm[,X]),
                         MoreArgs = list(x = dm[,ixcol], xout = ixval))
          #Simpify the interpolated row to a vector
          rval <- unlist(rval[2,])
          if(all(is.na(rval))){
            message(paste("ixval",ixval,"is outside of bounds of ixcol",ixcol,
                          "so no linear interpolation applies. Returning NULL"))
            rval <- NULL
          }else{
            #Return as named matrix as input
            rval <- matrix(rval,ncol = 3,nrow = 1,dimnames = dimnames(dm))
          }
        }
      }
      return(rval)
    },
    #' @description stairStep searches by key and select closest previous (i.e.
    #'   without exceeding)
    #' @param dm matrix to search
    #' @param ixval key value to search for matches
    #' @param ixcol Column to search for
    #' @return closest value
    stairStep = function(dm, ixval, ixcol) {
      # match nearest val that is less than or equal to ixval
      if (is.null(ncol(dm))) {
        # given only a 1-column entity
        lm <- dm
      } else {
        lm <- dm[,ixcol]
      }
      #Get the closest value without exceeding
      ixs <- (ixval - lm) >= 0
      rix <- max(which(ixs))
      if (is.null(ncol(dm))) {
        rval <- dm[rix]
      } else {
        rval <- dm[rix,]
      }
      return(rval)
    },
    #' @description closest select closest value, similar to stair step but
    #'   allows exceeding
    #' @param dm array to search
    #' @param ixval key value to search for matches
    #' @param ixcol Column to search for
    #' @return closest value
    closest = function(dm, ixval, ixcol) {
      # match row & col exactly
      if (is.null(ncol(dm))) {
        # given only a 1-column entity
        lm <- dm
      } else {
        lm <- dm[,1]
      }
      rix <- which.min(abs(lm - ixval))
      if (is.null(ncol(dm))) {
        rval <- dm[rix]
      } else {
        rval <- dm[rix,]
      }
      return(rval)
    },
    #' @description window does what???
    #' @param dm array to search
    #' @param ixval key to search for
    #' @param ixoff index offset
    #' @return calculated value
    window = function(dm, ixval, ixoff) {
      # get values prior to and after ixval, use ixoff to help guess
      if (is.null(ncol(dm))) {
        # given only a 1-column entity
        lm = dm
      } else {
        lm = dm[,1]
      }
      # get closest match to ixval
      # search for previous and next to ixval
      # create a matrix with the 3 entries before ixval, ixval, and after ixval
      # apply desired search function (stairStep, interp)
      # could use the "closest()" method below???
      # - Or is there a more convenient R func?
      six = which.min(abs(lm - ixval + ixoff)); # guess start of interval
      eix = which.min(abs(lm - ixval + ixoff)); # guess end of interval
      if (is.null(ncol(dm))) {
        rval = dm[rix]
      } else {
        rval = dm[rix,]
      }
      return(rval)
    },
    #' @description init sets up for viewing or run and parses key matrix
    #'   variables into the relevant fields on openmi.om.matrix
    #' @return NULL, but returns an instance of this matrix
    init = function () {
      super$init()
      # @todo: enable complex matching types: stair-step, interpolate
      # Case: Exact Match
      if (length(self$rowindex) == 0) {
        self$rowindex <- 1
      }
      if (length(self$colindex) == 0) {
        self$colindex <- 1
      }
      self$data['rowindex'] <- self$rowindex
      self$data['colindex'] <- self$colindex
      if (length(self$datamatrix) == 0) {
        self$datamatrix <-matrix(nrow=1,ncol=1)
      }
      if (length(self$rowtype) == 0) {
        self$rowtype <-as.integer(1)
      }
      if (length(self$coltype) == 0) {
        self$coltype <-as.integer(1)
      }
    }
  )
)
