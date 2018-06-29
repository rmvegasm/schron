#' @title A class to represent `data.frame`-like objects within cores
#' 
#' @description
#' This is a `VIRTUAL` class implementing spreadsheet-like behaviour that
#' emulates *some* of the `data.frame` functionality.
#' 
#' @details
#' Objects belonging to subclases of `coreData` must have equal length vectors
#' within slots, representing variables along a series of observations. This allows
#' to define formal subclasses that inherit this basic structure, being specific
#' about the type of data they should hold. Methods allow to subset these objects
#' by rows and columns, and to coerce them to `data.frame` or `list` objects if
#' needed. `[` method allows a single argument and subsets by rows, returning an
#' object of the same class as the one that is being subsetted. `[[` method allows
#' subsetting by rows and columns, returning a named list. Objects can be created
#' from a single `data.frame` or `list` if names match the corresponding slot
#' names. 
#'
#' @family core data classes 
#' @include utils.r

setClass('coreData')
setValidity('coreData',
  function (object) {
    len <- numeric(0L)
    for (i in slotNames(object)) len <- append(len, length(slot(object, i)))
     if (length(unique(len)) != 1L) return('unequal number of observations (rows)')
    TRUE
  }
)
setMethod("initialize",
  signature(.Object = "coreData"),
  function (.Object, ...) {
    arg <- list(...)
    Class <- class(.Object)
    classDef <- getClass(Class)
    if (length(arg) == 1L) {
      sarg <- arg[[1L]]
      if (is.data.frame(sarg)) sarg <- as.list(sarg)
      if (is.list(sarg)) {
        provides <- names(sarg)
        expects <- slotNames(classDef)
        if (!all(provides %in% expects)) warning('there were unused variables')
        pass <- sarg[match(expects, provides)]
        narg <- c(Class, pass)
        return(do.call(new, narg))
      }
    }
    callNextMethod()
  }
)
setMethod("[",
  signature(x = "coreData"),
  function (x, i, j, ..., drop = TRUE) {
    checkOther(x, i, j, ...)
    checkSingleBracketSubset(x, i, j)
    for (sl in slotNames(x)) slot(x, sl) <- slot(x, sl)[i]
    x
  }
)
setMethod("[<-",
  signature(x = "coreData"),
  function (x, i, j, ..., value) {
    checkOther(x, i, j, ...)
    checkSingleBracketSubset(x, i ,j)
    if (!(class(value) %in% c(class(x), 'data.frame', 'list')))
      stop('the replacement should be an object of class "', class(x),
        '" , "data.frame" or "list"')
    if (!(class(x) == class(value))) value <- new(class(x), value)
    for (sl in slotNames(x)) slot(x, sl)[i] <- slot(value, sl)
    x
  }
)
setMethod("[[",
  signature(x = "coreData"),
  function (x, i, j, ...) {
    checkDoubleBracketSubset(x, i, j)
    if (rows && cols) {
      x <- as(x[i], 'list')[[j]]
    } else {
      if (rows) x <- as(x[i], 'list')
      if (cols) x <- as(x, 'list')[[j]]
    }
    x
  }
)
setMethod("[[<-",
  signature(x = "coreData"),
  function (x, i, j, ..., value) {
    checkDoubleBracketSubset(x, i, j)
    if (rows && cols) {
      for (sl in j) slot(x, sl)[i] <- value[[j]]
    } else {
      if (rows) for (sl in slotNames(x)) slot(x, sl)[i] <- value[[sl]]
      if (cols) for (sl in j) slot(x, sl) <- value[[sl]]
    }
    x
  }
)
setAs(from = 'coreData', to = 'list',
  def = function (from) {
    x <- list()
    for (sl in slotNames(from)) {
      x[[sl]] <- slot(from, sl)
    }
    x
  }
)
setAs(from = 'coreData', to = 'data.frame',
  def = function (from) {
    data.frame(as(from, 'list'), stringsAsFactors = FALSE)
  }
)
setMethod("show",
  signature(object = "coreData"),
  function (object) {
    cat('An object of class ', dQuote(class(object)), '\n')
    print(as(object, 'data.frame'), row.names = FALSE)
  }
)
setMethod("length",
  signature(x = "coreData"),
  function (x) {
    sl <- slotNames(x)
    rows <- length(slot(x, sl[1L]))
    rows
  }
)
setMethod("dim",
  signature(x = "coreData"),
  function (x) {
    sl <- slotNames(x)
    rows <- length(x)
    cols <- length(sl)
    c(rows,cols)
  }
)
