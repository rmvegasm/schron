#' @title Check for allowed subset types for subclasses of [coreData-class].
#'
#' @description
#'
#' This functions are for internal use. They are called within the `[`,  `[<-`,
#' `[[` and `[[<-` methods for [coreData-class],  check that the subsets are
#' appropriate and throw an informative error if they are not.
#'
#' @param x An object of any of the core data classes.
#'
#' @param i An integer vector or a logical vector of length equal to `length(x)`
#' giving the rows to be subsetted.
#'
#' @param j An integer vector, a logical vector of length equal to
#' `length(slotNames(x))` or a character string giving the columns to be
#' subsetted.
#'
#' @param ... A series of named values, of the form `field = value`. See details
#' below.
#' 
#' @details
#'
#' `[` and `[<-` allow a single subset vector, wich can be a `numeric` of length
#' not greater than the object's length, or a `logical`
#' of length equal to the object's length. Alternatively named arguments can be
#' used, where each name should match one of the object's slot names, and the
#' value assigned to such name should be of the same class as the corresponding
#' slot.
#' 
#' `[[` and `[[<-` allow for two subset vectors, for rows and columns
#' respectively. Constraints for row subsetting are identical to those of `[`.
#' Columns are subsettable by `character`, provided that every element within
#' the subset vector matches some slot name within the object.
#' 
#' `checkRows` and `checkCols` do most of the testing. If successful, they
#' assign a boolean `rows` and `cols` in the calling frame, that
#' controls if rows, columns or both are to be subsetted.
#' 
#' `checkOther` implements a simple *query* mechanism inside `[`.
#' It will capture a series of named arguments that should have the form
#' `slotName = value`. Every row within the corresponding `slot` that matches
#' anything within `value` will be subsetted.
#' This is equivalent to writting inside `[` something like
#' ```
#' other <- list(...)
#' index <- !logical(length(x))    ## all TRUE
#' for (sl in names(other)) {
#'   who <- match(slot(x, sl), other[[sl]], nomatch = 0) > 0
#'   index <- index & who
#' }
#' x[index]
#' ```
#' where `x` is the object to be subsetted. This mechanism cannot be used along
#' normal indices. If you provide both, named arguments will be ignored with a
#' warning.
#'
#' @return `invisible()` if successful, exits with an error otherwise.
#' `checkRows` and `checkCols` have the side effect of assigning a boolean within
#' the `parent.frame(2)`. `checkOther` returns `invisible()` if successful, and
#' has the side effect of assigning `i` within the calling frame.
#'
#' 
#' @name subsetCheck
#'
checkRows <- function (x, i) {
  if (!missing(i)) {
    if (anyNA(i)) stop("NAs not allowed")
    if (!(is.integer(i) || is.logical(i))) 
      stop('subsetting by ', class(i), ' not supported')
    if (is.logical(i)) {
      if (!(length(i) == length(x))) 
        stop('logical subset vector and object lengths differ') 
    }
    if (is.integer(i)) {
      if (!(length(i) <= length(x)))
        stop('subset vector length is greater than object length')
    }
		assign('rows', TRUE, envir = parent.frame(2))
  } else {assign('rows', FALSE, envir = parent.frame(2))}
  invisible()
}

#' @rdname subsetCheck
#'
checkCols <- function (x, j) {
  if (!missing(j)) {
    if (anyNA(j)) stop('NAs not allowed')
    if (!(is.integer(j) || is.logical(j) || is.character(j)))
      stop('subsetting by ', class(j), ' not supported')
    if (is.integer(j)) {
      if (!(length(j) <= dim(x)[2L]))
        stop('attempting to subset more columns than available')
      assign('j', slotNames(x)[j], envir = parent.frame(2))
    }
    if (is.logical(j)) {
      if (!(length(j) == dim(x)[2L]))
        stop('logical subset vector for columns must match the number of columns')
      assign('j', slotNames(x)[j], envir = parent.frame(2))
    }
    if (is.character(j)) {
      if (!all(j %in% slotNames(x)))
        stop('all columns subset must match a slot name')
    }
	  assign('cols', TRUE, envir = parent.frame(2))
  } else {assign('cols', FALSE, envir = parent.frame(2))}
  invisible()
}

#' @rdname subsetCheck
#'
checkSingleBracketSubset <- function (x, i, j) {
  if (missing(i) && missing(j)) stop("no dimensions given")
  if (!missing(j)) warning('not possible to subset "columns", try "[[" instead')
  checkRows(x, i)
  invisible()
}

#' @rdname subsetCheck
#'
checkDoubleBracketSubset <- function (x, i, j) {
  if (missing(i) && missing(j)) stop('no dimensions given')
  checkRows(x, i)
  checkCols(x, j)
  invisible()
}

#' @rdname subsetCheck
#'
checkOther <- function (x, i, j, ...) {
  other <- list(...)
  if (length(other) == 0L) return(invisible())
  if (!(missing(i) && missing(j))) {
    warning('other named arguments provided ammong indices, only the latter
    where used')
    return(invisible())
  }
  if (!all(names(other) %in% slotNames(x))) stop('wrong slot names')
  sst <- !logical(length(x))
  for (sl in names(other)) {
    if (class(other[[sl]]) != class(slot(x, sl)))
      stop('class of the given subset does not match the class of the slot')
    sst <- sst & (slot(x, sl) %in% other[[sl]])
  }
  assign('i', sst, envir = parent.frame())
}

