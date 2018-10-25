## This class is designed to hold arbitrary data in a spreadsheet-like
## style within core classes. Is basically the same idea as with the
## 'coreData' class, but needs a separate implementation since we don't
## know the number or kind of variales that will be within.
## Data needs to be a slot of class 'list' to accept an undefined number of
## variables. For this reason we need to define validity and subsetting methods
## that closely resemble those for 'coreData', but acting within the slot
## of class 'list' instead of among slots.
##
## After a lot of thinking i haven't found a way to avoid duplication here
## and still get the desired functionality... any ideas are welcomed

#' @title A class to represent arbitrary data for a core
#'
#' @description Class `proxyData` holds arbitrary data within a spreadsheet-like
#' structure. It resembles a `data.frame` in that it behaves both as a `list`
#' and a `matrix`, and represents a series of observations (rows) of one or more
#' variables (columns).
#'
#' @slot names a `character` vector giving the names for each variable
#'
#' @slot data a `list` of equal length vectors
#'
#' @include utils.r
#' @family core data classes 

setClass('proxyData',
	slots = list(
		names = 'character',
		data  = 'list')
)
setMethod('initialize',
	signature('proxyData'),
	definition = function (.Object, ...) {
		arg <- list(...)
		.Object@data <- data <- arg$data
		.Object@names <- names(data)
		validObject(.Object)
		.Object
	}
)
setValidity('proxyData',
	function (object) {
		len <- numeric()
		names <- object@names
		data <- object@data
		for (nm in names) len <- append(len, length(data[[nm]]))
		if (length(unique(len)) != 1L) return('unequal number of observations (rows) within data slot')
		TRUE
	}
)
setMethod('[',
	signature('proxyData'),
	function (x, i, j, ..., drop = TRUE) {
		.checkSingleBracketSubset(x, i, j)
		for (nm in x@names) x@data[[nm]] <- x@data[[nm]][i]
		validObject(x)
		x
	}
)
setMethod('[<-',
	signature('proxyData'),
	function (x, i, j, ..., value) {
		.checkSingleBracketSubset(x, i, j)
		if ((isS4(value) && class(x) == class(value)) || class(value) == 'data.frame') {
			value <- as(value, 'list', strict = FALSE) # preserve names
		}
		for (nm in x@names) x@data[[nm]][i] <- value[[nm]]
		validObject(x)
		x
	}
)
setAs(from = 'proxyData', to = 'list',
	def = function (from) {
		from@data
	}
)
setAs(from = 'proxyData', to = 'data.frame',
	def = function (from) {
		data.frame(from@data, stringsAsFators = FALSE)
	}
)
setMethod('length',
	signature(x = 'proxyData'),
	function (x) {
		length(x@data[[1L]])
	}
)
setMethod('dim',
	signature(x = 'proxyData'),
	function (x) {
		rows <- length(x)
		cols <- length(x@names)
		c(rows, cols)
	}
)
