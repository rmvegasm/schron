#' @title Validate objects within constructor functions
#'
#' @description `valid()` is a generic intended to validate objects from a class
#' within the constructor function for that class. Typical usage should be of
#' the form:
#' ```
#' constructor <- function (x, ...) {
#'   out <- structure(x, class = 'className')
#'   valid(out)
#'   out
#' }
#' ```
#' @details `valid` is a generic function with no default. For it to work you
#' need to write a method for the class, usually as part of the class
#' definition. The `valid.className` method should return `TRUE` if all
#' conditions are met, and halt with informative error messages otherwise:
#' ```
#' valid.className <- fuction (x) {
#'   conditions <- with(x, c(
#'     given == expected,
#'      .
#'      .
#'      .
#'     )
#'   )
#'   if(all(conditions)) return(TRUE)
#'   stop('Informative error message')
#' }
#'
#' @param x An object of the class to be validated.
#' 
#' @return `TRUE` if successful.
#' @export
valid <- function (x, ...) UseMethod('valid')

#' @title Initialize objects within constructor functions
#'
#' @description `init()` is a generic intended to *initialize* objects within a
#' constructor function. Usage is only meaningful as a way to isolate special
#' operations needed for a certain class from the structure of the object
#' itself. This keeps the code more readable.
#'
#' @param x An object of the class to be initialized
#'
#' @return An object of the class being initialized, as modified by the
#' `init.className` method.
#' @export
init <- function (x, ...) UseMethod('init')

#' @title Normalize a vector of probabilities
#'
#' @description Given a vector of probabilities, normalize ensures they sum up
#' to 1.
#'
#' @details This is a generic function. Methods can be written for classes
#' holding probability distributions. The default method takes a vector `x` and
#' returns `x / sum(x)`. Specific methods should `normalize` the element holding
#' the probabilities within the object, and return the modified version of that
#' object.
#'
#' @param x A numeric vector of probabilies, or an object from a class for which
#' a method has been defined.
#'
#' @return A numeric vector, or an object from a class for which a method has
#' been created.
#'
#' @export
normalize <- function (x, ...) UseMethod('normalize')
normalize.default <- function (x) x / sum(x)

#' @title Compute highest density region
#'
#' @description Computes the interval holding the desired probability density
#' for a probability distribution.
#' 
#' @details This is a generic function. The default method takes a `value` and a
#' `prob` vector and finds the range(es) within `value` holding the probability
#' density defined by `int`. 
#'
#' @export
hdr <- function (x, ...) UseMethod('hdr')

#' @describeIn hdr
#'
#' @param value A numeric vector of values for which the probability function
#' gives probabilities.
#'
#' @param prob A numeric vector of probabilities of the same length as `value`
#'
#' @param int A numeric vector describing the *size* of the desired range, as a
#' percentage. The default, `c(95, 99)`, will give the intervals that account
#' for 95% and 99% of the probability.
#'
#' @export
hdr.default <- function (value, prob, int = NULL) {
  if (is.null(int)) int <- c(95, 99)
  int  <- unique(int)
  spr  <- sort(prob, decreasing = TRUE)
  cspr <- cumsum(spr)
  sval <- value[order(prob, decreasing = TRUE)]
  out  <- list()
  for (i in int) {
    nm        <- as.character(i)
    ssval     <- sort(sval[cspr <= i / 100])
    out[[nm]] <- cont_ranges(ssval)
  }
  out
}

sample.default <- base::sample
sample <- function (x, ...) UseMethod('sample')
