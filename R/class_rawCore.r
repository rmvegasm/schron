#' @title A class to represent segmented cores from a series of boreholes
#'
#' @description Class `rawCore` holds a core in the form that it is usually
#' recovered when using a 'piston corer' or similiar tools that retrieve the
#' core piecewice. It represents the core as a series of `subCore`s, with an
#' internal depth scale, and provides methods to build a `core` object with a
#' composite depth scale for further analysis.
#'
#' @slot core A length one character string with the name of the core. Should
#' match the `core` slot at any embeded `subCore` object.
#'
#' @slot boreholes A character vector with the names of the boreholes, or
#' something that can be coerced to.
#'
#' @slot subCores A list of objects of class `subCore`.
#' 
#' @family core data classes

setClass('rawCore',
  slots = c(
    core = 'character',
    boreholes = 'character',
    subCores = 'list'
  )
)
