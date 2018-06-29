#' @title A class to represent a core segment
#'
#' @description Class `subCore` represents a single segment from a core
#' recovered by using a 'piston corer' or any similar tool that retrieves a core
#' in a piecewise fashion.
#' 
#' @slot core,borehole A length one character vector with the name of the core
#' and the borehole. These should match the `core` slot at the enclosing
#' [`rawCore`] object, and one of the `boreholes`.
#'
#' @slot subCore A length one integer representing the sub core number. This
#' should be unique for each `subCore` within a single borehole.
#'
#' @slot topDepth A length one integer or double representing the depth from the
#' top of the sub core to the boring surface or datum.
#'
#' @slot dLength,rLength A length one integer or double providing the penetrated
#' (*drilled*) and recovered depths, respectively. These values will be used to
#' correct for compression ---or decompression--- derived from the coring
#' exercise.
#'
#' @slot An object of class [`proxyData`] with arbitrary data on a unique depth
#' scale, arranged as a *spreadsheet*.
#'
#' @include class_proxyData.r
#' @family core data classes

setClass('subCore',
  slots = c(
    core = 'character',
    borehole = 'character',
    subCore = 'integer',
    topDepth = 'numeric',
    dLength = 'numeric',
    rLength = 'numeric',
    data = 'proxyData'
  )
)
