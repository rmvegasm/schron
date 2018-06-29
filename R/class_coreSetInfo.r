#' @family core data classes 
#' @import sp
#' @include class_coreData.r

setClass('coreSetInfo',
  slots = c(
	  core     = 'character',
    location = 'SpatialPoints',
    length   = 'numeric',
    year     = 'numeric',
    citation = 'character'
	),
  contains = 'coreData'
)

