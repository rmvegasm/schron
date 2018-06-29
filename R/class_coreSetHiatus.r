#' @family core data classes 
#' @include class_coreData.r

setClass('coreSetHiatus',
  slots = c(
	  depth = 'numeric',
    core  = 'character'
	),
  contains = 'coreData'
)
