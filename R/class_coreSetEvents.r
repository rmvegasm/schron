#' @family core data classes 
#' @include class_coreData.r

setClass('coreSetEvents',
  slots = c(
	  depth   = 'numeric',
    base    = 'numeric',
    top     = 'numeric',
    type    = 'character',
    eventID = 'character',
    core    = 'character'
	),
  contains = 'coreData'
)

