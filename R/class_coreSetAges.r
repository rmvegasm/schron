#' @family core data classes
#' @include class_coreData.r

setClass('coreSetAges',
  slots = c(
	  depth    = 'numeric',
    age      = 'numeric',
    error    = 'numeric',
    labID    = 'character',
    sampleID = 'character',
    core     = 'character'
	),
  contains = 'coreData'
)
