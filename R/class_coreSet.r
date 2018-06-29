#' @title A class to represent a set of related sedimentary cores
#'
#' @include class_coreData.r class_coreSetAges.r class_coreSetEvents.r class_coreSetHiatus.r class_coreSetInfo.r

coreSet <- setClass('coreSet',
  slots = c(
  	ages   = 'coreSetAges',
    events = 'coreSetEvents',
    hiatus = 'coreSetHiatus',
    info   = 'coreSetInfo'
	)
)
