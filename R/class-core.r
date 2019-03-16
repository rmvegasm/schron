## -- Constructor function ---------------------------------
core <- function (name, dates, events, extent, year) {
  out <- structure(
    list(
      dates  = tibble::as.tibble(dates),
      events = tibble::as.tibble(events)
      name   = name,
      extent = extent,
      year   = year,
    ),
    class  = 'core')
  if (valid(out)) out
}
## -- Validate function ------------------------------------
valid.core <- function (x) {
  cls <- c(
    with(x, c(
      tibble::is.tibble(dates),
      tibble::is.tibble(events)
      )
    ),
    is.character(x$name),
    is.numeric(x$extent),
    is.integer(x$year)
  )
  len <- c(
    length(x$name)   == 1L,
    length(x$extent) == 1L,
    length(x$year)   == 1L
  )
  if (all(c(cls,len))) return(TRUE)
  stop('invalid core... i know i should tell you why...')
}
