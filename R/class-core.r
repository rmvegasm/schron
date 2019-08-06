## -- Constructor function ---------------------------------
core <- function (name, dates, events, extent, year) {
  out <- structure(
    list(
      dates  = tibble::as_tibble(dates),
      events = tibble::as_tibble(events),
      name   = name,
      extent = extent,
      year   = year
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
  msg <- c(
    '`dates` should be a tibble, or coersible to one',
    '`events` should be a tibble, or coersible to one',
    '`name` should be a character',
    '`extent` should be numeric',
    '`year` should be an integer'
  )
  cond <- c(cls, len)
  if (all(cond)) return(TRUE)
  stop(paste('\n', c('invalid core:', msg[!cond])))
}
