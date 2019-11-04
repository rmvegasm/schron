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
    is.numeric(x$year)
  )
  len <- c(
    length(x$name)   == 1L,
    length(x$extent) == 1L,
    length(x$year)   == 1L
  )
  tbl <- c(
    all(c('age', 'error', 'depth') %in% names(x$dates)),
    all(c('base_depth', 'top_depth', 'event_id') %in% names(x$events)),
    with(x$events, all(base_depth - top_depth > 0L))
  )
  msg <- c(
    '`dates` must be a tibble, or coersible to one',
    '`events` must be a tibble, or coersible to one',
    '`name` must be a character',
    '`extent` must be numeric',
    '`year` must be numeric',
    '`name` must be of length 1',
    '`extent` must be of length 1',
    '`year` must be of length 1',
    '`dates` must have at least `age`, `error` and `depth` columns',
    '`events` must have at least `base_depth`, `top_depth` and `event_id` columns',
    'in `events`: found inconsistent base and top depths for some event layers'
  )
  cond <- c(cls, len, tbl)
  if (all(cond)) return(TRUE)
  stop(paste('\n', c('invalid core:', msg[!cond])))
}
