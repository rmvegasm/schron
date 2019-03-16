## -- Constructor function ---------------------------------
pmf <- function (x = NULL, value, prob) {
  if (!is.null(x)) {
    x <- as.integer(x)
    value <- do.call('seq.int', as.list(range(x)))
    count <- integer()
    for (i in seq_along(value)) count[i] <- sum(as.integer(value[i] == x))
    prob  <- normalize(count)
  }
  out <- structure(
    list(
      value = value,
      prob  = prob
    ),
    class = 'pmf'
  )
  out <- init(out)
  valid(out)
  out
}
## -- Validate function ------------------------------------
valid.pmf <- function (x) {
  len <- length(x$value) == length(x$prob)
  sum <- sum(x$prob) == 1
  srt <- !is.unsorted(x$value)
  if (len && sum && srt) return(TRUE)
  c(
    if (!len) 'lengths of "value" and "probs" differ',
    if (!sum) '"probs" should add up to 1',
    if (!srt) '"values" are unsorted'
  ) 
}
## -- Initialize function ----------------------------------
init.pmf <- function (x) {
  x %<>% sort() %>% normalize()
}
## -- Other methods ----------------------------------------
normalize.pmf <- function (x) {
  x$prob <- normalize(x$prob)
  x
}
sort.pmf <- function (x, ...) {
  ord <- order(x$value, ...)
  x$value <- sort(x$value, ...)
  x$prob  <- x$prob[ord]
  x
}
plot.pmf <- function (x, pretty = TRUE, ...) {
  if (pretty) {
	  nb     <- with(x, nclass.Sturges(value))
	  breaks <- with(x, pretty(value, n = nb))
	  eqdist <- unique(diff(breaks))
	  stopifnot(length(eqdist) == 1L)
	  mids   <- breaks[-length(breaks)] + diff(breaks) / 2
	  pooled <- numeric()
	  for (i in seq.int(mids)) {
	    them      <- with(x, value >= breaks[i] & value < breaks[i + 1L])
	    pooled[i] <- with(x, sum(prob[them]))
	  }
	  x <- pmf(value = mids, prob = pooled)
  } else {
    eqdist <- min(unique(with(x, diff(value))))
    stopifnot(length(eqdist) == 1L)
  }
  with(x, plot(value, prob, type = 'n', bty = 'n', ...))
  with(x, rect(
    xleft   = value - eqdist / 2,
    ybottom = 0,
    xright  = value + eqdist / 2,
    ytop    = prob,
    ...
    )
  )
  invisible()
}
hdr.pmf <- function (x, ...) {
  with(x, hdr(value, prob, ...))
}
