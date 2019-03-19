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
#' @export
normalize.pmf <- function (x) {
  x$prob <- normalize(x$prob)
  x
}
#' @export
sort.pmf <- function (x, ...) {
  ord <- order(x$value, ...)
  x$value <- sort(x$value, ...)
  x$prob  <- x$prob[ord]
  x
}
#' @export
sample.pmf <- function (x, size) {
  sample(x$value, size = size, replace = TRUE, prob = x$prob)
}
#' @export
plot.pmf <- function (x, nb = NULL, density = NULL, angle = 45, col = NA,    
                      border = NULL, lty = par('lty'), lwd = par('lwd'),
                      add = FALSE, ...) {
  smpl   <- sample(x, size = 1000)
  nb     <- if (is.null(nb)) nclass.Sturges(smpl) else nb
  breaks <- pretty(smpl, n = nb)
  eqdist <- unique(diff(breaks))
  stopifnot(length(eqdist) == 1L)
  mids   <- breaks[-length(breaks)] + eqdist / 2
  pooled <- numeric()
  for (i in seq.int(mids)) {
    them      <- with(x, value >= breaks[i] & value < breaks[i + 1L])
    pooled[i] <- with(x, sum(prob[them]))
  }
  if (!add) plot(mids, pooled, type = 'n', bty = 'n', ...)
  rect(
    xleft   = mids - eqdist / 2,
    ybottom = 0,
    xright  = mids + eqdist / 2,
    ytop    = pooled,
    density, angle, col, border, lty, lwd
    )
  invisible()
}
#' @export
hdr.pmf <- function (x, ...) {
  with(x, hdr(value, prob, ...))
}
