valid <- function (x, ...) UseMethod('valid')

init <- function (x, ...) UseMethod('init')

normalize <- function (x, ...) UseMethod('normalize')
normalize.default <- function (x) x / sum(x)

hdr <- function (x, ...) UseMethod('hdr')
hdr.default <- function (value, prob, int = NULL) {
  if (is.null(int)) int <- c(95, 99)
  int  <- unique(int)
  spr  <- sort(prob, decreasing = TRUE)
  cspr <- cumsum(spr)
  sval <- value[order(prob, decreasing = TRUE)]
  out  <- list()
  for (i in int) {
    nm        <- as.character(i)
    ssval     <- sort(sval[cspr <= i / 100])
    out[[nm]] <- cont_ranges(ssval)
  }
  out
}
