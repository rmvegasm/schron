cont_ranges <- function (x, ...) {
  diffs  <- diff(x)
  by     <- min(diffs)
  if (all(diffs == by)) return(range(x, ...))
  breaks <- which(diffs != by)
  out  <- numeric()
  from <- 1L
  for (to in c(breaks, length(x))) {
    ssx  <- x[from:to]
    out  <- append(out, range(ssx, ...))
    from <- to + 1L
  }
  out
}
