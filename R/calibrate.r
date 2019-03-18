#' @title Calibrate 14C age determinations
#'
#' @param age a numeric vector containing the mean 14C age for the samples.
#'
#' @param error a numeric vector containing the error for each 14C
#' determination.
#'
#' @param curve a string, currently only `"SHcal13.txt"` implemented.
#'
#' @param eps the smallest probability to be considered as something. Values
#' under this number will not be included in the `pmf` output.
#'
#' @return a list of `pmf` objects.
#' @export
calibrate <- function (age, error, curve = 'SHcal13.txt', eps = 1e-5) {
  curve <- read_curve(curve)
  cal_one <- function (age, error) {
    probs <- with(curve, exp(-(age - rc_age)^2 / (error^2 + rc_err^2)) / sqrt(error^2 + rc_err^2))
    probs <- normalize(probs)
    who <- probs > eps
    pmf(value = curve[['year']][who], prob = probs[who])
  }
  if (length(age) != length(error)) stop('age and error lengths differ')
  n <- length(age)
  cal <- list()
  for (i in 1:n) cal[[i]] <- cal_one(age[i], error[i])
  cal
}
read_curve <- function(file) {
  path <- system.file('extdata', file, package = 'schron', mustWork = TRUE)
  curve <- read.table(path, sep = ',', col.names = c('cal', 'rc_age', 'rc_err',
    'delta', 'sigma'))[, 1:3]
  year <- 50000L:0L
  toyear <- function (x) with(curve, approx(cal, x, xout = year)$y)
  rc_age <- toyear(curve$rc_age)
  rc_err <- toyear(curve$rc_err)
  list(year = year, rc_age = rc_age, rc_err = rc_err)
}
