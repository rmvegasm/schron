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
  if (n == 1L) return(cal_one(age, error))
  cal <- list()
  for (i in 1:n) cal[[i]] <- cal_one(age[i], error[i])
  cal
}
read_curve <- function(file) {
  curve <- read.table(file, sep = ',', col.names = c('cal', 'rc_age', 'rc_err',
    'delta', 'sigma'))[, 1:3]
  year <- 50000L:0L
  toyear <- function (x) with(curve, approx(cal, x, xout = year)$y)
  rc_age <- toyear(curve$rc_age)
  rc_err <- toyear(curve$rc_err)
  list(year = year, rc_age = rc_age, rc_err = rc_err)
}
