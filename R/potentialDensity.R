#' Calculate potential density
#'
#' Given the temperature and salinity of a seawater sample, calculate what
#' its potential density would be at sea level. Based on an approximation to
#' the UNESCO 1983 (EOS 80) polynomial.
#' @param t temperature in deg C
#' @param s salinity, in absolute salinity units
#' @param p pressure, in dbar
#' @return potential density in units of kg/m^3
#' @export
#' @examples sigmaTheta(t, s)

sigmaTheta <- function(t, s, p){
  # calculate potential density based on temperature and salinity
  # t: temperature (deg C)
  # s: salinity (absolute salinity units)
  # p: pressure (dbar)
  # based on approximation to the UNESCO 1983 (EOS 80) polynomial
  d <- 9.979080e+02 + -5.179496e-02 * t + -6.646409e-03 * t^2 + 4.260763e-05 * t^3 +
    8.708481e-01 * s + 1.649368e-03 * s^2 + -5.392211e-05 * s^3
  d <- 9.991570e+02 + -4.959932e-02 * t + -6.681673e-03 * t^2 +
    4.220786e-05 * t^3 + 8.813839e-01 * s + -1.853130e-03 * s^2 +
    7.917367e-06 * s^3 + 2.502144e-05 * p + -5.639820e-09 * p^2 +
    5.174855e-13 * p^3
  d
}

