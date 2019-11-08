#' Calculate potential density
#'
#' Given the temperature and salinity of a seawater sample, calculate what
#' its potential density would be at sea level. Based on an approximation to
#' the UNESCO 1983 (EOS 80) polynomial.
#' @param t temperature in deg C
#' @param s salinity, in absolute salinity units
#' @return potential density in units of kg/m^3
#' @export
#' @examples sigmaTheta(t, s)

sigmaTheta <- function(t, s){
  # calculate potential density based on temperature and salinity
  # t: temperature (deg C)
  # s: salinity (absolute salinity units)
  # based on approximation to the UNESCO 1983 (EOS 80) polynomial
  d <- 9.979080e+02 + -5.179496e-02 * t + -6.646409e-03 * t^2 + 4.260763e-05 * t^3 +
    8.708481e-01 * s + 1.649368e-03 * s^2 + -5.392211e-05 * s^3
  d
}

