#' Iron(II) oxidation kinetics
#' 
#' Given temperature, pH, O2 concentrations, and ionic strength, calculate 
#' the first order reaction constant for iron(II) oxidation by oxygen. Based
#' on the equations of Millero et al. GCA. 1987.
#' @param temp Temperature in degrees C
#' @param ph pH
#' @param o2 Dissolved oxygen concentration, in umol/kg
#' @param I Ionic strength. Defaults to 0.7 (35 psu seawater)
#' @return Oxidation rate constant (kg H20 / mol Fe(II) / min)
#' @export
#' @examples feOx(temp = 20, ph = 8.2, o2 = 100)
feOx <- function(temp, ph, o2, I = 0.7){
  # temp should be in celsius
  # o2 should be in umol / kg
  tkelvin <- temp + 273.15
  poh <- 14 - ph
  concohmolar <- 10^-poh
  concoh <- concohmolar / 1.025
  logk0 <- 21.56 - 1545 / tkelvin
  logk <- logk0 - (3.29 * I^0.5) + (1.52 * I)
  conco2 <- o2 / 1e6
  k <- 10^logk
  k1 <- k * concoh^2 * conco2
}