#' Determine an oxidation constant
#'
#' Typically, first order reaction constants are estimated by log
#' transforming the data, and then using linear regression. Unfortunately,
#' this biases the result in favor of lower concentration samples. This
#' function fits a first order reaction curve to the concentration data
#' directly, solving this problem
#' @param t time points of the experiment (vector)
#' @param c concentrations at the time points (vector of same size)
#' @return A first order constant
#' @export
#' @examples
#' k <- optimKinetics(t, c)

optimKinetics <- function(t, c){
  # given a set of time points and paired concentration data, fit a least squares line,
  # assuming first order kinetics
  # t and c should be vectors of the same size
  # EXAMPLE
  # t <- 1:10
  # c <- 5 * exp(-.2 * t) + rnorm(length(t), sd = .05)
  # optim <- optim_kinetics(t, c)
  # print(optim)
  logc <- log(c)
  guess_coefs <- coef(lm(logc ~ t))
  kin_error <- function(coefs, t, c){
    pred_c <- exp(coefs[1]) * exp(coefs[2] * t)
    sum((pred_c - c)^2)
  }
  fit <- nlm(kin_error, guess_coefs, t = t, c = c)$estimate
  fit[1] <- exp(fit[1])
  fit
}

