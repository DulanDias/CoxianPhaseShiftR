#' Akaike Information Criterion for Coxian Distribution
#'
#' Computes the Akaike Information Criterion (AIC) for a given set of data and Coxian distribution parameters.
#'
#' @param data A numeric vector of observed data points.
#' @param lambda A numeric vector of lambda values for the Coxian distribution.
#' @param mu A numeric vector of mu values for the Coxian distribution.
#'
#' @return A numeric value representing the AIC.
#'
#' @examples
#' data_sample <- rexp(100, rate = 0.5)
#' lambda <- c(0.5, 0)
#' mu <- c(0.2, 0.4)
#' coxianAic(data_sample, lambda, mu)
#'
#' @export
coxianAic <- function(data, lambda, mu) {
  log_likelihood <- sum(sapply(data, function(x) log(coxianPdf(x, lambda, mu))))
  k <- length(lambda) + length(mu)  # number of parameters
  -2 * log_likelihood + 2 * k
}
