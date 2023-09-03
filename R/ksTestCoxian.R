#' Kolmogorov-Smirnov Test for Coxian Distribution
#'
#' Performs the Kolmogorov-Smirnov test on data to compare it with a Coxian distribution.
#'
#' @param data A numeric vector of observed data points.
#' @param lambda A numeric vector of lambda values for the Coxian distribution.
#' @param mu A numeric vector of mu values for the Coxian distribution.
#'
#' @return A list with class "htest" representing the test results.
#'
#' @examples
#' data_sample <- rexp(100, rate = 0.5)
#' lambda <- c(0.5, 0)
#' mu <- c(0.2, 0.4)
#' ksTestCoxian(data_sample, lambda, mu)
#'
#' @export
ksTestCoxian <- function(data, lambda, mu) {
  coxian_cdf_values <- sapply(data, function(x) coxianCdf(x, lambda, mu))
  ks.test(data, "punif", min(coxian_cdf_values), max(coxian_cdf_values))
}
