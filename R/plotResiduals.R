#' Residuals Plot for Coxian Distribution
#'
#' Plots the residuals of the observed data against the expected values from the Coxian distribution.
#'
#' @param data A numeric vector of observed data points.
#' @param lambda A numeric vector of lambda values for the Coxian distribution.
#' @param mu A numeric vector of mu values for the Coxian distribution.
#'
#' @return A plot showing the residuals.
#'
#' @examples
#' data_sample <- rexp(100, rate = 0.5)
#' lambda <- c(0.5, 0)
#' mu <- c(0.2, 0.4)
#' plotResiduals(data_sample, lambda, mu)
#'
#' @export
plotResiduals <- function(data, lambda, mu) {
  expected_values <- sapply(data, function(x) coxian_pdf(x, lambda, mu))
  residuals <- data - expected_values
  plot(data, residuals, main = "Residuals Plot", xlab = "Data", ylab = "Residuals")
  abline(h = 0, col = "red")
}
