#' Compute the Cumulative Distribution Function (CDF) for a Coxian Distribution
#'
#' This function calculates the CDF of a Coxian distribution for a given value of t,
#' given transition rates (lambda) and absorption rates (mu).
#'
#' @param t A numeric value where the CDF is evaluated.
#' @param lambda Numeric vector representing the transition rates.
#' The length of lambda determines the number of phases in the Coxian distribution.
#' @param mu Numeric vector representing the absorption rates for each phase.
#'
#' @return The value of the CDF of the Coxian distribution at t.
#' @examples
#' lambda_vec <- c(0.5, 0.3)
#' mu_vec <- c(0.2, 0.4)
#' coxianCdf(1, lambda_vec, mu_vec)
#'
#' @export
coxianCdf <- function(t, lambda, mu) {

  # Check if t is a vector or scalar
  if (length(t) > 1) {
    # If t is a vector, apply the function element-wise
    return(sapply(t, function(ti) coxianCdf(ti, lambda, mu)))
  }

  # Handle scalar t
  if (t == 0) {
    return(0)
  }

  return(integrate(coxianPdf, -Inf, t, lambda = lambda, mu = mu)$value)
}
