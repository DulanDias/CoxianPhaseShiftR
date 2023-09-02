#' Compute the p_n term for the Coxian PDF
#'
#' This function computes the p_n term used in the Coxian phase-type distribution PDF.
#'
#' @param lambda A numeric vector of lambda values.
#' @param mu A numeric vector of mu values.
#' @param n An integer indicating the phase for which p_n is to be computed.
#'
#' @return A numeric value representing p_n.
#'
#' @keywords internal
compute_pn <- function(lambda, mu, n) {
  if (n == 1) {
    return(mu[1] / (lambda[1] + mu[1]))
  }

  product_term <- prod(sapply(1:(n-1), function(i) lambda[i] / (lambda[i] + mu[i])))
  return(product_term * mu[n] / (lambda[n] + mu[n]))
}
