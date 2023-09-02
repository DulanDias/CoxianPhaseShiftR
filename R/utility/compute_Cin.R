#' Compute the C_{i,n} term for the Coxian PDF
#'
#' This function computes the C_{i,n} term used in the Coxian phase-type distribution PDF.
#'
#' @param lambda A numeric vector of lambda values.
#' @param mu A numeric vector of mu values.
#' @param i An integer indicating the i-th term.
#' @param n An integer indicating the phase.
#'
#' @return A numeric value representing C_{i,n}.
#'
#' @keywords internal
compute_Cin <- function(lambda, mu, i, n) {
  terms <- sapply(1:n, function(j) {
    if (j != i) {
      return((lambda[j] + mu[j]) / (lambda[j] + mu[j] - lambda[i] - mu[i]))
    } else {
      return(1)
    }
  })
  return(prod(terms))
}
