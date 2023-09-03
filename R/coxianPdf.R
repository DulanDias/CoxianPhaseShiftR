#' Coxian Phase-Type Distribution PDF
#'
#' Computes the probability density function (PDF) of a Coxian phase-type distribution for a given time value t.
#'
#' @param t A numeric value representing time.
#' @param lambda A numeric vector of lambda values.
#' @param mu A numeric vector of mu values.
#'
#' @return A numeric value representing the PDF value at time t.
#'
#' @examples
#' lambda <- c(0.5, 0.3, 0)
#' mu <- c(0.2, 0.4, 0.6)
#' t_value <- 1
#' pdf_value <- coxian_pdf(t_value, lambda, mu)
#' print(pdf_value)
#'
#' @export
coxianPdf <- function(t, lambda, mu) {
  # Check if lambda and mu have the same length
  if (length(lambda) != length(mu)) {
    stop("Length of lambda and mu should be the same.")
  }

  # Ensure lambda and mu are numeric vectors
  if (!is.numeric(lambda) || !is.numeric(mu)) {
    stop("Both lambda and mu should be numeric vectors.")
  }

  # Ensure t is non-negative
  if (t < 0 || !is.numeric(t)) {
    stop("t should be a non-negative number.")
  }

  # Boundary condition: when t is 0, the PDF should be 0
  if (t == 0) {
    return(0)
  }

  m <- length(lambda)

  # Compute the outer sum
  result <- sum(sapply(1:m, function(i) {
    inner_sum <- sum(sapply(1:m, function(n) {
      compute_pn(lambda, mu, n) * compute_Cin(lambda, mu, i, n)
    }))
    return(inner_sum * (lambda[i] + mu[i]) * exp(- (lambda[i] + mu[i]) * t))
  }))

  return(result)
}

