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
coxian_pdf <- function(t, lambda, mu) {
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
