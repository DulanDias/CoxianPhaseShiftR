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
  # Validate inputs
  if (length(lambda) != length(mu)) {
    stop("Length of lambda and mu should be the same.")
  }

  if (!is.numeric(lambda) || !is.numeric(mu) || any(lambda < 0) || any(mu < 0)) {
    stop("Both lambda and mu should be non-negative numeric vectors.")
  }

  if (any(t < 0) || !is.numeric(t)) {
    stop("t should be a non-negative numeric vector.")
  }

  m <- length(lambda)
  lambda_mu_sum <- lambda + mu

  # Precompute product terms independent of t
  product_terms <- sapply(1:m, function(j) {
    prod(sapply(1:m, function(k) {
      if (k != j) {
        return((lambda[k] + mu[k]) / (lambda[k] + mu[k] - lambda_mu_sum[j]))
      } else {
        return(1)
      }
    }))
  })

  # Vectorized calculation of phase components for each t
  phase_components <- sapply(t, function(t_val) {
    if (t_val == 0) {
      return(0)
    } else {
      exp_components <- lambda_mu_sum * exp(-lambda_mu_sum * t_val)
      return(sum(exp_components * product_terms))
    }
  })

  return(phase_components)
}

