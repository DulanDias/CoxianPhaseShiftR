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

  # Ensure lambda and mu are numeric vectors and non-negative
  if (!is.numeric(lambda) || !is.numeric(mu) || any(lambda < 0) || any(mu < 0)) {
    stop("Both lambda and mu should be non-negative numeric vectors.")
  }

  # Ensure t is non-negative and numeric
  if (any(t < 0) || !is.numeric(t)) {
    stop("t should be a non-negative numeric vector.")
  }

  # Initialize result vector
  result <- numeric(length(t))

  # Compute PDF for each element in t
  for (i in seq_along(t)) {
    # Boundary condition: when t is 0, the PDF should be 0
    if (t[i] == 0) {
      result[i] <- 0
    } else {
      m <- length(lambda)
      # Compute the PDF using a stable method
      pdf_value <- sum(sapply(1:m, function(j) {
        lambda_j_mu_j_sum <- lambda[j] + mu[j]
        # Avoid division by zero or very small numbers by adding a small epsilon
        epsilon <- .Machine$double.eps
        inner_sum <- sum(sapply(1:m, function(k) {
          if (k != j) {
            lambda_k_mu_k_diff <- lambda[k] + mu[k] - lambda_j_mu_j_sum
            # Adjust denominator to avoid division by zero
            return((lambda[k] + mu[k]) / (lambda_k_mu_k_diff + epsilon))
          } else {
            return(1)
          }
        }))
        # Compute PDF component for j
        return(inner_sum * lambda_j_mu_j_sum * exp(-lambda_j_mu_j_sum * t[i]))
      }))
      result[i] <- pdf_value
    }
  }

  return(result)
}


