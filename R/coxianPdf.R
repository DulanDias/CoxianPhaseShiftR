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

  # Vectorized boundary condition: when t is 0, the PDF should be 0
  # Use vectorized ifelse to handle t being a vector
  result <- ifelse(t == 0, 0, NA_real_)

  # Indices for which the PDF needs to be computed (t != 0)
  needs_computation <- is.na(result)

  # Only compute PDF for t values that are not zero
  if (any(needs_computation)) {
    m <- length(lambda)

    # Calculate PDF values for non-zero t's
    computed_values <- sapply(t[needs_computation], function(current_t) {
      # Compute the sum for each phase
      pdf_components <- sapply(1:m, function(j) {
        lambda_j_mu_j_sum <- lambda[j] + mu[j]
        epsilon <- .Machine$double.eps  # Avoid division by zero

        # Compute inner sum, avoiding self-comparison and division by very small numbers
        inner_sum <- prod(sapply(1:m, function(k) {
          if (k != j) {
            # Adjust denominator to ensure numerical stability
            return((lambda[k] + mu[k]) / (lambda[k] + mu[k] - lambda_j_mu_j_sum + epsilon))
          } else {
            return(1)
          }
        }))

        # Calculate and return the component of the PDF for phase j
        inner_sum * lambda_j_mu_j_sum * exp(-lambda_j_mu_j_sum * current_t)
      })

      # Sum up components to get the total PDF value for current_t
      sum(pdf_components)
    })

    # Assign computed PDF values back to the corresponding positions in result
    result[needs_computation] <- computed_values
  }

  return(result)
}


