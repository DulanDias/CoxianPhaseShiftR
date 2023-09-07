#' Compute Loss for Fitting Coxian Phase Type Distribution
#'
#' This function computes the loss between observed data and the expected density
#' from a Coxian phase type distribution. The loss can be computed using either
#' the sum of squared differences (SSD) or the negative log-likelihood (NLL).
#'
#' @param lambda A numeric vector representing the lambda values of the Coxian phase type distribution.
#' @param mu A numeric vector representing the mu values of the Coxian phase type distribution.
#' @param data A numeric vector of observed data points.
#' @param method A character string specifying the method to compute the loss.
#'   Either "ssd" for sum of squared differences or "nll" for negative log-likelihood.
#'   Default is "ssd".
#'
#' @return A numeric value representing the computed loss.
#'
#' @examples
#' lambda_sample <- c(0.5, 0.3)
#' mu_sample <- c(0.2, 0.4)
#' data_sample <- rexp(100, rate = 0.5)
#'
#' loss_value_ssd <- loss_function(lambda_sample, mu_sample, data_sample, method = "ssd")
#' loss_value_nll <- loss_function(lambda_sample, mu_sample, data_sample, method = "nll")
#'
loss_function <- function(lambda, mu, data, method = c("ssd", "nll")) {
  method <- match.arg(method)

  # Ensure lambda and mu have the same length
  if (length(lambda) != length(mu)) {
    stop("Length of lambda and mu should be the same.")
  }

  # Compute the kernel density estimation of the data
  kde <- density(data)

  if (method == "ssd") {
    differences <- sapply(data, function(x) {
      observed_density <- interpolate_density(x, kde)
      expected_density <- coxianPdf(x, lambda, mu)

      # Handle NA values in observed_density
      if (is.na(observed_density) || is.na(expected_density)) {
        return(1e6)  # Assign a large but finite penalty for NA values
      }

      return((observed_density - expected_density)^2)
    })
    return(sum(differences))

  } else if (method == "nll") {
    log_likelihoods <- sapply(data, function(x) {
      pdf_value <- coxianPdf(x, lambda, mu)
      log_value <- log(pdf_value)

      # Handle 0 or NA values in pdf_value and log_value
      if (pdf_value == 0 || is.na(pdf_value) || is.na(log_value)) {
        return(-1e6)  # Assign a large but finite penalty for 0 or NA values
      }

      # Handle -Inf values in log_value
      if (log_value == -Inf) {
        return(1e6)  # Assign a large but finite penalty for -Inf values
      }

      return(log_value)
    })
    return(-sum(log_likelihoods))
  }
}


