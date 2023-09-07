#' Fit Coxian Phase Type Distribution to Data
#'
#' Fits a Coxian phase type distribution to the provided data based on the specified number of phases.
#'
#' @param data A numeric vector of observed data points.
#' @param n_phases An integer specifying the number of phases for the Coxian distribution.
#' @param objective A character string specifying the objective function to be minimized during
#'   optimization. Can take values "nll" (negative log-likelihood) or "ssd" (sum of squared differences).
#'   Default is "ssd".
#'
#' @return A list containing the optimized lambda and mu values, negative log-likelihood, sum of squared
#'   differences (SSD) and Akaike Information Criterion (AIC).
#'
#' @examples
#' data_sample <- rexp(100, rate = 0.5)
#' fit_results <- fitCoxian(data_sample, 2, objective = "ssd")
#'
#' @export
fitCoxian <- function(data, n_phases, objective = "ssd") {
  # Initial values for lambda and mu
  init_lambda <- c(rep(0.5, n_phases - 1), 0)
  init_mu <- rep(0.5, n_phases)

  # Optimize the parameters
  optimized_params <- optimize(data, init_lambda, init_mu, objective = objective)

  # Extract lambda and mu from the optimized parameters
  lambda <- optimized_params$lambda
  mu <- optimized_params$mu

  # Compute the negative log-likelihood
  nll <- loss_function(lambda, mu, data, method = "nll")

  # Compute the sum of squared differences (SSD)
  ssd <- loss_function(lambda, mu, data, method = "ssd")

  # Compute the Akaike Information Criterion (AIC)
  aic <- coxianAic(data, lambda, mu)

  return(list(lambda = lambda, mu = mu, negative_log_likelihood = nll, ssd = ssd, AIC = aic))
}
