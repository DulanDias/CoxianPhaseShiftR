#' Optimize Parameters for Coxian Phase Type Distribution
#'
#' This function optimizes the parameters lambda and mu of a Coxian phase type distribution
#' to fit the provided data. The optimization method can be controlled through the `optim_method` parameter.
#'
#' @param data A numeric vector of observed data points.
#' @param init_lambda A numeric vector of initial values for lambda.
#' @param init_mu A numeric vector of initial values for mu.
#' @param optim_method A character string specifying the optimization method to be used by `optim`.
#'   Default is "BFGS".
#'
#' @return A list containing the optimized lambda and mu values.
#'
#' @examples
#' data_sample <- rexp(100, rate = 0.5)
#' init_lambda <- c(0.5, 0)
#' init_mu <- c(0.2, 0.4)
#' result <- optimize(data_sample, init_lambda, init_mu)
#'
#' @export
optimize <- function(data, init_lambda, init_mu, optim_method = "BFGS") {
  # Ensure init_lambda and init_mu have the same length
  if (length(init_lambda) != length(init_mu)) {
    stop("Length of init_lambda and init_mu should be the same.")
  }

  # Ensure the last term of init_lambda is 0
  if (tail(init_lambda, n = 1) != 0) {
    stop("The last term of init_lambda should be 0.")
  }

  # Objective function to be minimized with the transformation
  objective_function <- function(params) {
    lambda <- c(exp(params[1:(length(init_lambda) - 1)]), 0)  # Set the last lambda value to 0 and apply inverse transformation
    mu <- exp(params[(length(init_lambda)):length(params)])
    loss_function(lambda = lambda, mu = mu, data = data, method = "ssd")
  }

  # Apply transformation to the initial parameters
  initial_parameters <- c(log(init_lambda[-length(init_lambda)]), log(init_mu))

  # Optimization
  result <- optim(par = initial_parameters, fn = objective_function, method = optim_method)

  # Extracting lambda and mu values with inverse transformation
  lambda_optimized <- c(exp(result$par[1:(length(init_lambda) - 1)]), 0)  # Add the 0 value for the last lambda
  mu_optimized <- exp(result$par[(length(init_lambda)):length(result$par)])

  return(list(lambda = lambda_optimized, mu = mu_optimized))
}
