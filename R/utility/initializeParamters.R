#' Initialize Parameters for Coxian Phase Type Distribution
#'
#' Provides initial estimates for the lambda and mu parameters.
#'
#' @param data A numeric vector or matrix containing the data to be fitted.
#' @param phases An integer indicating the number of phases for the Coxian distribution.
#'
#' @return A list containing initial estimates for:
#' \itemize{
#'   \item \code{lambda_initial} - Initial lambda values.
#'   \item \code{mu_initial} - Initial mu values.
#' }
initializeParameters <- function(data, phases) {
lambda_initial <- runif(phases)
mu_initial <- runif(phases)

return(list(lambda_initial = lambda_initial, mu_initial = mu_initial))
}
