#' Calculate Expected Event Time for Coxian Phase-Type Distribution
#'
#' This function calculates the expected event time for a given observation
#' within a Coxian phase-type distribution model. It uses numerical integration
#' to compute the expected value based on the probability density function (PDF)
#' derived from the model's transition rates.
#'
#' @param model_object An object containing the fitted model, including transition rates.
#' @param new_observation A dataframe or a vector containing the covariate values for the new observation.
#' @param n_phases The total number of phases in the Coxian model.
#' @param current_phase The current phase of the observation for which the expected time is calculated.
#' @param current_time The current time from which the expectation is calculated. This is the lower limit for the integration.
#' @param upper_time Optional upper limit for numerical integration to approximate infinity. Defaults to 10000, which should be adjusted based on the distribution's characteristics.
#' @param strata_by A string specifying the column name in new_observation used for stratification, aligning with the model's structure.
#'
#' @return A numeric value representing the expected event time for the given observation. If the integration does not converge or if there are numerical issues, the function returns `NA` and issues a warning.
#'
#' @examples
#' \dontrun{
#'   # Assume `model_object` is a pre-fitted model object
#'   # `new_observation` is a dataframe or vector with covariate values
#'   expected_time <- calculateExpectedEventTime(model_object, new_observation,
#'                           n_phases = 3, current_phase = 1, current_time = 0,
#'                           upper_time = 10000, strata_by = "phase")
#'   print(expected_time)
#' }
#'
#' @export
calculateExpectedEventTime <- function(model_object, new_observation, n_phases, current_phase, current_time, upper_time = 10000, strata_by){

  # Estimate the transition rates using the fitted model
  transition_rates <- estimate_transition_rates(model_object$fit, new_observation, n_phases = n_phases, current_phase = current_phase, strata_by = strata_by)
  if(is.null(transition_rates) || is.na(transition_rates$lambda) || is.na(transition_rates$mu)) {
    warning("Transition rates could not be estimated.")
    return(NA)
  }

  lambda <- unlist(transition_rates$lambda)
  mu <- unlist(transition_rates$mu)

  integrand <- function(t) {
    t * coxianPdf(t, lambda, mu)
  }

  result <- integrate(integrand, lower = current_time, upper = upper_time)

  if(result$message == "OK") {
    return(result$value)
  } else {
    warning("Integration may not have converged, check lambda and mu values.")
    return(NA)
  }
}
