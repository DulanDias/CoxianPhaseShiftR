#' Estimate Transition Rates
#'
#' This function estimates the transition rates (lambda and mu) for a new observation using the coefficients from a fitted Cox proportional hazards model.
#'
#' @param coxph_object A coxph object obtained from fitting a Cox proportional hazards model using the fitCoxPhModel function.
#' @param new_observation A data frame containing the covariate values for the new observation. The data frame should have the same structure as the input data used to fit the Cox model.
#'
#' @return A list containing the estimated lambda and mu values for the new observation.
#'
#' @examples
#' \dontrun{
#'   # Assuming fit_result is a coxph object obtained from the fitCoxPhModel function
#'   # and new_obs is a data frame with the covariate values for the new observation
#'   transition_rates <- estimate_transition_rates(fit_result, new_obs)
#'   print(transition_rates)
#' }
#'
#' @export
estimate_transition_rates <- function(coxph_object, new_observation) {
  # Check if the input is a valid coxph object
  if(!inherits(coxph_object, "coxph")) {
    stop("Input must be a valid coxph object")
  }

  # Check if the new observation is a valid data frame
  if(!is.data.frame(new_observation)) {
    stop("New observation must be a data frame")
  }

  # Extract the coefficients from the coxph object
  cox_coefficients <- coef(coxph_object)

  # Match the names of the coefficients with the column names in the new observation
  matching_cols <- intersect(names(cox_coefficients), names(new_observation))

  # Calculate the linear predictor (X %*% beta)
  linear_predictor <- sum(new_observation[1, matching_cols] * cox_coefficients[matching_cols])

  # Calculate the baseline hazard function at the specified time
  baseline_hazard <- basehaz(coxph_object, newdata = new_observation[1, , drop = FALSE])

  # Calculate the transition rates
  lambda <- exp(linear_predictor) * baseline_hazard$hazard
  mu <- exp(-linear_predictor) * baseline_hazard$hazard

  # Return the estimated lambda and mu values
  return(list(lambda = lambda, mu = mu))
}
