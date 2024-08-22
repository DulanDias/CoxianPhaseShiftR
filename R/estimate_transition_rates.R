#' Estimate Transition Rates
#'
#' @param coxph_object A coxph object from the fitCoxPhModel function.
#' @param new_observation A data frame with the covariate values for the new observation.
#' @param n_phases The total number of phases in the Coxian model.
#' @param current_phase The current phase to start the estimation from.
#' @param strata_by A string specifying the column name in new_observation used for stratification, aligning with the model's structure.
#'
#' @return A list containing the estimated lambda and mu values for each phase.
#'
#' @examples
#' \dontrun{
#'   # Assuming fit_result is a coxph object from the fitCoxPhModel function
#'   # and new_obs is a data frame with the covariate values for the new observation
#'   transition_rates <- estimate_transition_rates(fit_result, new_obs, n_phases = 3, current_phase = 2)
#'   print(transition_rates)
#' }
#'
#' @export
estimate_transition_rates <- function(coxph_object, new_observation, n_phases, current_phase, strata_by) {

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

  # Replace NA values with 0
  cox_coefficients[is.na(cox_coefficients)] <- 0

  if(!is.numeric(n_phases) || n_phases <= 0) {
    stop("Number of phases must be a positive integer")
  }

  if(!is.numeric(current_phase) || current_phase <= 0 || current_phase > n_phases) {
    stop("current_phase must be a positive integer between 1 and the number of phases")
  }

  # Initialize lists to store lambda and mu values for each phase
  lambda_list <- vector("list", n_phases)
  mu_list <- vector("list", n_phases)

  # Loop through each phase to calculate lambda and mu values
  for(i in seq(current_phase, n_phases)) {
      # Create a new variable in new_observation to match the current phase
      new_observation[[as.symbol(strata_by)]] <- i

      # Match the names of the coefficients with the column names in the new observation
      matching_cols <- intersect(names(cox_coefficients), names(new_observation))

      # Calculate the linear predictor (X %*% beta), including interaction terms
      linear_predictor <- sum(new_observation[1, matching_cols] * cox_coefficients[matching_cols])

      # Calculate the baseline hazard function
      baseline_hazard <- basehaz(coxph_object)

      # Calculate the survival function at the specified time
      surv_fit <- survfit(coxph_object, newdata = new_observation[1, , drop = FALSE])

      # Get the hazard rate at the specified time
      hazard_rate <- summary(surv_fit, times = new_observation$time[1], extend = TRUE)$cumhaz
      if (length(hazard_rate) == 0) {
        warning("Transition rates could not be estimated for some phases.")
        hazard_rate <- NA  # or set to a default value
      }

      # Calculate the transition rates for the current phase
      if (i < n_phases) {
        lambda_list[[i]] <- exp(linear_predictor) * tail(baseline_hazard$hazard, n = 1) # Transition rate to next phase
      } else {
        lambda_list[[i]] <- 0 # Transition rate to next phase for the last phase is 0
      }
      mu_list[[i]] <- (1 - exp(-tail(baseline_hazard$hazard, n = 1))) * exp(linear_predictor) # Transition rate to absorbing state

  }

  # Return the estimated lambda and mu values for each phase
  return(list(lambda = lambda_list, mu = mu_list))
}
