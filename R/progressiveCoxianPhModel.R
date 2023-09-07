#' Progressive Coxian Phase Type Model
#'
#' This function computes the probability density function (PDF) of a Coxian phase type model at a given time,
#' considering the current phase of the process. It adjusts the number of phases and the data accordingly
#' based on the current phase before fitting the Cox proportional hazards model and estimating the transition rates.
#'
#' @param training_data A data frame containing the training data to fit the Cox proportional hazards model.
#' @param new_observation A data frame containing the covariate values for the new observation.
#' @param n_phases An integer specifying the total number of phases in the Coxian model.
#' @param current_phase An integer specifying the current phase of the process (must be between 1 and n_phases).
#' @param current_time A numeric value specifying the time at which to calculate the Coxian PDF.
#' @param strata_by A string specifying the column name in the data frame to be used for stratification.
#'
#' @return A numeric value representing the calculated PDF of the Coxian phase type model at the specified time.
#'
#' @examples
#' \dontrun{
#'   # Assuming training_data is a data frame with appropriate columns and new_observation is a data frame with the covariate values for the new observation
#'   coxian_pdf <- progressiveCoxianPhModel(training_data, new_observation, n_phases = 3, current_phase = 1, current_time = 5, strata_by = "phase")
#'   print(coxian_pdf)
#' }
#'
#' @importFrom survival coxph
#' @export
progressiveCoxianPhModel <- function(training_data, new_observation, n_phases, current_phase, current_time, strata_by) {
  # Load necessary packages
  library(survival)

  # Check if current phase is valid
  if(current_phase < 1 || current_phase > n_phases) {
    stop("Current phase must be between 1 and n_phases")
  }

  # Adjust the number of phases based on the current phase
  adjusted_n_phases <- n_phases - current_phase + 1

  # Remove data from phases before the current phase
  training_data <- training_data[training_data[strata_by] >= current_phase, ]

  # Adjust the strata_by variable in the training data and new observation to match the new phase numbering
  training_data[strata_by] <- training_data[strata_by] - current_phase + 1
  new_observation[strata_by] <- new_observation[strata_by] - current_phase + 1

  # Fit the Cox PH model with the adjusted number of phases
  fit_result <- fitCoxPhModel(training_data, "time", "status", strata_by, "patient_id", n_phases = adjusted_n_phases)

  # Estimate the transition rates using the fitted model
  transition_rates <- estimate_transition_rates(fit_result$fit, new_observation, n_phases = adjusted_n_phases, strata_by = strata_by)

  # Calculate the PDF of the Coxian phase type model at the given time
  coxian_pdf <- coxianPdf(current_time, transition_rates$lambda, transition_rates$mu)

  # Return the calculated PDF
  return(coxian_pdf)
}
