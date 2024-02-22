#' Predict Coxian PDF
#'
#' This function predicts the probability density function (PDF) of a Coxian phase-type model at a given time
#' for a new observation, based on a trained model object. It uses the current phase and time to adjust the
#' model's transition rates and calculate the PDF.
#'
#' @param model_object A list containing the trained model output, including `fit_result` and `transition_rates`.
#'        The `transition_rates` must include lambda (transition rates between phases) and mu (absorption rates).
#' @param new_observation A dataframe containing the covariate values for the new observation.
#' @param current_phase An integer specifying the current phase of the process for the new observation.
#'        It must be between 1 and the total number of phases in the model.
#' @param current_time A numeric value specifying the time at which to calculate the Coxian PDF.
#' @param strata_by A string specifying the column name in `new_observation` that represents the phase.
#'
#' @return A numeric value representing the calculated PDF of the Coxian phase-type model at the specified time.
#'
#' @examples
#' \dontrun{
#'   # Assuming `model_object` is a result from `trainCoxianModel` and `new_observation` is properly formatted
#'   pdf_value <- predictCoxianPdf(model_object, new_observation, current_phase = 2, current_time = 10, strata_by = "phase")
#'   print(pdf_value)
#' }
#'
#' @export
predictCoxianPdf <- function(model_object, new_observation, current_phase, current_time, strata_by) {
  # Check if current phase is valid
  if(current_phase < 1 || current_phase > model_object$transition_rates$n_phases) {
    stop("Current phase must be between 1 and the number of phases in the model")
  }

  # Adjust new_observation phase
  new_observation[[strata_by]] <- current_phase

  # Assuming coxianPdf is a function you have or will implement
  # This function calculates the PDF of the Coxian phase type model
  coxian_pdf <- coxianPdf(current_time, unlist(model_object$transition_rates$lambda), unlist(model_object$transition_rates$mu))

  # Return the calculated PDF
  return(coxian_pdf)
}
