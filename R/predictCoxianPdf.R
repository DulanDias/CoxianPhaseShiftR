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
