trainCoxianModel <- function(training_data, n_phases, strata_by) {
  # Ensure strata_by column exists in training_data
  if(!strata_by %in% colnames(training_data)) {
    stop(paste("Column", strata_by, "not found in training_data"))
  }

  # Fit the Cox PH model
  # Assuming fitCoxPhModel is a function you have or will implement
  fit_result <- fitCoxPhModel(training_data, time_col = "time", status_col = "status", strata_col = strata_by, id_col = "patient_id")

  # Assuming estimate_transition_rates is a function you have or will implement
  # This function should estimate transition rates based on the fitted Cox PH model
  transition_rates <- estimate_transition_rates(fit_result, n_phases = n_phases, strata_by = strata_by)

  # Return the fitted model and transition rates
  return(list(fit_result = fit_result, transition_rates = transition_rates))
}
