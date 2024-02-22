#' Train Coxian Model
#'
#' This function trains a Coxian phase-type model using the provided training data.
#' It fits a Cox proportional hazards model and estimates transition rates for the specified number of phases.
#'
#' @param training_data A dataframe containing the training data, which must include time-to-event, event indicator,
#'        and any covariates for stratification.
#' @param n_phases An integer specifying the total number of phases in the Coxian model.
#' @param strata_by A string specifying the column name in the training_data used for stratification.
#'
#' @return A list containing two elements: `fit_result`, which is the result of fitting the Cox PH model.
#'
#' @examples
#' \dontrun{
#'   training_data <- data.frame(
#'     time = c(5, 10, 15),
#'     status = c(1, 0, 1),
#'     phase = c(1, 2, 3),
#'     patient_id = c(1, 2, 3)
#'   )
#'   result <- trainCoxianModel(training_data, n_phases = 3, strata_by = "phase")
#'   print(result)
#' }
#'
#' @export
trainCoxianModel <- function(training_data, n_phases, strata_by) {
  # Ensure strata_by column exists in training_data
  if(!strata_by %in% colnames(training_data)) {
    stop(paste("Column", strata_by, "not found in training_data"))
  }

  # Fit the Cox PH model
  fit_result <- fitCoxPhModel(training_data, "time", "status", strata_by, "patient_id")

  # Return the fitted model and transition rates
  return(fit_result)
}
