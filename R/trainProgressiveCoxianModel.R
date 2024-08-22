#' Train Progressive Coxian Model
#'
#' This function trains a Progressive Coxian phase-type model using the provided training data.
#' It fits a Cox proportional hazards model and estimates transition rates for the specified number of phases.
#'
#' @param training_data A dataframe containing the training data, which must include a time-to-event variable (`time`), an event indicator (`event`),
#'        and any covariates for stratification.
#' @param n_phases An integer specifying the total number of phases in the Coxian model.
#' @param strata_by A string specifying the column name in the training_data used for stratification (e.g., "phase").
#' @param time A string specifying the column name in the training_data that represents the current time variable.
#' @param event A string specifying the column name in the training_data that represents the event indicator (1 if event occurred, 0 if censored).
#' @param cluster_by An optional string specifying the column name in the training_data to be used for clustering.
#' @param penalty A string specifying the type of penalty for regularization (e.g., "none", "ridge", "lasso"). Defaults to "none".
#' @param lambda A numeric value specifying the regularization strength. Defaults to 1.
#'
#' @return A list containing the results of the fitted Cox PH models for each phase.
#'
#' @examples
#' \dontrun{
#'   training_data <- data.frame(
#'     time = c(5, 10, 15, 20, 25),
#'     event = c(1, 0, 1, 1, 0),
#'     phase = c(1, 1, 2, 2, 3),
#'     cluster_id = c(101, 101, 102, 102, 103)
#'   )
#'   result <- trainProgressiveCoxianModel(training_data, n_phases = 3, strata_by = "phase", time = "time", event = "event", cluster_by = "cluster_id")
#'   print(result)
#' }
#'
#' @export
trainProgressiveCoxianModel <- function(training_data, n_phases, strata_by, time, event, cluster_by = NULL, penalty = "none", lambda = 1) {

  # Ensure the necessary columns exist in training_data
  required_columns <- c(time, event, strata_by)
  if (!is.null(cluster_by)) {
    required_columns <- c(required_columns, cluster_by)
  }

  missing_columns <- setdiff(required_columns, colnames(training_data))
  if (length(missing_columns) > 0) {
    stop(paste("The following required columns are missing from training_data:", paste(missing_columns, collapse = ", ")))
  }

  # Initialize a list to store the results of the fitted models
  fit_results <- list()

  # Loop over phases, filtering the data and fitting the model
  for (phase in 1:n_phases) {
    # Filter the data to include only the relevant phases
    filtered_data <- training_data[training_data[[strata_by]] >= phase, ]

    # Handle 1-phase model specifically
    if (n_phases - phase + 1 == 1) {
      warning("Only one phase remaining, fitting a simplified Cox model without stratification or clustering.")
      fit_result <- fitCoxPhModel(filtered_data, time, event, strata_by = NULL, cluster_by = NULL, penalty = penalty, lambda = lambda)
    } else {
      # Fit the Cox PH model with or without clustering
      fit_result <- fitCoxPhModel(filtered_data, time, event, strata_by, cluster_by, penalty = penalty, lambda = lambda)
    }

    # Store the result in the list with the phase-specific key
    phase_key <- paste0(n_phases - phase + 1, "-phase")
    fit_results[[phase_key]] <- fit_result
  }

  # Return the list of fitted models
  return(fit_results)
}
