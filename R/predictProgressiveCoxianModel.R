#' Predict Event Times Using Progressive Coxian Model
#'
#' This function predicts the expected event times for new observations using a fitted Progressive Coxian model.
#' It selects the appropriate model from the list of fitted models based on the "phase" variable in the new observations.
#'
#' @param model_list A list of fitted Cox PH model objects, which is the output of `trainProgressiveCoxianModel`.
#' @param new_observations A dataframe containing the covariate values for new observations, including the "phase" variable.
#' @param n_phases The total number of phases in the Coxian model.
#' @param upper_time Optional upper limit for numerical integration to approximate infinity. Defaults to 10000.
#' @param strata_by A string specifying the column name in new_observations used for stratification, aligning with the model's structure.
#' @param file_path Optional file path to save each row's results to a CSV file. If not provided, results will not be saved to a file.
#'
#' @return The input dataframe with an additional `estimatedEventTime` column containing the expected event time for each observation.
#'
#' @examples
#' \dontrun{
#'   # Assume `model_list` is a list of pre-fitted model objects
#'   # `new_observations` is a dataframe with multiple rows, each row having covariate values and a "phase" column
#'   result_df <- predictProgressiveCoxianModel(model_list, new_observations,
#'                     n_phases = 3, upper_time = 10000, strata_by = "phaseColumn", file_path = "predictions.csv")
#'   print(result_df)
#' }
#'
#' @export
predictProgressiveCoxianModel <- function(model_list, new_observations, n_phases, upper_time = 10000, strata_by, file_path = NULL) {

  # Convert new_observations to a data frame if it's not already
  new_observations <- as.data.frame(new_observations)

  # Initialize the estimatedEventTime column
  new_observations$estimatedEventTime <- NA

  # Iterate over each row to calculate the expected event time
  for (i in 1:nrow(new_observations)) {
    row <- new_observations[i, ]

    # Determine the phase key based on the current phase of the observation
    phase_key <- paste0(n_phases - row[[strata_by]] + 1, "-phase")

    # Select the appropriate model from the list
    model_object <- model_list[[phase_key]]

    # Calculate the expected event time using the selected model
    new_observations$estimatedEventTime[i] <- tryCatch({
      calculateExpectedEventTime(
        model_object,
        row,
        n_phases,
        current_phase = row[[strata_by]],
        current_time = row$time,
        upper_time,
        strata_by
      )
    }, error = function(e) {
      return(NA)  # Return NA on error
    })

    # Save the results if file_path is provided
    if (!is.null(file_path)) {
      write.csv(new_observations, file = file_path, row.names = FALSE)
    }
  }

  return(new_observations)
}
