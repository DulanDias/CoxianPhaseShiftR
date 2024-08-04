#' Calculate Expected Event Time for Multiple Observations in Coxian Phase-Type Distribution
#'
#' Extends the calculateExpectedEventTime function to handle a dataframe of new observations,
#' adding an estimatedEventTime column to the dataframe with the expected event time for each observation.
#' If an error occurs during the calculation for any observation, the function sets the estimatedEventTime to "ERROR"
#' for that observation and continues processing the remaining observations.
#'
#' @param model_object An object containing the fitted model, including transition rates.
#' @param new_observations A dataframe containing the covariate values for new observations.
#' @param n_phases The total number of phases in the Coxian model.
#' @param upper_time Optional upper limit for numerical integration to approximate infinity. Defaults to 10000, which should be adjusted based on the distribution's characteristics.
#' @param strata_by A string specifying the column name in new_observations used for stratification, aligning with the model's structure.
#' @param file_path Optional file path to save each row's results to a CSV file. If not provided, results will not be saved to a file.
#'
#' @return The input dataframe with an additional `estimatedEventTime` column containing the expected event time for each observation.
#'
#' @examples
#' \dontrun{
#'   # Assume `model_object` is a pre-fitted model object
#'   # `new_observations` is a dataframe with multiple rows, each row having covariate values
#'   result_df <- calculateExpectedEventTimes(model_object, new_observations,
#'                     n_phases = 3, upper_time = 10000, strata_by = "phaseColumn", file_path = "results.csv")
#'   print(result_df)
#' }
#'
#' @export
calculateExpectedEventTimes <- function(model_object, new_observations, n_phases, upper_time = 10000, strata_by, file_path = NULL) {

  # Convert new_observations to a data frame if it's not already
  new_observations <- as.data.frame(new_observations)

  # Read existing data if file_path is provided and the file exists
  existing_data <- if (!is.null(file_path) && file.exists(file_path)) {
    read.csv(file_path)
  } else {
    NULL
  }

  # Initialize the estimatedEventTime column
  new_observations$estimatedEventTime <- NA

  # Iterate over each row to calculate the expected event time
  for (i in 1:nrow(new_observations)) {
    row <- new_observations[i, ]
    new_observations$estimatedEventTime[i] <- tryCatch({
      calculateExpectedEventTime(model_object, row, n_phases, current_phase = row$phase, current_time = row$time, upper_time, strata_by)
    }, error = function(e) {
      return(NA)  # Return NA or any default value on error
    })

    # Save the results if file_path is provided
    if (!is.null(file_path)) {
      write.csv(new_observations, file = file_path, row.names = FALSE)
    }
  }



  return(new_observations)
}
