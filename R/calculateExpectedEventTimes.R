#' Calculate Expected Event Time for Multiple Observations in Coxian Phase-Type Distribution
#'
#' Extends the calculateExpectedEventTime function to handle a dataframe of new observations,
#' adding an estimatedEventTime column to the dataframe with the expected event time for each observation.
#'
#' @param model_object An object containing the fitted model, including transition rates.
#' @param new_observations A dataframe containing the covariate values for new observations.
#' @param n_phases The total number of phases in the Coxian model.
#' @param current_phase The column name in new_observations that indicates the current phase of each observation.
#' @param current_time The column name in new_observations that indicates the current time from which the expectation is calculated.
#' @param upper_time Optional upper limit for numerical integration to approximate infinity. Defaults to 10000, which should be adjusted based on the distribution's characteristics.
#' @param strata_by A string specifying the column name in new_observations used for stratification, aligning with the model's structure.
#'
#' @return The input dataframe with an additional `estimatedEventTime` column containing the expected event time for each observation.
#'
#' @examples
#' \dontrun{
#'   # Assume `model_object` is a pre-fitted model object
#'   # `new_observations` is a dataframe with multiple rows, each row having covariate values
#'   result_df <- calculateExpectedEventTime(model_object, new_observations,
#'                     n_phases = 3, current_phase = "currentPhaseColumn", current_time = "currentTimeColumn",
#'                     upper_time = 10000, strata_by = "phaseColumn")
#'   print(result_df)
#' }
#' @export
calculateExpectedEventTimes <- function(model_object, new_observations, n_phases, upper_time = 10000, strata_by) {
  # Ensure that rowwise operations can be performed
  new_observations <- rowwise(new_observations)

  # Add the estimatedEventTime column
  new_observations <- new_observations %>%
    mutate(estimatedEventTime = list(calculateExpectedEventTime(model_object, cur_data(), n_phases, current_phase = cur_data()$phase, current_time = cur_data()$time, upper_time, strata_by))) %>%
    unnest(cols = c(estimatedEventTime))

  return(new_observations)
}
