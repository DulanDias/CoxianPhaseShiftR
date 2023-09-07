#' Fit Cox Proportional Hazards Model
#'
#' This function fits a Cox proportional hazards model to the input data using specified column names for time, status, strata, and cluster variables. All other columns in the data frame are used as covariates in the model.
#'
#' @param data A data frame containing the data to be used for fitting the model. The data frame must contain columns with names matching the time, status, strata_by, and cluster_by parameters.
#' @param time A string specifying the column name in the data frame to be used as the time variable.
#' @param status A string specifying the column name in the data frame to be used as the status variable.
#' @param strata_by A string specifying the column name in the data frame to be used for stratification.
#' @param cluster_by A string specifying the column name in the data frame to be used for clustering.
#'
#' @return A fit object from the coxph function, which contains the results of the Cox model fit.
#'
#' @examples
#' \dontrun{
#'   # Assuming df is a data frame with appropriate columns
#'   fit_result <- fitCoxPhModel(df, "time_column_name", "status_column_name", "strata_column_name", "cluster_column_name")
#'
#'   # Summary of the fit result
#'   summary(fit_result)
#' }
#'
#' @importFrom survival coxph Surv
#' @export
fitCoxPhModel <- function(data, time, status, strata_by, cluster_by) {
  # Load the necessary package
  library(survival)

  # Check the validity of the inputs
  if(!is.data.frame(data)) {
    stop("Input data must be a data frame")
  }

  if(!all(c(time, status, strata_by, cluster_by) %in% names(data))) {
    stop("Specified column names are not present in the data frame")
  }

  if(any(sapply(data[, c(time, status, strata_by, cluster_by)], function(col) any(is.na(col))))) {
    stop("Specified columns contain NA values")
  }

  # Create a Surv object
  data$surv_obj <- with(data, Surv(get(time), get(status)))

  # Get the column names to be used as covariates (excluding the specified columns and the created surv_obj column)
  covariate_columns <- setdiff(names(data), c(time, status, strata_by, cluster_by, "surv_obj"))

  # Create a formula for the coxph function
  covariate_formula <- paste(covariate_columns, collapse = " + ")
  cox_formula <- as.formula(paste("surv_obj ~", covariate_formula, "+ strata(", strata_by, ") + cluster(", cluster_by, ")"))

  # Fit the Cox proportional hazards model
  fit <- coxph(cox_formula, data = data)

  # Return the fit object
  return(fit)
}
