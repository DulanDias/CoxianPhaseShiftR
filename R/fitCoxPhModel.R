#' Fit Cox Proportional Hazards Model
#'
#' This function fits a Cox proportional hazards model to the input data using specified column names for time, status, strata, and optionally cluster variables. All other columns in the data frame are used as covariates in the model.
#'
#' @param data A data frame containing the data to be used for fitting the model. The data frame must contain columns with names matching the time, status, strata_by parameters, and optionally cluster_by if provided.
#' @param time A string specifying the column name in the data frame to be used as the time variable. This column should contain the time until the event of interest or censoring occurs for each observation.
#' @param status A string specifying the column name in the data frame to be used as the status variable. This column should contain binary values indicating whether the event of interest occurred (1) or the observation was censored (0).
#' @param strata_by A string specifying the column name in the data frame to be used for stratification. Stratification is used to allow for separate baseline hazards for different groups in the data, without estimating separate regression coefficients for the covariates. This can be useful when there is a categorical variable that affects the baseline hazard function but not the hazard ratios. Set to `NULL` if no stratification is needed.
#' @param cluster_by An optional string specifying the column name in the data frame to be used for clustering. Clustering is used to account for correlations between observations within the same group (e.g., multiple observations from the same patient). The cluster variable defines the groups, and the standard errors of the regression coefficients are adjusted to account for the within-group correlations.
#'
#' @return A fit object from the coxph function, which contains the results of the Cox model fit. This object contains various pieces of information about the fitted model, including the regression coefficients, standard errors, and test statistics.
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
fitCoxPhModel <- function(data, time, status, strata_by = NULL, cluster_by = NULL, iter.max = 1000000, eps = 1e-4) {

  # Check the validity of the inputs
  if(!is.data.frame(data)) {
    stop("Input data must be a data frame")
  }

  if(!all(c(time, status) %in% names(data))) {
    stop("Specified column names are not present in the data frame")
  }

  if(any(sapply(data[, c(time, status)], function(col) any(is.na(col))))) {
    stop("Specified columns contain NA values")
  }

  if (!is.null(cluster_by) && !cluster_by %in% names(data)) {
    stop("Specified cluster_by column name is not present in the data frame")
  }

  # Create a Surv object
  data$surv_obj <- Surv(data[[time]], data[[status]])

  # Get the column names to be used as covariates
  covariate_columns <- setdiff(names(data), c(time, status, strata_by, cluster_by, "surv_obj"))

  # Create a formula for the coxph function
  covariate_formula <- paste(covariate_columns, collapse = " + ")

  # Combine the main effects to create the full formula
  if (!is.null(strata_by) && !is.null(cluster_by)) {
    cox_formula <- as.formula(paste("surv_obj ~", covariate_formula, "+ strata(", strata_by, ") + cluster(", cluster_by, ")"))
  } else if (!is.null(strata_by)) {
    cox_formula <- as.formula(paste("surv_obj ~", covariate_formula, "+ strata(", strata_by, ")"))
  } else if (!is.null(cluster_by)) {
    cox_formula <- as.formula(paste("surv_obj ~", covariate_formula, "+ cluster(", cluster_by, ")"))
  } else {
    cox_formula <- as.formula(paste("surv_obj ~", covariate_formula))
  }

  # Fit the Cox proportional hazards model
  fit <- coxph(cox_formula, data = data, iter.max = iter.max, eps = eps)

  # Return the fit object
  return(fit)
}
