progressiveCoxianPhModel <- function(training_data, new_observation, n_phases, current_phase, current_time, strata_by) {

  # Load the necessary package
  library(survival)
  
  # Adjust the number of phases based on the current phase
  adjusted_n_phases <- n_phases - current_phase + 1

  # Remove data from phases before the current phase
  training_data <- training_data[training_data[[strata_by]] >= current_phase, ]

  # Adjust the phase variable in the training data and new observation to match the new phase numbering
  training_data[[strata_by]] <- training_data[[strata_by]] - current_phase + 1
  new_observation[[strata_by]] <- current_phase

  # Fit the Cox PH model with the adjusted number of phases

  # Determine the number of phases
  n_phases <- length(unique(training_data[[strata_by]]))

  # Create a Surv object
  training_data$surv_obj <- Surv(training_data[[time]], training_data[[status]])

  # Get the column names to be used as covariates
  # (excluding the specified columns and the created surv_obj column)
  covariate_columns <- setdiff(names(training_data), c(time, status, strata_by, "patient_id", "surv_obj"))

  # Create a formula for the coxph function
  covariate_formula <- paste(covariate_columns, collapse = " + ")

  # Combine the main effects to create the full formula (without interaction terms involving strata_by variable)
  cox_formula <- as.formula(paste("surv_obj ~", covariate_formula, "+ strata(", strata_by, ") + cluster(", patient_id, ")"))

  # Fit the Cox proportional hazards model
  fit <- coxph(cox_formula, data = training_data)

  # Return the fit object and the number of phases
  fit_result <- list(fit = fit, n_phases = n_phases, strata_by = strata_by)

  # Estimate the transition rates using the fitted model

  coxph_object <- fit_result$fit

  # Extract the coefficients from the coxph object
  cox_coefficients <- coef(coxph_object)

  # Determine the number of phases from the coxph object if not provided
  if(is.null(adjusted_n_phases)) {
    adjusted_n_phases <- length(unique(coxph_object$strata))
  }

  # Initialize lists to store lambda and mu values for each phase
  lambda_list <- vector("list", adjusted_n_phases)
  mu_list <- vector("list", adjusted_n_phases)

  # Loop through each phase to calculate lambda and mu values
  for(i in seq_len(adjusted_n_phases)) {

    # Create a new variable in new_observation to match the current phase
    new_observation[[as.symbol(strata_by)]] <- i

    # Match the names of the coefficients with the column names in the new observation
    matching_cols <- intersect(names(cox_coefficients), names(new_observation))

    # Calculate the linear predictor (X %*% beta), including interaction terms
    linear_predictor <- sum(new_observation[1, matching_cols] * cox_coefficients[matching_cols])

    # Calculate the baseline hazard function at the specified time
    baseline_hazard <- basehaz(coxph_object, newdata = new_observation[1, , drop = FALSE])

    # Calculate the survival function at the specified time
    surv_fit <- survfit(coxph_object, newdata = new_observation[1, , drop = FALSE])

    # Get the hazard rate at the specified time
    hazard_rate <- summary(surv_fit, times = new_observation$time[1])$cumhaz

    #print(summary(surv_fit, times = new_observation$time[1]))

    # Calculate the transition rates for the current phase
    if(i < adjusted_n_phases) {
      lambda_list[i] <- exp(linear_predictor) * tail(baseline_hazard$hazard, n = 1) # Transition rate to next phase
    } else {
      lambda_list[i] <- 0 # Transition rate to next phase for the last phase is 0
    }
    mu_list[i] <- (1 - exp(-tail(baseline_hazard$hazard, n = 1))) * exp(linear_predictor) # Transition rate to absorbing state
  }

  # Return the estimated lambda and mu values for each phase
  transition_rates <- list(lambda = lambda_list, mu = mu_list)
  
  # Calculate the PDF of the Coxian phase type model at the given time

  # Boundary condition: when current_time is 0, the PDF should be 0
  if (current_time == 0) {
    return(0)
  }

  lambda <- unlist(transition_rates$lambda)
  mu <- unlist(transition_rates$mu)

  m <- length(lambda)

  # Compute the outer sum
  coxian_pdf <- sum(sapply(1:m, function(i) {
    inner_sum <- sum(sapply(1:m, function(n) {
      terms <- sapply(1:n, function(j) {
        if (j != i) {
          return((lambda[j] + mu[j]) / (lambda[j] + mu[j] - lambda[i] - mu[i]))
        } else {
          return(1)
        }
      })

      if (n == 1) {
        inner_sum <- mu[1] / (lambda[1] + mu[1])
      }
    
      product_term <- prod(sapply(1:(n-1), function(i) lambda[i] / (lambda[i] + mu[i])))
      inner_sum <- product_term * mu[n] / (lambda[n] + mu[n])
      
      inner_sum * prod(terms)
    }))
    return(inner_sum * (lambda[i] + mu[i]) * exp(- (lambda[i] + mu[i]) * current_time))
  }))

  # Return the calculated PDF
  return(coxian_pdf)
}
