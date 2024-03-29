library(testthat)

context("Testing estimate_transition_rates")

test_that("estimate_transition_rates returns an error for invalid inputs", {

  # Assuming fit_result is a coxph object obtained from the fitCoxPhModel function
  fit_result <- fitCoxPhModel(patient_data, "time", "status", "phase", "patient_id")

  # Test that an error is returned for a non-coxph input
  expect_error(estimate_transition_rates("not a coxph object", new_obs, fit_result$n_phases, fit_result$strata_by))

  # Test that an error is returned for a non-data frame new observation
  expect_error(estimate_transition_rates(fit_result$fit, "not a data frame", fit_result$n_phases, fit_result$strata_by))
})

test_that("estimate_transition_rates returns a list with lambda and mu for valid inputs", {

  # Assuming fit_result is a coxph object obtained from the fitCoxPhModel function
  fit_result <- fitCoxPhModel(patient_data, "time", "status", "phase", "patient_id")

  # Assuming new_obs is a data frame with the covariate values for the new observation
  new_obs <- data.frame(
    time = 1,
    status = 0,
    patient_id = 101,
    age = 30,
    gender = "Male",
    time_dependent_covariate1 = 0.5,
    time_dependent_covariate2 = -0.3
  )

  # Test that a list with lambda and mu is returned for valid inputs
  transition_rates <- estimate_transition_rates(fit_result$fit, new_obs, fit_result$n_phases, fit_result$strata_by)
  expect_is(transition_rates, "list")
  expect_named(transition_rates, c("lambda", "mu"))
})

test_that("estimate_transition_rates computes transition rates correctly", {

  # Assuming fit_result is a coxph object obtained from the fitCoxPhModel function
  fit_result <- fitCoxPhModel(patient_data, "time", "status", "phase", "patient_id")

  # Assuming new_obs is a data frame with the covariate values for the new observation
  new_obs <- data.frame(
    time = 1,
    status = 0,
    patient_id = 101,
    age = 30,
    gender = "Male",
    time_dependent_covariate1 = 0.5,
    time_dependent_covariate2 = -0.3
  )

  # Test that the transition rates are computed correctly
  transition_rates <- estimate_transition_rates(fit_result$fit, new_obs, fit_result$n_phases, fit_result$strata_by)

  expect_true(all(unlist(sapply(transition_rates$lambda, function(x) x >= 0))))
  expect_true(all(unlist(sapply(transition_rates$mu, function(x) x >= 0))))
})

test_that("estimate_transition_rates prints the results correctly", {

  # Assuming fit_result is a coxph object obtained from the fitCoxPhModel function
  fit_result <- fitCoxPhModel(patient_data, "time", "status", "phase", "patient_id")

  # Assuming new_obs is a data frame with the covariate values for the new observation
  new_obs <- data.frame(
    time = 1,
    status = 0,
    patient_id = 101,
    age = 30,
    gender = "Male",
    time_dependent_covariate1 = 0.5,
    time_dependent_covariate2 = -0.3
  )

  # Estimate the transition rates for the new observation
  transition_rates <- estimate_transition_rates(fit_result$fit, new_obs, fit_result$n_phases, fit_result$strata_by)

  # Print the estimated lambda and mu values
  #print(paste("Estimated lambda:", transition_rates$lambda))
  #print(paste("Estimated mu:", transition_rates$mu))

  # Check that the transition_rates object is a list with named elements "lambda" and "mu"
  expect_is(transition_rates, "list")
  expect_named(transition_rates, c("lambda", "mu"))
})

