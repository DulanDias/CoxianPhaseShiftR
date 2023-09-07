library(testthat)

context("Testing progressiveCoxianPhModel")

data("patient_data")
training_data <- patient_data

new_observation  <- data.frame(
  time = 1,
  status = 0,
  patient_id = 101,
  age = 30,
  gender = "Male",
  time_dependent_covariate1 = 0.5,
  time_dependent_covariate2 = -0.3
)

test_that("progressiveCoxianPhModel returns error for invalid current phase", {
  expect_error(progressiveCoxianPhModel(training_data, new_observation, n_phase = 3, current_phase = 0, current_time = 5, strata_by = "phase"),
               "Current phase must be between 1 and n_phases")
  expect_error(progressiveCoxianPhModel(training_data, new_observation, n_phase = 3, current_phase = 4, current_time = 5, strata_by = "phase"),
               "Current phase must be between 1 and n_phases")
})

test_that("progressiveCoxianPhModel returns a numeric value for valid inputs", {
  result <- progressiveCoxianPhModel(training_data, new_observation, n_phase = 3, current_phase = 1, current_time = 5, strata_by = "phase")
  expect_type(result, "double")
})

test_that("progressiveCoxianPhModel adjusts phase numbering correctly", {
  result_phase_3 <- progressiveCoxianPhModel(training_data, new_observation, n_phase = 4, current_phase = 3, current_time = 9, strata_by = "phase")
  result_phase_4 <- progressiveCoxianPhModel(training_data, new_observation, n_phase = 4, current_phase = 4, current_time = 12, strata_by = "phase")

  expect_true(result_phase_4 != result_phase_3)
})
