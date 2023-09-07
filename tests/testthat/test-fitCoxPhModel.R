library(testthat)
context("Testing fitCoxPhModel")

test_that("fitCoxPhModel returns an error for invalid inputs", {
  # Load the patient data
  data("patient_data")

  # Test that an error is returned for a non-data frame input
  expect_error(fitCoxPhModel("not a data frame", "time", "status", "phase", "patient_id"))

  # Test that an error is returned for missing column names
  expect_error(fitCoxPhModel(patient_data, "nonexistent_column", "status", "phase", "patient_id"))
})


test_that("fitCoxPhModel returns a coxph object for valid inputs", {
  # Load the patient data
  data("patient_data")

  # Test that a coxph object is returned for valid inputs
  fit <- fitCoxPhModel(patient_data, "time", "status", "phase", "patient_id")
  expect_s3_class(fit, "coxph")
})


test_that("fitCoxPhModel handles NA values correctly", {
  # Load the patient data
  data("patient_data")

  # Introduce NA values into the data
  patient_data_with_na <- patient_data
  patient_data_with_na[1, "time"] <- NA

  # Test that an error is returned for data with NA values in the specified columns
  expect_error(fitCoxPhModel(patient_data_with_na, "time", "status", "phase", "patient_id"))
})

test_that("fitCoxPhModel prints the results correctly", {
  # Load the patient data
  data("patient_data")

  # Fit the model to the data
  fit <- fitCoxPhModel(patient_data, "time", "status", "phase", "patient_id")

  # Print the summary of the fit object
  print(summary(fit))

  # Check that the fit object is of class "coxph"
  expect_s3_class(fit, "coxph")
})
