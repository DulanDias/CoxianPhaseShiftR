library(testthat)
context("Testing fitCoxian")

test_that("fitCoxian returns correct structure", {

  # Sample data
  data_sample <- rexp(100, rate = 0.5)

  # Call the fitCoxian function
  result <- fitCoxian(data_sample, 2)

  # Check if lambda, mu, negative_log_likelihood, ssd, AIC, and ks_test are present in the result
  expect_true(all(c("lambda", "mu", "negative_log_likelihood", "ssd", "AIC") %in% names(result)))

  # Check if lambda and mu have the correct length
  expect_equal(length(result$lambda), 2)
  expect_equal(length(result$mu), 2)

  # Ensure the last lambda value is 0
  expect_equal(tail(result$lambda, n = 1), 0)

  # Check if negative_log_likelihood, ssd, and AIC are numeric values
  expect_type(result$negative_log_likelihood, "double")
  expect_type(result$ssd, "double")
  expect_type(result$AIC, "double")

})

test_that("fitCoxian returns reasonable values", {

  # Sample data
  data_sample <- rexp(100, rate = 0.5)

  # Call the fitCoxian function
  result <- fitCoxian(data_sample, 2)

  # Check if lambda and mu values are within reasonable bounds (assuming bounds between 0 and Inf)
  expect_true(all(result$lambda[-length(result$lambda)] >= 0 & result$lambda[-length(result$lambda)] <= Inf))  # Exclude the last lambda value
  expect_true(result$lambda[length(result$lambda)] == 0)  # Ensure the last lambda value is 0
  expect_true(all(result$mu >= 0 & result$mu <= Inf))

  # Check if negative_log_likelihood, ssd, and AIC are not NA
  expect_false(is.na(result$negative_log_likelihood))
  expect_false(is.na(result$ssd))
  expect_false(is.na(result$AIC))

})

test_that("fitCoxian returns correct structure for real dataset for multiple phases", {

  # Load the dataset
  data(los)

  # Check if 'los' is in the environment
  expect_true(exists("los"))

  # Test fitCoxian function on the loaded dataset for various phase numbers
  phases <- c(5)
  for (phase in phases) {
    result <- fitCoxian(los, phase)
    cat(sprintf("Results for %d-phase Coxian:\n", phase))
    print(result)
    cat("\n")
  }
})
