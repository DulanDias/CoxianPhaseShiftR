library(testthat)
context("Testing optimize")

test_that("optimize function returns correct structure", {

  # Sample data and initial parameters
  data_sample <- rexp(100, rate = 0.5)
  init_lambda <- c(0.5, 0.3, 0)
  init_mu <- c(0.2, 0.4, 0.6)

  # Call the optimize function
  result <- optimize(data_sample, init_lambda, init_mu)

  # Check if lambda and mu are correctly structured
  expect_equal(length(result$lambda), length(init_lambda))
  expect_equal(length(result$mu), length(init_mu))

  # Check if lambda and mu are named correctly
  expect_true("lambda" %in% names(result))
  expect_true("mu" %in% names(result))
})

test_that("optimize function returns values within reasonable bounds", {

  # Sample data and initial parameters
  data_sample <- rexp(100, rate = 0.5)
  init_lambda <- c(0.5, 0.3, 0)
  init_mu <- c(0.2, 0.4, 0.6)

  # Call the optimize function
  result <- optimize(data_sample, init_lambda, init_mu)

  # Check if lambda and mu values are within reasonable bounds (assuming bounds between 0 and Inf)
  expect_true(all(result$lambda[-length(result$lambda)] >= 0))  # Exclude the last lambda value
  expect_true(result$lambda[length(result$lambda)] == 0)  # Ensure the last lambda value is 0
  expect_true(all(result$mu >= 0))
})

