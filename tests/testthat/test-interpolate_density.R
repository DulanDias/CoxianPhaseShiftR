library(testthat)
context("Testing interpolate_density")

test_that("interpolate_density returns correct values", {

  # Generate sample data and its KDE
  data_sample <- rexp(100, rate = 0.5)
  kde <- density(data_sample)

  # Test for a known value
  interpolated_value <- interpolate_density(0.5, kde)
  expect_type(interpolated_value, "double")

  # Test for multiple values
  interpolated_values <- interpolate_density(c(0.5, 1, 1.5), kde)
  expect_equal(length(interpolated_values), 3)
})
