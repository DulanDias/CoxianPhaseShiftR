library(testthat)
context("Testing loss_function")

test_that("loss_function returns correct values", {

  lambda <- c(0.5, 0.3, 0)
  mu <- c(0.2, 0.4, 0.6)
  data_sample <- rexp(100, rate = 0.5)

  # Test for SSD method
  ssd_result <- loss_function(lambda, mu, data_sample, method = "ssd")
  expect_type(ssd_result, "double")

  # Test for NLL method
  nll_result <- loss_function(lambda, mu, data_sample, method = "nll")
  expect_type(nll_result, "double")

  # Test for invalid method
  expect_error(loss_function(lambda, mu, data_sample, method = "invalid"),
               "'arg' should be one of \"ssd\", \"nll\"")

  # Test for NaN results
  ssd_result <- loss_function(lambda, mu, data_sample, method = "ssd")
  nll_result <- loss_function(lambda, mu, data_sample, method = "nll")

  expect_false(is.nan(ssd_result))
  expect_false(is.nan(nll_result))
})
