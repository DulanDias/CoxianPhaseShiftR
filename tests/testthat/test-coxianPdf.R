library(testthat)
context("Testing coxianPdf")

test_that("coxianPdf returns a numeric value for valid inputs", {
  lambda <- c(0.5, 0.3, 0)
  mu <- c(0.2, 0.4, 0.6)
  t_value <- 1
  result <- coxianPdf(t_value, lambda, mu)
  expect_type(result, "double")
})

test_that("coxianPdf handles boundary conditions", {
  lambda <- c(0.5, 0.3, 0)
  mu <- c(0.2, 0.4, 0.6)

  # When t is 0, the PDF should be 0
  result <- coxianPdf(0, lambda, mu)
  expect_equal(result, 0)

})

test_that("coxianPdf handles errors gracefully", {
  lambda <- c(0.5, 0.3)
  mu <- c(0.2, 0.4, 0.6)

  expect_error(coxianPdf(1, lambda, mu), "Length of lambda and mu should be the same")

})

test_that("coxianPdf does not produce NaN or 0 values", {

  # Sample lambda and mu values
  lambda_sample <- c(0.5, 0.3)
  mu_sample <- c(0.2, 0.4)

  # Generate sample data
  data_sample <- rexp(100, rate = 0.5)

  # Compute the PDF values
  pdf_values <- sapply(data_sample, function(x) {
    coxianPdf(x, lambda_sample, mu_sample)
  })

  # Check for NaN values
  expect_false(any(is.nan(pdf_values)),
               info = "coxianPdf produced NaN values.")

  # Check for 0 values
  expect_false(any(pdf_values == 0),
               info = "coxianPdf produced 0 values.")
})
