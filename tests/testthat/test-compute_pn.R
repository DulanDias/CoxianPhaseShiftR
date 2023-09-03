library(testthat)
context("Testing compute_pn")

test_that("compute_pn returns correct values", {

  lambda <- c(0.5, 0.3, 0)
  mu <- c(0.2, 0.4, 0.6)

  # Test for n = 1
  expect_equal(compute_pn(lambda, mu, 1), mu[1] / (lambda[1] + mu[1]))

  # Test for n = 2
  pn_2 <- (lambda[1] / (lambda[1] + mu[1])) * (mu[2] / (lambda[2] + mu[2]))
  expect_equal(compute_pn(lambda, mu, 2), pn_2)

})
