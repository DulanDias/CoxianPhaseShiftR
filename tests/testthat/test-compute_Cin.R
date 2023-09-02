context("Testing compute_Cin")

test_that("compute_Cin returns correct values", {

  lambda <- c(0.5, 0.3, 0)
  mu <- c(0.2, 0.4, 0.6)

  # Test for i = 1, n = 2
  Cin_1_2 <- (lambda[2] + mu[2]) / (lambda[2] + mu[2] - lambda[1] - mu[1])
  expect_equal(compute_Cin(lambda, mu, 1, 2), Cin_1_2)

  # Test for i = 2, n = 3
  Cin_2_3 <- (lambda[1] + mu[1]) / (lambda[1] + mu[1] - lambda[2] - mu[2]) *
    (lambda[3] + mu[3]) / (lambda[3] + mu[3] - lambda[2] - mu[2])
  expect_equal(compute_Cin(lambda, mu, 2, 3), Cin_2_3)

})
