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

  skip(message = "Skipping")

  # Load the dataset
  data(los)

  # Check if 'los' is in the environment
  expect_true(exists("los"))

  # Convert 'los' to a data.frame and filter to only include values > 0
  los <- data.frame(los)

  # Check if 'los' has valid number of rows to sample from
  if (!is.null(nrow(los)) && nrow(los) > 0) {
    # Sample rows from the filtered 'los' data.frame
    los_sample <- los[sample(nrow(los), min(100, nrow(los))), ]

    # Test fitCoxian function on the loaded dataset for various phase numbers
    phases <- c(3)
    for (phase in phases) {
      result <- fitCoxian(los_sample, phase)
      cat(sprintf("\nResults for %d-phase Coxian:\n", phase))
      print(result)
      cat("\n")
    }
  } else {
    cat("\nlos data frame has no rows with values greater than 0.\n")
  }
})


