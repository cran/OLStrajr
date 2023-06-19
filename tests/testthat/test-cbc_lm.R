test_that("Test that cbc_lm returns an error for invalid input", {

  # Define some example data
  df <- data.frame(ids = rep(1:5, 5),
                   vals = stats::rnorm(25),
                   outs = stats::rnorm(25, 10, 25))

  # Test that error is returned when 'data' is not a data.frame
  expect_error(cbc_lm(data = c(1, 2, 3), formula = outs ~ vals, .case = "ids"), "'data' must be a data frame.")

  # Test that error is returned when 'formula' is not coercible to a formula
  expect_error(cbc_lm(data = df, formula = "not a formula", .case = "ids"), "'formula' must be a valid formula or a string coercible to a formula.")

  # Test that error is returned when '.case' is not a column name in 'data'
  expect_error(cbc_lm(data = df, formula = outs ~ vals, .case = "nonexistent_column"), "'.case' must be a valid column name in 'data'.")

  # Test that error is returned when 'n_bootstrap' is not a positive integer
  expect_error(cbc_lm(data = df, formula = outs ~ vals, .case = "ids", n_bootstrap = -5), "'n_bootstrap' must be a positive integer.")

  # Test that error is returned when 'lm_options', 'boot_options', or 'boot.ci_options' is not a list
  expect_error(cbc_lm(data = df, formula = outs ~ vals, .case = "ids", lm_options = c(1, 2, 3)), "'lm_options', 'boot_options', and 'boot.ci_options' must be lists.")

  # Test that error is returned when 'na.rm' is not a logical value
  expect_error(cbc_lm(data = df, formula = outs ~ vals, .case = "ids", na.rm = "not a logical value"), "'na.rm' must be a logical value.")
})

test_that("Test that cbc_lm runs correctly with valid input", {

  # Define some example data
  df <- data.frame(ids = rep(1:5, 5),
                   vals = stats::rnorm(25),
                   outs = stats::rnorm(25, 10, 25))

  # Test that the function runs without error
  expect_error(cbc_lm(data = df, formula = outs ~ vals, .case = "ids"), NA)

  # Test that the function returns an object of class "cbc_lm"
  expect_s3_class(cbc_lm(data = df, formula = outs ~ vals, .case = "ids"), "cbc_lm")

  # More specific tests can be added based on the expected outputs of the function
})


test_that("Test that cbc_lm gives expected output", {

  # Define some example data
  df <- data.frame(ids = rep(1:5, 5),
                   vals = stats::rnorm(25),
                   outs = stats::rnorm(25, 10, 25))


  res <- cbc_lm(data = df, formula = outs ~ vals, .case = "ids")

  expect_equal(length(res), 2)


  expect_equal(length(res$models), 5)

  expect_equal(length(summary(res)), 6)

  s_res <- summary(res)

  expect_null(s_res$bm_coef)

})
