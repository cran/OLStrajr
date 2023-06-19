test_that("OLStraj returns correct output for sample dataframe", {
  # Create a sample data frame
  df <- data.frame(id = c(1, 2, 3),
                   anti1 = c(1,3, 6),
                   anti2 = c(3, 5, 7),
                   anti3 = c(3, 4, 8),
                   anti4 = c(5, 6, 7))

  # Run the function
  result <- OLStraj(data = df,
                    varlist = c("anti1", "anti2", "anti3", "anti4"),
                    regtype = "lin",
                    int_bins = 5,
                    lin_bins = 5,
                    quad_bins = 5)

  # Assert that the output contains the expected elements
  expect_type(result, "list")
  expect_true("out_data" %in% names(result))
  expect_true("group_plots" %in% names(result))
  expect_true("individual_plots" %in% names(result))
})

test_that("OLStraj throws an error if data is not a dataframe", {
  # Assert that the function throws an error when the data argument is not a data frame
  expect_error(OLStraj(data = c(1, 2, 3)), "data must be a data frame.")
})

test_that("OLStraj throws an error if idvarname does not exist in data", {
  df <- data.frame(time = c(1, 2, 3), score = c(4, 5, 6))

  # Assert that the function throws an error when the idvarname argument does not exist in data
  expect_error(OLStraj(data = df), "idvarname must be a column name in the provided data frame.")
})

test_that("OLStraj throws an error if elements of varlist do not exist in data", {
  df <- data.frame(id = c(1, 2, 3), time = c(1, 2, 3), score = c(4, 5, 6))

  # Assert that the function throws an error when elements of varlist do not exist in data
  expect_error(OLStraj(data = df, varlist = c("nonexistent1", "nonexistent2")),
               "All elements of varlist must be column names in the provided data frame.")
})

test_that("OLStraj throws an error if inclmiss is not logical", {
  # Create a sample data frame
  df <- data.frame(id = c(1, 2, 3),
                   anti1 = c(1,3, 6),
                   anti2 = c(3, 5, 7),
                   anti3 = c(3, 4, 8),
                   anti4 = c(5, 6, 7))

  # Assert that the function throws an error when inclmiss is not logical
  expect_error(OLStraj(data = df, inclmiss = "not logical"), "inclmiss must be of type logical.")
})

# test_that("multiplication works", {
#   df <- data.frame(id = c(1,2,3,4,5),
#                    var1 = c(3,7,4,5,8),
#                    var2 = c(7,3,9,4,7),
#                    var3 = c(8,5,3,9,7),
#                    var4 = c(1,5,3,9,30))
#
#   olstraj_out <- OLStraj(data = df,
#                          varlist = c("var1", "var2", "var3", "var4"),
#                          regtype = "quad",
#                          int_bins = 5,
#                          lin_bins = 5,
#                          quad_bins = 5)
#
#   # Debugging parameters:
#   data = df
#   varlist = c("var1", "var2", "var3", "var4")
#   regtype = "both"
#   idvarname = "id"
#   predvarname = "time"
#   outvarname = "score"
#   timepts = c(0, 1, 2, 3)
#   inclmiss = "n"
#   level = "both"
#   numplot = NULL
#   hist = "y"
#   box = "y"
#   outds = TRUE
#
#   expect_equal(2 * 2, 4)
# })
