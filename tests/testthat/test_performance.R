context('absolute_error')

##############################
# TEST: absolute_error #
##############################

##### Successful Tests #####

test_that("absolute_error: default model.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)
  error_per_period <- absolute_error(model)

  expect_equal(ncol(error_per_period), 2)
  expect_equal(nrow(error_per_period), 2)
})

test_that("absolute_error: model with cp_period > 0.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data, cp_period = 1)
  error_per_period <- absolute_error(model)

  expect_equal(ncol(error_per_period), 2)
  expect_equal(nrow(error_per_period), 2)
})

##### Error Tests #####

test_that("absolute_error: error when model is NA.", {
  expect_error(absolute_error(model = NA))
})
