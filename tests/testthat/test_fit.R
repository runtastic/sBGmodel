context("fit")

#################
# TEST: fit #
#################

##### Successful Tests #####
test_that("fit: df_data with correct columns.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)

  expect_is(model, "list")
  expect_false(is.na(model[1]))
  expect_false(is.na(model[2]))
  expect_false(is.na(model[3]))
  expect_true(all(names(model) %in% c('params', 'df_data', 'metrics')))
  expect_true(all(names(model$params) %in% c('alpha', 'beta', 'cp_period')))
})

test_that("fit: df_data with cp_period is 1.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data, cp_period = 1)

  expect_is(model, "list")
  expect_false(is.na(model[1]))
  expect_false(is.na(model[2]))
  expect_false(is.na(model[3]))  
  expect_true(all(names(model) %in% c('params', 'df_data', 'metrics')))
  expect_true(all(names(model$params) %in% c('alpha', 'beta', 'cp_period', 'cp_alpha', 'cp_beta')))
})

test_that("fit: df_data with additional columns.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109), month = c(1,2,3))
  model <- fit(df_data, cp_period = 0)

  expect_is(model, "list")
  expect_false(is.na(model[1]))
  expect_false(is.na(model[2]))
  expect_false(is.na(model[3]))
  expect_true(all(names(model) %in% c('params', 'df_data', 'metrics')))
  expect_true(all(names(model$params) %in% c('alpha', 'beta', 'cp_period')))
})

##### Error Tests #####

test_that("fit: error when column 'cohort' is missing.", {
  df_data <- data.frame( period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))

  expect_error(fit(df_data))
})

test_that("fit: error when column 'period' is missing.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)), survived_customers = c(158, 98, 53, 118, 85, 109))

  expect_error(fit(df_data))
})

test_that("fit: error when column 'survived_customers' is missing.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1))

  expect_error(fit(df_data))
})

test_that("fit: error when df_data is null.", {
  df_data <- NULL

  expect_error(fit(df_data))
})

test_that("fit: error when df_data has only one row.", {
  df_data <- data.frame(cohort = 1, period = 1, survived_customers = 30)

  expect_error(fit(df_data))
})

test_that("fit: error when df_data exists but is empty.", {
  df_data <- data.frame(cohort = numeric(), period = numeric(), survived_customers = numeric())
  expect_error(fit(df_data))
})

test_that("fit: error when column 'period' invalid data type.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)), period = c('d', 'e', 'f'), survived_customers = c(158, 98, 53, 118, 85, 109))
  df_data$period <- as.character(df_data$period)

  expect_error(fit(df_data))
})

test_that("fit: error when column 'survived_customers' invalid data type (factor).", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c('one', 'two', 'three'))

  expect_error(fit(df_data))
})

test_that("fit: error when column 'survived_customers' invalid data type (character).", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c('one', 'two', 'three'))
  df_data$survived_customers <- as.character(df_data$period)

  expect_error(fit(df_data))
})

test_that("fit: error when df_data contains NA values.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(NA, 2), rep(3, 1)), period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  df_data$period[1] <- NA

  expect_error(fit(df_data))
})

test_that("fit: error when cp_period is negative.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))

  expect_error(fit(df_data, cp_period = -1))
})

test_that("fit: error when cp_period is greater than the maximal period.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))

  expect_error(fit(df_data, cp_period = 4))
})

test_that("fit: error when cp_period is not numeric.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))

  expect_error(fit(df_data, cp_period = 'one'))
})
