context('derl')

###################
# TEST: derl #
###################

##### Successful Tests #####

test_that("derl: valid model with df_data is NA and metrics is NA.", {
  model <- sBG_model(alpha = 1, beta = 0.5, cp_period = 0)
  derl <- derl(model, 1)

  expect_true(is.numeric(derl))
})

test_that("derl: valid model with all three components and cp_period is 0.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data, cp_period = 0)
  derl <- derl(model, 1)

  expect_true(is.numeric(derl))
})

test_that("derl: valid model with all three components and cp_period greater than 0.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data, cp_period = 1)
  derl <- derl(model, period = 1)

  expect_true(is.numeric(derl))
})

##### Error Tests #####

test_that("derl: error when model is NA.", {
  expect_error(derl(model = NA, period = 1))
})

test_that("derl: error when period is NA.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)

  expect_error(derl(model = model, period = NA))
})

test_that("derl: error when discount_rate is NA.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)

  expect_error(derl(model = model, period = 1, discount_rate = NA))
})

test_that("derl: error when period is non numeric.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)

  expect_error(derl(model, period = 'period'))
})

test_that("derl: error when period smaller than 1.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)

  expect_error(derl(model, period = 0))
})

test_that("derl: error when discount_rate non numeric.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)

  expect_error(derl(model, period = 1, discount_rate = 'dr'))
})

test_that("derl: error when discount_rate <= 0.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)

  expect_error(derl(model, period = 1, discount_rate = 0))
})

test_that("derl: error when discount_rate greater than 1.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)

  expect_error(derl(model, period = 1, discount_rate = 1.1))
})
