context('customer_rates')

########################################
# TEST: customer_rates #
########################################

##### Successful Tests #####

test_that("customer_rates: valid model with df_data is NA and metrics is NA.", {
  model <- sBG_model(alpha = 1, beta = 3, cp_period = 0)
  cust_perf_rate <- customer_rates(model, start_period = 1, end_period = 7)

  expect_true(all(colnames(cust_perf_rate) %in% c('period', 'churn_rate', 'retention_rate', 'survival_rate')))
  expect_equal(nrow(cust_perf_rate), 7)
})

test_that("customer_rates: valid model with all three components, 3 metrics (default) and start period < end period.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)
  cust_perf_rate <- customer_rates(model, start_period = 1, end_period = 7)

  expect_true(all(colnames(cust_perf_rate) %in% c('period', 'churn_rate', 'retention_rate', 'survival_rate')))
  expect_equal(nrow(cust_perf_rate), 7)
})

test_that("customer_rates: valid model with all three components and cp_period > 0, 3 metrics (default) and start period < end period.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data, cp_period = 1)
  cust_perf_rate <- customer_rates(model, start_period = 1, end_period = 7)

  expect_true( all(colnames(cust_perf_rate) %in% c('period', 'churn_rate', 'retention_rate', 'survival_rate')))
  expect_equal(nrow(cust_perf_rate), 7)
})

test_that("customer_rates: valid model with all three components and 1 metric.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)
  cust_perf_rate <- customer_rates(model, start_period = 1, end_period = 7, metrics = c('survival'))

  expect_true( all(colnames(cust_perf_rate) %in% c('period', 'survival_rate')))
  expect_equal(nrow(cust_perf_rate), 7)
})

test_that("customer_rates: valid model with all three components and start period = end period.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)
  cust_perf_rate <- customer_rates(model, start_period = 7, end_period = 7)

  expect_true( all(colnames(cust_perf_rate) %in% c('period', 'churn_rate', 'retention_rate', 'survival_rate')))
  expect_equal(nrow(cust_perf_rate), 1)
})

test_that("customer_rates: valid model with all three components and cp_period > 0 and start period = end period.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))
  model <- fit(df_data)
  cust_perf_rate <- customer_rates(model, start_period = 7, end_period = 7)

  expect_true( all(colnames(cust_perf_rate) %in% c('period', 'churn_rate', 'retention_rate', 'survival_rate')))
  expect_equal(nrow(cust_perf_rate), 1)
})

##### Error Tests ######

test_that("customer_rates: error when model is NA.", {
  expect_error(customer_rates(NA, start_period = 1, end_period = 7))
})

test_that("customer_rates: error when start_period is NA.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))

  model <- fit(df_data)
  expect_error(customer_rates(model, start_period = NA, end_period = 7))
})

test_that("customer_rates: error when end_period is NA.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))

  model <- fit(df_data)
  expect_error(customer_rates(model, start_period = 1, end_period = NA))
})

test_that("customer_rates: error when invalid metric name.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))

  model <- fit(df_data)
  expect_error(customer_rates(model, start_period = 1, end_period = 7, metric = c('other')))
})

test_that("customer_rates: error when start_period > end_period.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))

  model <- fit(df_data)
  expect_error(customer_rates(model, start_period = 2, end_period = 1))
})

test_that("customer_rates: error when start_period = 0.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))

  model <- fit(df_data)
  expect_error(customer_rates(model, start_period = 0, end_period = 1))
})

test_that("customer_rates: error when start_period non numeric.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))

  model <- fit(df_data)
  expect_error(customer_rates(model, start_period = 'start_period', end_period = 7))
})

test_that("customer_rates: error when end_period non numeric.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))

  model <- fit(df_data)
  expect_error(customer_rates(model, start_period = 1, end_period = 'end_period'))
})

test_that("customer_rates: error when metrics contain non character.", {
  df_data <- data.frame(cohort = c(rep(1, 3), rep(2, 2), rep(3, 1)),  period = c(1:3, 1:2, 1), survived_customers = c(158, 98, 53, 118, 85, 109))

  model <- fit(df_data)
  expect_error(customer_rates(model, start_period = 1, end_period = 7, metrics = c(1)))
})
