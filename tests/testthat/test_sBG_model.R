context("sBG_model")

##########################
# TEST: sBG_model #
##########################

##### Successful Tests #####

test_that("sBG_model: all 3 parameters are given and cp_period is 0.", {
  alpha <- 2
  beta <- 3
  cp_period <- 0
  model <- sBG_model(alpha = alpha, beta = beta, cp_period = cp_period)

  expect_is(model, "list")
  expect_false(is.na(model[1])) 
  expect_true(all(names(model) %in% c('params', 'df_data', 'metrics')))
  expect_true(all(names(model$params) %in% c('alpha', 'beta', 'cp_period')))
})

test_that("sBG_model: all 5 parameters are given and cp_period is 1.", {
  alpha <- 2
  beta <- 3
  cp_alpha <- 3
  cp_beta <- 3
  cp_period <- 1
  model <- sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period)

  expect_is(model, "list")
  expect_true(all(names(model) %in% c('params', 'df_data', 'metrics')))
  expect_true(all(names(model$params) %in% c('alpha', 'beta', 'cp_period', 'cp_alpha', 'cp_beta')))
})


##### Error Tests #####

test_that("sBG_model: error when cp_period is negative.", {
  alpha <- 2
  beta <- 3
  cp_alpha <- 3
  cp_beta <- 2
  cp_period <- -1

  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when cp_alpha and cp_beta not given but cp_period is not 0.", {
  alpha <- 2
  beta <- 3
  cp_period <- 1

  expect_error(sBG_model(alpha = alpha, beta = beta, cp_period = cp_period))
})

test_that("sBG_model: error when cp_alpha and cp_beta given but cp_period is 0.", {
  alpha <- 2
  beta <- 3
  cp_alpha <- 3
  cp_beta <- 3
  cp_period <- 0

  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when cp_alpha is given but cp_beta is not.", {
  alpha <- 2
  beta <- 3
  cp_alpha <- 3
  cp_beta <- NA
  cp_period <- 1

  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when cp_beta is given but cp_alpha is not.", {
  alpha <- 2
  beta <- 3
  cp_alpha <- NA
  cp_beta <- 3
  cp_period <- 0

  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when cp_alpha and cp_beta are given and cp_period is 0", {
  alpha <- 2
  beta <- 3
  cp_alpha <- 3
  cp_beta <- 2
  cp_period <- 0

  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when parameter alpha is non numeric.", {
  alpha <- 'two'
  beta <- 3
  cp_alpha <- 3
  cp_beta <- 3
  cp_period <- 1

  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when parameter alpha is NA.", {
  alpha <- NA
  beta <- 3
  cp_alpha <- 3
  cp_beta <- 3
  cp_period <- 1
  
  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when parameter beta is non numeric.", {
  alpha <- 2
  beta <- 'three'
  cp_alpha <- 3
  cp_beta <- 3
  cp_period <- 1
  
  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when parameter beta is NA.", {
  alpha <- 2
  beta <- NA
  cp_alpha <- 3
  cp_beta <- 3
  cp_period <- 1
  
  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when parameter cp_alpha is non numeric.", {
  alpha <- 2
  beta <- 3
  cp_alpha <- 'three'
  cp_beta <- 3
  cp_period <- 1
  
  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when parameter cp_beta is non numeric.", {
  alpha <- 2
  beta <- 3
  cp_alpha <- 3
  cp_beta <- 'three'
  cp_period <- 1
  
  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when parameter cp_period is NA.", {
  alpha <- 2
  beta <- 3
  cp_alpha <- 3
  cp_beta <- 3
  cp_period <- NA
  
  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when parameter cp_period is non numeric.", {
  alpha <- 2
  beta <- 3
  cp_alpha <- 3
  cp_beta <- 3
  cp_period <- 'one'
  
  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})

test_that("sBG_model: error when alpha and beta are negative.", {
  alpha <- -2
  beta <- -3
  cp_alpha <- 3
  cp_beta <- 2
  cp_period <- 1

  expect_error(sBG_model(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period))
})
