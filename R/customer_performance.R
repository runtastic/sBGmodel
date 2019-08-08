#' Calculate churn rate
#'
#' \code{churn_rates} computes churn probabilities for given periods based on the sBG model.
#'
#' @param params a list of model parameters \code{alpha}, \code{beta}, \code{cp_alpha}, \code{cp_beta} and \code{cp_period}.
#' If it only contains \code{alpha} and \code{beta}, \code{cp_alpha} and \code{cp_beta} are set to \code{NA} and \code{cp_period} is set to 0.
#' @param period a numeric vector. Denotes for for which period(s) to calculate the churn probabilities.
#' @return a vector with churn probabilities per period(s)
#' @references http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.558.935&rep=rep1&type=pdf: Equation (7)
churn_rates <- Vectorize(function(params, period) {
  validate_input_params(params)
  if (any(period < 1)) {
    stop("period needs to be greater than 0.")
  }

  alpha <- params$alpha
  beta <- params$beta
  cp_period <- params$cp_period

  if (cp_period == 0 & period > cp_period) {
    if (period == cp_period + 1) {
      result <- alpha / (alpha + beta)
    } else {
      result <- churn_rates(params, period - 1) * (beta + period - cp_period - 2) / (alpha + beta + period - cp_period - 1)
    }
  } else{
    cp_alpha <- params$cp_alpha
    cp_beta <- params$cp_beta

    if (period == 1) {
      result <- cp_alpha / (cp_alpha + cp_beta)
    } else {
      result <- churn_rates(params, period - 1) * (cp_beta + period - 2) / (cp_alpha + cp_beta + period - 1)
    }
  }

  return(result)
}, vectorize.args = c("period"))

#' Calculate retention rate
#'
#' \code{retention_rates} computes retention probabilities for given periods based on the sBG model.
#'
#' @param params a list of model parameters \code{alpha}, \code{beta}, \code{cp_alpha}, \code{cp_beta} and \code{cp_period}.
#' If it only contains \code{alpha} and \code{beta}, \code{cp_alpha} and \code{cp_beta} are set to \code{NA} and \code{cp_period} is set to 0.
#' @param period a numeric vector. Denotes for for which period(s) to calculate the retention probabilities.
#' @return a vector with retention probabilities per period(s)
#' @references http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.558.935&rep=rep1&type=pdf: Equation (8)
retention_rates <- Vectorize(function(params, period){
  validate_input_params(params)
  if (any(period < 1)) {
    stop("period needs to be greater than 0.")
  }

  alpha <- params$alpha
  beta <- params$beta
  cp_period <- params$cp_period

  if (cp_period == 0) {
    result <- (beta + period - 1) / (alpha + beta + period - 1)
  } else {
    if(period <= cp_period) {
      cp_alpha <- params$cp_alpha
      cp_beta <- params$cp_beta

      result <- (cp_beta + period - 1) / (cp_alpha + cp_beta + period - 1)
    } else {
      # if cp_period > 0: start with calculation from cp_period
      result <- (beta + period - (cp_period + 1) - 1) / (alpha + beta + period - (cp_period + 1) - 1)
    }
  }

  return(result)
}, vectorize.args = c("period"))

#' Calculate survival rate
#'
#' \code{survival_rates} computes survival probabilities for given periods based on sBG model.
#'
#' @param params a list of model parameters \code{alpha}, \code{beta}, \code{cp_alpha}, \code{cp_beta} and \code{cp_period}.
#' If it only contains \code{alpha} and \code{beta}, \code{cp_alpha} and \code{cp_beta} are set to \code{NA} and \code{cp_period} is set to 0.
#' @param period a numeric vector. Denotes for for which period(s) to calculate the survival probabilities.
#' @return a vector with survival probabilities per period(s)
#' @references http://brucehardie.com/notes/017/sBG_estimation.pdf: Equation (2)
survival_rates = Vectorize(function(params, period) {
  validate_input_params(params)
  if (any(period < 1)) {
    stop("period needs to be greater than 0.")
  }

  if (period == 1) {
    result = 1 - churn_rates(params, 1)
  } else if (period > 1) {
    result <- survival_rates(params, period - 1) - churn_rates(params, period)
  }

  return(result)
}, vectorize.args = c("period"))


#' Customer performance rates
#'
#' \code{customer_rates} calculates churn, retention and survival rate
#' using the parameters from model input for periods between start_period and end_period.
#'
#' @param model a sBG model
#' @param start_period a number denoting the period to start calculation from
#' @param end_period a number denoting the period to end calculation at
#' @param metrics vector with customer performance rate to compute.
#' Default is a vector contain all three rates ('churn', 'retention' and 'survival').
#' @return a data frame with column period and given customer performance rate
#' @export
customer_rates <- function(model, start_period, end_period, metrics = c('churn', 'retention', 'survival')) {
  validate_input_model(model)
  start_period <- as.numeric(start_period)
  end_period <- as.numeric(end_period)
  metrics <- as.character(metrics)
  if (length(metrics) > 3) {
    stop("metrics can only contain maximum three metrics.")
  }
  if (any(!metrics %in% c('churn', 'retention', 'survival'))) {
    stop("metrics can only contain the following metrics: 'churn', 'retention', 'survival'.")
  }
  if (start_period > end_period) {
    stop("start_period needs to be smaller or equal to end_period.")
  }
  if (start_period <= 0) {
    stop("start_period needs to be greater than 0.")
  }

  periods <- start_period:end_period
  result <- data.frame(period = periods)

  if('churn' %in% metrics) {
    result$churn_rate <- churn_rates(model$params, periods)
  }
  if('retention' %in% metrics){
    result$retention_rate <- retention_rates(model$params, periods)
  }
  if('survival' %in% metrics){
    result$survival_rate <- survival_rates(model$params, periods)
  }
  return(result)
}

#' Calculate discounted expected residual lifetime
#'
#' \code{derl} computes discounted expected residual lifetime (DERL) for given customer's contract period(s)
#'
#' @param model a sBG model
#' @param period a vector with period(s) for which DERL should be calculated. (customer has made period-1 renewals). Default is set to 1.
#' @param discount_rate a number between 0 and 1 denoting the discount rate. Default is set to 0.25 (recommended value for private businesses).
#' @return a vector with discounted expected residual lifetime per given period
#' @references http://brucehardie.com/papers/022/fader_hardie_mksc_10.pdf Equation (6)
#' @export
derl <- Vectorize(function(model, period = 1, discount_rate = 0.25) {
  validate_input_model(model)
  params <- model$params
  validate_input_params(params)

  period <- as.numeric(period)
  discount_rate <- as.numeric(discount_rate)

  if (discount_rate <= 0 | discount_rate > 1 | length(discount_rate) > 1) {
    stop("discount_rate needs to be a number in ]0,1].")
  }
  if (any(period < 1)) {
    stop("period needs to be greater or equal than 1.")
  }

  alpha <- params$alpha
  beta <- params$beta
  cp_period <- params$cp_period

  if(cp_period == 0 | period > cp_period) {
    derl <- (beta + period - cp_period - 1) / (alpha + beta + period - cp_period - 1) *
      gsl::hyperg_2F1(1, beta + period - cp_period, alpha + beta + period - cp_period, 1/(1 + discount_rate))
  }else{
    cp_alpha <- params$cp_alpha
    cp_beta <- params$cp_beta

    derl <- (cp_beta + period - 1) / (cp_alpha + cp_beta + period - 1) *
      gsl::hyperg_2F1(1, cp_beta, cp_alpha + cp_beta, 1 / (1 + discount_rate))
  }

  return(derl)
}, vectorize.args = c("period"))
