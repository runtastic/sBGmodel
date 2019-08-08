#' Transform input for single cohort sBG model 
#'
#' \code{single_cohort_data} creates a data frame which can be used to apply the sBG model for a single cohort.
#' @importFrom dplyr "%>%" 
#' @param df_data a data frame that contains per cohort and period the survived customers (must have columns 'cohort', 'period' and 'survived_customers').
#' @return a data frame that contains per cohort and period the survived and lost customers (excluding first period).
single_cohort_data <- function(df_data){
  df_single_cohort_data <- df_data %>%  
    dplyr::mutate(lost_customers = dplyr::lag(survived_customers) - survived_customers) %>% 
    tidyr::drop_na()
  rownames(df_single_cohort_data) <- NULL # reset index
  
  return(df_single_cohort_data)
}

#' Transform dataframe to triangular matrix for CLV model.
#'
#' \code{triangular_matrix} computes lower triangular matrix for the CLV model.
#' @importFrom dplyr "%>%" 
#' @param df_data a data frame that contains per cohort and period the survived customers (must have columns 'cohort', 'period' and 'survived_customers').
#' @return a lower triangular matrix containing number of customers per cohort and period
triangular_matrix <- function(df_data) {
  validate_input_data(df_data)

  cohort_data <- dplyr::group_by(df_data, cohort, period) %>% 
    dplyr::summarise(survived_customers = sum(survived_customers)) %>% 
    dplyr::ungroup() %>% 
    tidyr::spread(period, survived_customers, fill = 0) %>% 
    dplyr::select(-cohort)

  return(as.matrix(cohort_data))
}

#' sBG model parameters
#'
#' \code{single_point_parameters} estimates parameters for sBG model based on variance and mean.
#'
#' @param var variance
#' @param mu mean
#' @return list with derived parameters for sBG model
single_point_parameters <- function(var, mu) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  
  return(c(alpha = alpha, beta = beta))
}

#' Parameter list for sBG model.
#'
#' \code{sBG_parameters} takes the parameter in the argument and combines them to one list as it is also in the output of the model
#'
#' @param alpha alpha paramter for sBG model
#' @param beta beta paramter for sBG model
#' @param cp_alpha alpha paramter for sBG model for periods until changepoint (when cp_period > 0)
#' @param cp_beta beta paramter for sBG model for periods until changepoint (when cp_period > 0)
#' @param cp_period changepoint period (default is 0, which means no change point)
#' @return list with parameters for sBG model
sBG_parameters <- function(alpha, beta, cp_alpha = NA, cp_beta = NA, cp_period=0) {
  cp_period <- as.numeric(cp_period)
  alpha <- as.numeric(alpha)
  beta <- as.numeric(beta)

  if (is.na(alpha) | is.na(beta)) {
    stop("alpha and beta must be numeric and not NA.")
  }

  if (cp_period < 0) {
    stop("cp_period needs to be greater or equal to 0")
  }

  params <- list(alpha = alpha, beta = beta, cp_period = cp_period)

  if ((is.na(cp_alpha) & !is.na(cp_beta)) | (is.na(cp_beta) & !is.na(cp_alpha))) {
    stop("cp_alpha and cp_beta need to be both given.")
  }

  if(!any(is.na(c(cp_alpha, cp_beta)))){
    if(!is.numeric(c(cp_alpha, cp_beta))){
      stop("cp_alpha and cp_beta must be numeric.")
    }
    params <- append(params, c(cp_alpha = cp_alpha, cp_beta = cp_beta))
  }

  validate_input_params(params)
  return(params)
}

#' Log-likelihood for single-cohort data
#'
#' \code{single_cohort_ll} computes log-likelihood function for single-cohort data. 
#' 
#' @param df_single_cohort_data a data.frame containing number of lost and survived_customers per period. 
#' Required columns:  columns 'period', 'lost_customers' and 'survived_customers'.
#' @param params a list of parameters \code{alpha}, \code{beta} to calculate log-likelihood function for.
#' @return Log-likelihood based on single cohort data and given input parameters.
#' @references http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.558.935&rep=rep1&type=pdf, formula B3.
single_cohort_ll <- function(df_single_cohort_data, params) {
  periods <- nrow(df_single_cohort_data)
  param_list <- sBG_parameters(alpha = params['alpha'], beta = params['beta'], cp_period = 0)
  
  return (- (sum(df_single_cohort_data$lost_customers * log(churn_rates(param_list, 1:periods))) + 
               df_single_cohort_data[periods,'survived_customers'] * log(survival_rates(param_list, periods))))
}

#' Calculate log-likelihood for multi-cohort data
#'
#' \code{multi_cohort_ll} computes log-likelihood function for a multi-cohort data. Input matrix must be a full lower triangular matrix.
#'
#' @param m_data a lower triangular matrix containing number of customers per period and cohort. Needs to be squared with more than 1 column.
#' @param params a list of parameters \code{alpha}, \code{beta} to calculate log-likelihood function for.
#' @return Log-likelihood based on multiple cohort data and given input parameters.
#' @references http://brucehardie.com/notes/017/sBG_estimation.pdf: Equation (3)
multi_cohort_ll <- function(m_data, params) {
  m_data <- as.matrix(m_data)

  I <- nrow(m_data) # number of cohorts
  J <- ncol(m_data) # number of periods

  if (I != J) {
    stop("Log-likelihood can only be calculated for a squared matrix.")
  }
  if (J == 1) {
    stop("Log-likelihood can only be calculated for more than one period.")
  }

  res <- 0
  param_list <- sBG_parameters(alpha = params['alpha'], beta = params['beta'], cp_period = 0)

  for (i in 1:(I-1)) {
    for (j in 1:(J - i)) {
      res <- res + (m_data[i, j] - m_data[i, j + 1]) * log(churn_rates(param_list, j))
    }
    res <- res + m_data[i, J - i + 1] * log(survival_rates(param_list, J - i))
  }

  return(-res)
}

#' Fit sBG Model
#'
#' \code{fit} fits a sBG model for CLV. It estimates the parameter \code{alpha}, \code{beta}, \code{cp_alpha} and \code{cp_beta}
#' The model is evaluated by calculating the median and mean of the root mean-square error between the predicted and actual retention rate per cohort.
#'
#' @param df_data a data frame must contain the columns 'cohort', 'period' and 'survived_customers'.
#' @param cp_period a number which denotes the period after which any change in the behaviour is happening.
#' If it is equal to 0, no change is happening. Default is set as 0.
#' Example: If period one shows a different behaviour as the other periods, cp_period is set as 1.
#' @return list with the following components: 
#' 'params' a list with estimated parameter, 
#' 'df_data' the data frame used for fitting.
#' @export
fit <- function(df_data, cp_period = 0){
  validate_input_data(df_data)
  cp_period <- as.numeric(cp_period)
  if (cp_period < 0) {
    stop("cp_period needs to be greater or equal to 0")
  }
  if (cp_period > max(df_data$period)) {
    stop("cp_period needs to be smaller than the maximum number of periods.")
  }

  params1 <- NA
  
  # check if single or multi_cohort data is available
  if( length(unique(df_data$cohort)) == 1) {
    if(cp_period > 0){
      stop("Changepoint period only supported for multi-cohorts.")
    }
    
    df_single_cohort_data <- single_cohort_data(df_data)
    
    tryCatch({
    params <- stats::optim(c(alpha = 1, beta = 1), single_cohort_ll, df_single_cohort_data = df_single_cohort_data,
                    method = 'L-BFGS-B', lower=rep(1/1000000, 2)) 
    }, error = function(err) {
      stop(paste("Model could not be fitted! ", err))
    })
  }else {
    # multi cohort
    m_data <- triangular_matrix(df_data)
    
    if(cp_period > 0) {
      if(cp_period == 1) {
        m_data <- as.matrix(m_data)
        
        if (ncol(m_data) < 2) {
          stop("df_data needs to contain at least two columns.")
        }
        
        churn_rate <- (m_data[,1] - m_data[,2]) / m_data[,1]
        
        var <- var(churn_rate)
        mu <- mean(churn_rate)
        
        params1 <- single_point_parameters(var, mu)
      } else{
        #cp_period > 1 => optimize parameters for sub-data
        cp_periods <- 1:cp_period
        params1 <- stats::optim(c(alpha = 1, beta = 1), multi_cohort_ll, m_data = m_data[cp_periods, cp_periods],
                         method = 'L-BFGS-B', lower=rep(1/1000000, 2))
        params1 <- params1$par
      }
    }
    # calculate alpha and beta for all periods
    # if cp_period > 0 => cut off cp_period columns and rows
    tryCatch({
      params <-
        stats::optim(c(alpha = 1, beta = 1), multi_cohort_ll, m_data = m_data[1:(nrow(m_data) - cp_period), (cp_period + 1):ncol(m_data)],
              method = 'L-BFGS-B', lower=rep(1/1000000, 2))
      }, error = function(err) {
        stop(paste("Model could not be fitted! ", err))
        }
    )
  }
  
  params <- params$par
  
  if(any(is.na(params1))){
    params <- sBG_parameters(alpha = params[['alpha']], beta = params[['beta']], cp_period = 0)
  }else{
    params <- sBG_parameters(params[['alpha']], params[['beta']], params1[['alpha']], params1[['beta']], cp_period=cp_period)
  }
  
  list(params = params,
       df_data = df_data)
}

#' Initialize sBG Model
#' 
#' Initialize a sBG model with parameter list
#'
#' @param alpha alpha parameter for sBG model
#' @param beta beta parameter for sBG model
#' @param cp_alpha alpha parameter for sBG model for periods until changepoint (when cp_period > 0)
#' @param cp_beta beta parameter for sBG model for periods until changepoint (when cp_period > 0)
#' @param cp_period changepoint period
#' @return list with the following components: 'params' a list with given parameter
#' @export
sBG_model <- function(alpha, beta, cp_alpha = NA, cp_beta = NA, cp_period=0) {
  if (alpha <= 0 |beta <= 0 ){
    stop("alpha and beta must be greater 0.")
  }
  params <- sBG_parameters(alpha = alpha, beta = beta, cp_alpha = cp_alpha, cp_beta = cp_beta, cp_period = cp_period)
  model <- list(params = params)

  return(model)
}
