#' Absolute error
#'
#' Computes absolute error (actual retention rate - predicted retention rate) per cohort and period 
#' for given input data. Input data can either be used from the model (only available when trained on
#' data) or provided as a parameter.
#'
#' @param model sBG model 
#' @param df_data data frame that contains per cohort and period the survived customers (must have 
#' columns 'cohort', 'period' and 'survived_customers').
#' @return a matrix with absolute errors per cohort (rows) and period (columns)
#' @importFrom dplyr "%>%"
#' @export
absolute_error <- function(model, df_data=NA) {
  validate_input_model(model)
  input_data <- df_data
  
  if(is.na(df_data)){
    input_data <- model$df_data
  }
  
  validate_input_data(input_data)
  
  periods <- max(input_data$period)
  predicted_retention_rate <- data.frame(period = 1:(periods-1), predicted_retention_rate = retention_rates(model$params, 1:(periods-1)))

  df_data_retention_rate <- input_data %>%
    dplyr::group_by(cohort) %>%
    dplyr::mutate(actual_retention_rate = dplyr::lead(survived_customers, k = 1) / survived_customers) %>%
    tidyr::drop_na() 
  
  df_retention_rates <- merge.data.frame(df_data_retention_rate, predicted_retention_rate, by = 'period')  %>%
    dplyr::mutate(absolute_error = actual_retention_rate - predicted_retention_rate)
  
  actual_retention_rates <- tidyr::spread(df_retention_rates[,c('cohort', 'period', 'absolute_error')], key='period', value='absolute_error') %>%
    dplyr::select(-cohort)
  
  return (as.matrix(actual_retention_rates))
}

#' Performace metrics
#'
#' \code{performance_metrics} computes performance metrics for sBG model fit. Input data can 
#' either be used from the model (only available when trained on data) or provided as a parameter.
#'
#' @param model sBG model
#' @param df_data data frame that contains per cohort and period the survived customers (must have columns 
#' 'cohort', 'period' and 'survived_customers').
#' @param metrics list of performance metrics names you want to get (currently only absolute_error is available).
#' @return a vector with absolute error per period (period 1 is first element).
#' @export
performance_metrics <- function(model, df_data=NA, metrics = 'absolute_error') {
  if (metrics == 'absolute_error'){
    m_absolute_error <- absolute_error(model, df_data)
    
    return(colMeans(m_absolute_error, na.rm = TRUE))
  }
}
