#' Validate input data for sBG model
#'
#' \code{validate_input_data} receives a dataframe and validated if all columns are given and no NA values are included.
#'
#' @param df_data a data frame must contain the columns 'cohort', 'period' and 'survived_customers'.
validate_input_data <- function(df_data) {
  if (!is.data.frame(df_data)) {
    stop("df_data needs to be a data frame.")
  }
  if (!all(c('cohort', 'period', 'survived_customers') %in% colnames(df_data))) {
    stop("df_data needs to contain the following columns: 'cohort', 'period' and 'survived_customers'.")
  }
  if (any(is.na(df_data$cohort)) | any(is.na(df_data$period))) {
    stop("Columns 'cohort' and 'period' of df_data can not contain NA values.")
  }
  # period and survived_customers must be numeric
  if(!is.numeric(df_data$period)){
    stop("Column period must be numeric.")
  }
  
  df_data$survived_customers <- as.numeric(df_data$survived_customers)
}

#' Validate input parameter for sBG model
#'
#' \code{validate_input_params} receives list with parameters and validates if all of them are correctly given.
#'
#' @param params a list of parameters (\code{alpha}, \code{beta}, \code{cp_alpha}, \code{cp_beta} and \code{cp_period}).
validate_input_params <- function(params) {
  if (length(params) == 3) {
    if (!all(c('alpha', 'beta', 'cp_period') %in% names(params))) {
      stop("Invalid params.")
    }
    alpha <- as.numeric(params$alpha)
    beta <- as.numeric(params$beta)
    cp_period <- as.numeric(params$cp_period)
    if(params$cp_period > 0){
      stop("cp_period must be 0 if cp_alpha and cp_beta are not given.")
    }
  } else if (length(params) == 5) {
    if (!all(c('alpha', 'beta', 'cp_alpha', 'cp_beta', 'cp_period') %in% names(params))) {
      stop("Invalid params.")
    }
    alpha <- as.numeric(params$alpha)
    beta <- as.numeric(params$beta)
    cp_alpha <- as.numeric(params$cp_alpha)
    cp_beta <- as.numeric(params$cp_beta)
    cp_period <- as.numeric(params$cp_period)
    if(params$cp_period <= 0){
      stop("cp_period must be greater 0 if cp_alpha and cp_beta are given.")
    }
  } else {
    stop("params needs to be of length 3 or 5.")
  }
}

#' Validate input of sBG model
#'
#' \code{validate_input_model} receives a model and validates if its structure is correct (i.e. if it contains the right components).
#'
#' @param model sBG model.
validate_input_model <- function(model) {
  if (!all(c('params') %in% names(model))) {
    stop("model needs to contain the following components: 'params'.")
  }
  validate_input_params(model$params)
}

