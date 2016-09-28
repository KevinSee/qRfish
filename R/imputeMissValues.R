#' @title Impute missing covariate values
#'
#' @description filter rows with too many missing values, and imputes the remaining missing values.
#'
#' @param data data.frame containing the response metric, the covariates and any other imputation metrics
#' @param covariates character vector of names of covariates to be used in QRF model
#' @param imputation_metrics character vector of names of additional covariates to be used in imputing missing values in the \code{covariates}.
#' @param max_miss maximum number of missing covariate values for any given data point. Data points with more than this number of missing values will be deleted.
#' @param response column name of response value
#'
#' @author Kevin See
#'
#' @import missForest magrittr dplyr
#' @export
#' @return data.frame with response vector in first column and covariates in subsequent columns.


imputeMissValues = function(data = NULL,
                            covariates = NULL,
                            imputation_metrics = NULL,
                            max_miss = 3,
                            response = NA) {
  if(is.null(data)) {
    cat('data must be supplied')
    return(NULL)
  }

  # how much missing data?
  data$n_miss = data %>%
    select(one_of(covariates)) %>%
    is.na() %>%
    rowSums()

  # remove rows with more missing values than max_miss
  data %<>%
    filter(n_miss <= max_miss) %>%
    select(-n_miss)

  # imputed missing values with missForest package
  imputed_values = data %>%
    select(one_of(covariates, imputation_metrics)) %>%
    as.data.frame() %>%
    missForest(variablewise = T, verbose = T)

  # put together the new data set with no missing values
  imputed_df = data %>%
    select(matches(response)) %>%
    bind_cols(imputed_values$ximp) %>%
    select(matches(response), one_of(covariates)) %>%
    tbl_df()

  return(imputed_df)
}

