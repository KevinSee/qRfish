#' @title QRF Variable Selection
#'
#' @description Calculates the maximal information criteria for all pairwise groups of the response metric and covariates.
#'
#' @param data \code{data.frame} that was used to fit the \code{mod}
#' @param covariates character vector of names of possible covariates
#' @param response column name of response value
#' @param covar_dict \code{data.frame} containing columns called ShortName, Name, and MetricCategory. ShortName corresponds to the covariates that went into the \code{mod}, while Name is a longer version for plotting purposes. MetricCategory is a grouping factor
#' @param save_csv_file should the results be written to a .csv file?
#'
#' @author Kevin See
#'
#' @import tidyr dplyr minerva readr
#' @importFrom plyr ldply
#' @export
#' @return NULL
#' @examples

runMINE = function(data = NULL,
                   covariates = NULL,
                   response = NULL,
                   covar_dict = NULL,
                   save_csv_file = F) {

  mine_res_list = vector('list', length(covariates))
  names(mine_res_list) = covariates
  for(i in 1:length(covariates)) {
    mine_res_list[[i]] = mine(x = data[,match(covariates[i], names(data))],
                                  y = data[,match(response, names(data))],
                                  use = 'pairwise.complete.obs') %>%
      unlist()
  }

  mine_res = ldply(mine_res_list, .id = 'variable') %>% tbl_df %>%
    left_join(data %>%
                select(one_of(covariates)) %>%
                gather(variable, value) %>%
                group_by(variable) %>%
                summarise(non_NA = sum(!is.na(value)),
                          is_NA = sum(is.na(value)),
                          perc_NA = round(is_NA / (is_NA + non_NA), 3),
                          non_0 = sum(value != 0, na.rm = T)) %>%
                left_join(select(covar_dict, variable = ShortName, MetricCategory))) %>%
    select(MetricCategory, variable, non_NA:non_0, everything()) %>%
    arrange(MetricCategory, desc(MIC))

  if(save_csv_file) {
    write_csv(mine_res,
              paste0('MINE_', response, '.csv'))
  }

  return(mine_res)

}
