#' @title Plot relative importance of covariates
#'
#' @description plot the relative importance of each covariate in a quantile regression forest model
#'
#' @param qrf_mod object of class \code{quantregForest} returned by the function \code{quantregForest}
#'
#' @author Kevin See
#'
#' @import ggplot2
#' @export
#' @return NULL
#' @examples relImpPlot(qrf_mod)

relImpPlot = function(qrf_mod,
                      fill = 'gray40',
                      title = 'Relative Importance',
                      relImp_var = c( 'relImp', 'IncNodePurity')) {

  relImp_var = match.arg(relImp_var)

  imp_df = relImportance(qrf_mod) %>%
    mutate(Metric = factor(Metric, levels = Metric[order(IncNodePurity)]))
  names(imp_df)[match(relImp_var, names(imp_df))] = 'plot_y'

  imp_p = ggplot(data = imp_df,
                 aes(x = Metric, y = plot_y)) +
    geom_bar(stat = 'identity', fill = fill) +
    coord_flip() +
    labs(title = 'Relative Importance') +
    theme_minimal() +
    theme(axis.title = element_blank())

  return(imp_p)
}
