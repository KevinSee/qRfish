#' @title Plot relative importance of covariates
#'
#' @description plot the relative importance of each covariate in a quantile regression forest model
#'
#' @param mod object of class \code{quantregForest} returned by the function \code{quantregForest}
#' @param covar_dict \code{data.frame} containing columns called ShortName and Name. ShortName corresponds to the covariates that went into the \code{mod}, while Name is a longer version for plotting purposes.
#'
#' @author Kevin See
#'
#' @import ggplot2
#' @export
#' @return NULL
#' @examples relImpPlot(qrf_mod)

relImpPlot = function(mod,
                      covar_dict,
                      fill = 'gray40',
                      title = 'Relative Importance',
                      relImp_var = c('relImp', 'IncNodePurity')) {

  stopifnot(!is.null(mod), !is.null(covar_dict))

  relImp_var = match.arg(relImp_var)

  imp_df = relImportance(mod) %>%
    left_join(covar_dict %>%
                select(Metric = ShortName, Name)) %>%
    mutate(Metric = factor(Metric, levels = Metric[order(IncNodePurity)]),
           Name = factor(Name, levels = Name[order(IncNodePurity)]))
  names(imp_df)[match(relImp_var, names(imp_df))] = 'plot_y'

  if(sum(is.na(imp_df$Name)) > 0) {
    cat(paste('Missing covariate names for', paste(imp_df$Metric[is.na(imp_df$Name)], collapse = ', ')))
    stop()
  }

  imp_p = ggplot(data = imp_df,
                 aes(x = Name, y = plot_y)) +
    geom_bar(stat = 'identity', fill = fill) +
    coord_flip() +
    labs(title = 'Relative Importance') +
    theme_minimal() +
    theme(axis.title = element_blank())

  return(imp_p)
}
