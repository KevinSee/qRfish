#' @title Partial Dependence Plots
#'
#' @description plot the marginal effect of each covariate on the response at the selected quantiles
#'
#' @param mod object of class \code{quantregForest} returned by the function \code{quantregForest}
#' @param data \code{data.frame} that was used to fit the \code{mod}
#' @param covariates character vector of names of covariates to be used in QRF model
#' @param plot_covars which covariates should be included in final plot
#' @param quantiles which quantiles should be included on the partial dependence plots
#' @param n_pts how many points to make predictions for
#'
#' @author Kevin See
#'
#' @import tidyr magrittr plyr dplyr ggplot2
#' @export
#' @return NULL
#' @examples

pdpPlot = function(data = NULL,
                   mod = NULL,
                   covariates = NULL,
                   plot_covars = NULL,
                   quantiles = c(0.9),
                   n_pts = 200) {

  if(is.null(plot_covars)) plot_covars = covariates

  # get means and ranges of all covariates
  covar_range = select(data, one_of(covariates)) %>%
    gather(covar, value) %>%
    group_by(covar) %>%
    summarise(mean_value = mean(value, na.rm = T),
              median_value = median(value, na.rm = T),
              min_value = min(value, na.rm = T),
              max_value = max(value, na.rm = T))

  pdp_list = dlply(covar_range, .(covar), function(x) cbind(seq(x$min_value, x$max_value, length.out = n_pts),
                                                            matrix(covar_range$median_value, nrow = n_pts, ncol = nrow(covar_range), byrow = T, dimnames = list(NULL, covar_range$covar))))

  for(i in 1:length(pdp_list)) {
    pdp_list[[i]] = pdp_list[[i]][,-match(names(pdp_list)[i], colnames(pdp_list[[i]]))]
    colnames(pdp_list[[i]])[1] = names(pdp_list)[i]
  }

  pdp_df = ldply(pdp_list) %>% tbl_df

  quant_preds = predict(mod, newdata = select(pdp_df, one_of(hab_mets)), what = quantiles) %>%
    as.data.frame() %>%
    tbl_df()
  names(quant_preds) = paste0('quantile_', quantiles*100)

  pdp_df %<>%
    bind_cols(quant_preds) %>%
    gather(Metric, value, -covar, -matches('^quantile_')) %>%
    filter(covar == Metric) %>%
    select(covar, value, matches('^quantile_')) %>%
    filter(covar %in% plot_covars) %>%
    left_join(hab_dict %>%
                select(covar = ShortName, covar_label = Name)) %>%
    mutate(covar = factor(covar,
                          levels = relImportance(mod)$Metric),
           covar_label = factor(covar_label,
                                levels = match(hab_dict$Name))) %>%
    gather(quantile, pred, -covar_label, -covar, -value) %>%
    tbl_df()

  rug_df = select(data, WatershedName, one_of(hab_mets)) %>%
    gather(covar, value, -WatershedName) %>%
    filter(covar %in% plot_covars) %>%
    left_join(hab_dict %>%
                select(covar = ShortName, covar_label = Name)) %>%
    tbl_df()

  if(length(quantiles) == 1) {
    pdp_p = ggplot(data = pdp_df,
             aes(x = value,
                 y = pred)) +
      geom_smooth(method = 'loess',
                  se = F) +
      geom_rug(data = rug_df, aes(x = value, y=NULL, color = WatershedName)) +
      scale_color_brewer(palette = 'Set1') +
      facet_wrap(~ covar_label, scales = 'free') +
      theme_bw() +
      theme(legend.position = 'bottom') +
      labs(y = 'Prediction (fish / m)',
           x = 'Covariate Value',
           title = 'Partial Dependence Plots',
           color = 'Watershed')
  }

  if(length(quantiles) > 1) {
    pdp_p = pdp_df %>%
      mutate(quantile = gsub('quantile_', '', quantile),
             quantile = gsub('$', 'th', quantile)) %>%
      ggplot(aes(x = value,
                 y = pred)) +
      geom_smooth(method = 'loess',
                  se=F,
                  aes(color = quantile)) +
      geom_rug(data = rug_df,
               aes(x = value,
                   y = NULL),
               color = 'lightgray') +
      scale_color_brewer(palette = 'Set1') +
      facet_wrap(~ covar_label, scales = 'free') +
      theme_bw() +
      theme(legend.position = 'bottom') +
      labs(y = 'Prediction (fish / m)',
           x = 'Covariate Value',
           title = 'Partial Dependence Plots',
           color = 'Quantile')
  }




  return(pdp_p)


}
