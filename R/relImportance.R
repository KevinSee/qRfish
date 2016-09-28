#' @title Calculate relative importance of covariates
#'
#' @description Calculate node purity and relative importance
#'
#' @param qrf_mod object of class \code{quantregForest} returned by the function \code{quantregForest}
#'
#' @author Kevin See
#'
#' @import quantregForest randomForest dplyr
#' @export
#' @return NULL
#' @examples relImportance(qrf_mod)

relImportance = function(qrf_mod) {
  imp = importance(qrf_mod)
  data.frame(Metric = row.names(imp),
             imp) %>%
    mutate(relImp = IncNodePurity / max(IncNodePurity)) %>%
    arrange(desc(relImp))
}
