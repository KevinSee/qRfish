% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relImpPlot.R
\name{relImpPlot}
\alias{relImpPlot}
\title{Plot relative importance of covariates}
\usage{
relImpPlot(qrf_mod, fill = "gray40", title = "Relative Importance",
  relImp_var = c("relImp", "IncNodePurity"))
}
\arguments{
\item{qrf_mod}{object of class \code{quantregForest} returned by the function \code{quantregForest}}
}
\description{
plot the relative importance of each covariate in a quantile regression forest model
}
\examples{
relImpPlot(qrf_mod)
}
\author{
Kevin See
}
