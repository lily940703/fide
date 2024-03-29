% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/errors_intermittent.R
\name{calc_errors_smapis}
\alias{calc_errors_smapis}
\title{Calculate the sMAPIS error}
\usage{
calc_errors_smapis(dataset)
}
\arguments{
\item{dataset}{The dataset. A list including some time series.
In each time series, \code{x} is the historical value, \code{xx} is the true value,
and \code{ff} is the forecasting values based on the 12 methods.}
}
\value{
\code{dataset} with sMAPIS errors
}
\description{
Calculate the sMAPIS error
}
