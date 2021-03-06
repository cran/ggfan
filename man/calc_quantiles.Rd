% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_interval.R
\name{calc_quantiles}
\alias{calc_quantiles}
\title{Calculate quantiles of a tidy dataframe}
\usage{
calc_quantiles(data, intervals, x_var = "x", y_var = "y", rename = T)
}
\arguments{
\item{data}{A data frame with containing x and y columns, with several y
values for every x}

\item{intervals}{A list of intervals for which corresponding quantiles are
desired.}

\item{x_var}{A character string giving the name of the x variable}

\item{y_var}{A character string giving the name of the y variable}

\item{rename}{Logical. Indicates whether to retain existing variable name
or use \code{x} and \code{y}.}
}
\value{
A data frame containing x, y, and quantile columns (possibly renamed)
}
\description{
Calculate quantiles of a tidy dataframe
}
\examples{

head(fake_df)

fake_q <- calc_quantiles(fake_df, intervals=c(0,0.5,0.8))
head(fake_q)

}
